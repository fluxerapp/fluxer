// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {MessageSearchFilters, SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {type ChannelID, createChannelID, createMessageID, type MessageID} from '../BrandedTypes';
import type {IMessageRepository} from '../channel/repositories/IMessageRepository';
import {Logger} from '../Logger';
import type {Message} from '../models/Message';
import {mapWithConcurrency} from '../utils/ConcurrencyUtils';
import type {IMessageSearchService} from './IMessageSearchService';
import {deleteMessageSearchDocuments} from './MessageSearchIndexCleanup';

const RECONCILE_BATCH_SIZE = 250;
const MAX_RECONCILE_PAGES = 40;
const MAX_STALE_DELETE_ABSOLUTE = 250;
const MAX_STALE_DELETE_RATIO = 0.5;
const HIT_LOOKUP_CONCURRENCY = 32;

interface MessageLookupRepository {
	readonly messages: Pick<IMessageRepository, 'getMessage'>;
}

interface MessageSearchLookupResult extends SearchResult<SearchableMessage> {
	messages: Array<Message>;
}

interface SearchExistingMessagesParams {
	searchService: IMessageSearchService;
	messageRepository: MessageLookupRepository;
	query: string;
	filters: MessageSearchFilters;
	hitsPerPage: number;
	page: number;
	cursor?: Array<string>;
}

interface ValidatedHit {
	hit: SearchableMessage;
	message: Message | null;
}

interface ValidatedHits {
	validHits: Array<ValidatedHit>;
	staleMessageIds: Array<MessageID>;
	lookupErrorCount: number;
}

function resolvedMessages(validHits: Array<ValidatedHit>): Array<Message> {
	return validHits.map((entry) => entry.message).filter((message): message is Message => message !== null);
}

export async function searchExistingMessages({
	searchService,
	messageRepository,
	query,
	filters,
	hitsPerPage,
	page,
	cursor,
}: SearchExistingMessagesParams): Promise<MessageSearchLookupResult> {
	const result = await searchService.searchMessages(query, filters, {
		hitsPerPage,
		page: cursor?.length ? undefined : page,
		cursor,
	});
	const validated = await validateSearchHits(messageRepository, result.hits);
	if (validated.staleMessageIds.length === 0) {
		return {...result, messages: resolvedMessages(validated.validHits)};
	}
	if (cursor?.length) {
		if (validated.lookupErrorCount === 0) {
			await deleteStaleSearchDocuments(searchService, validated.staleMessageIds, result.hits.length);
		}
		return {
			...result,
			hits: validated.validHits.map((entry) => entry.hit),
			total: Math.max(validated.validHits.length, result.total - validated.staleMessageIds.length),
			messages: resolvedMessages(validated.validHits),
		};
	}
	return reconcileOffsetSearchResult({
		searchService,
		messageRepository,
		query,
		filters,
		hitsPerPage,
		page,
	});
}

async function reconcileOffsetSearchResult({
	searchService,
	messageRepository,
	query,
	filters,
	hitsPerPage,
	page,
}: Omit<SearchExistingMessagesParams, 'cursor'>): Promise<MessageSearchLookupResult> {
	const requestedOffset = (page - 1) * hitsPerPage;
	const pageHits: Array<ValidatedHit> = [];
	const staleMessageIds: Array<MessageID> = [];
	let lookupErrorCount = 0;
	let examinedCount = 0;
	let validTotal = 0;
	let rawOffset = 0;
	let rawPage = 1;
	let corpusTotal = 0;
	while (rawPage <= MAX_RECONCILE_PAGES) {
		const result = await searchService.searchMessages(query, filters, {
			hitsPerPage: RECONCILE_BATCH_SIZE,
			page: rawPage,
		});
		corpusTotal = result.total;
		if (result.hits.length === 0) {
			break;
		}
		const validated = await validateSearchHits(messageRepository, result.hits);
		lookupErrorCount += validated.lookupErrorCount;
		examinedCount += result.hits.length;
		staleMessageIds.push(...validated.staleMessageIds);
		for (const entry of validated.validHits) {
			if (validTotal >= requestedOffset && pageHits.length < hitsPerPage) {
				pageHits.push(entry);
			}
			validTotal += 1;
		}
		rawOffset += result.hits.length;
		if (pageHits.length >= hitsPerPage && rawOffset >= requestedOffset) {
			break;
		}
		if (rawOffset >= result.total) {
			break;
		}
		rawPage += 1;
	}
	if (lookupErrorCount === 0) {
		await deleteStaleSearchDocuments(searchService, staleMessageIds, examinedCount);
	}
	return {
		hits: pageHits.map((entry) => entry.hit),
		total: Math.max(pageHits.length, corpusTotal - staleMessageIds.length),
		messages: resolvedMessages(pageHits),
	};
}

async function validateSearchHits(
	messageRepository: MessageLookupRepository,
	hits: Array<SearchableMessage>,
): Promise<ValidatedHits> {
	const checked = await mapWithConcurrency(hits, HIT_LOOKUP_CONCURRENCY, async (hit) => {
		let channelId: ChannelID;
		let messageId: MessageID;
		try {
			channelId = createChannelID(BigInt(hit.channelId));
			messageId = createMessageID(BigInt(hit.id));
		} catch (_invalidId) {
			return {entry: null, staleMessageId: null, lookupError: false};
		}
		let message: Message | null;
		try {
			message = await messageRepository.messages.getMessage(channelId, messageId);
		} catch (error) {
			Logger.warn(
				{error, messageId: hit.id, channelId: hit.channelId},
				'Search read repair lookup failed; keeping document',
			);
			return {entry: {hit, message: null}, staleMessageId: null, lookupError: true};
		}
		if (message && message.channelId.toString() === hit.channelId) {
			return {entry: {hit, message}, staleMessageId: null, lookupError: false};
		}
		return {entry: null, staleMessageId: messageId, lookupError: false};
	});
	const validHits: Array<ValidatedHit> = [];
	const staleMessageIds: Array<MessageID> = [];
	let lookupErrorCount = 0;
	for (const item of checked) {
		if (item.entry) {
			validHits.push(item.entry);
		}
		if (item.staleMessageId) {
			staleMessageIds.push(item.staleMessageId);
		}
		if (item.lookupError) {
			lookupErrorCount += 1;
		}
	}
	return {validHits, staleMessageIds, lookupErrorCount};
}

async function deleteStaleSearchDocuments(
	searchService: IMessageSearchService,
	messageIds: Array<MessageID>,
	examinedCount: number,
): Promise<void> {
	if (messageIds.length === 0) {
		return;
	}
	if (
		messageIds.length > MAX_STALE_DELETE_ABSOLUTE ||
		(examinedCount > 0 && messageIds.length / examinedCount > MAX_STALE_DELETE_RATIO)
	) {
		Logger.warn(
			{staleMessageCount: messageIds.length, examinedCount},
			'Search read repair delete exceeded safety cap; skipping delete',
		);
		return;
	}
	await deleteMessageSearchDocuments(messageIds, {
		searchService,
		context: {source: 'message_search_read_repair', staleMessageCount: messageIds.length},
	});
}
