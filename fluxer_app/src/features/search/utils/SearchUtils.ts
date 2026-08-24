// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXAMPLE_DOMAIN} from '@app/features/app/config/I18nDisplayConstants';
import {Channel} from '@app/features/channel/models/Channel';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {http} from '@app/features/platform/transport/RestTransport';
import SearchHistory from '@app/features/search/state/SearchHistory';
import {addUniqueSearchParam as addUnique, parseQuery} from '@app/features/search/utils/SearchQueryParser';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const NO_RESPONSE_BODY_RECEIVED_DESCRIPTOR = msg({
	message: 'No response body received',
	comment:
		'Error thrown when a message search returns an empty HTTP body. Reaches the user as a toast or inline error.',
});
const STILL_INDEXING_DESCRIPTOR = msg({
	message: 'Still indexing — check back in a moment',
	comment:
		'Status message returned by search when results are unavailable because the server is still building the index.',
});
const VALUE_LINK_LABEL_DESCRIPTOR = msg({
	message: 'link',
	comment: 'has:link chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_LINK_DESCRIPTION_DESCRIPTOR = msg({
	message: 'a URL typed in the message text',
	comment: 'Description for has:link. Sentence case, no trailing punctuation.',
});
const VALUE_EMBED_LABEL_DESCRIPTOR = msg({
	message: 'embed',
	comment: 'has:embed chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_EMBED_DESCRIPTION_DESCRIPTOR = msg({
	message: 'a rich embed or link preview (not an uploaded file)',
	comment: 'Description for has:embed. Sentence case, no trailing punctuation.',
});
const VALUE_FILE_LABEL_DESCRIPTOR = msg({
	message: 'file',
	comment: 'has:file chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_FILE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'any uploaded attachment',
	comment: 'Description for has:file. Sentence case, no trailing punctuation.',
});
const VALUE_IMAGE_LABEL_DESCRIPTOR = msg({
	message: 'image',
	comment: 'has:image chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_IMAGE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'an uploaded image file',
	comment: 'Description for has:image. Sentence case, no trailing punctuation.',
});
const VALUE_VIDEO_LABEL_DESCRIPTOR = msg({
	message: 'video',
	comment: 'has:video chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_VIDEO_DESCRIPTION_DESCRIPTOR = msg({
	message: 'an uploaded video file',
	comment: 'Description for has:video. Sentence case, no trailing punctuation.',
});
const VALUE_SOUND_LABEL_DESCRIPTOR = msg({
	message: 'sound',
	comment: 'has:sound chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_SOUND_DESCRIPTION_DESCRIPTOR = msg({
	message: 'an uploaded audio file',
	comment: 'Description for has:sound. Sentence case, no trailing punctuation.',
});
const VALUE_STICKER_LABEL_DESCRIPTOR = msg({
	message: 'sticker',
	comment: 'has:sticker chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_STICKER_DESCRIPTION_DESCRIPTOR = msg({
	message: 'a sticker',
	comment: 'Description for has:sticker. Sentence case, no trailing punctuation.',
});
const VALUE_POLL_LABEL_DESCRIPTOR = msg({
	message: 'poll',
	comment: 'has:poll chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_POLL_DESCRIPTION_DESCRIPTOR = msg({
	message: 'a poll',
	comment: 'Description for has:poll. Sentence case, no trailing punctuation.',
});
const VALUE_FORWARD_LABEL_DESCRIPTOR = msg({
	message: 'forward',
	comment: 'has:forward chip label. Always lowercase; matches the typed query value 1:1.',
});
const VALUE_FORWARD_DESCRIPTION_DESCRIPTOR = msg({
	message: 'a forwarded message',
	comment: 'Description for has:forward. Sentence case, no trailing punctuation.',
});
const HINT_USER_DESCRIPTOR = msg({
	message: 'a user',
	comment: 'Hint shown beside from: and mentions:. Sentence case, no trailing punctuation.',
});
const HINT_EXCLUDE_USER_DESCRIPTOR = msg({
	message: 'exclude a user',
	comment: 'Hint shown beside -from: and -mentions: (negation). Sentence case, no trailing punctuation.',
});
const HINT_HAS_VALUES_DESCRIPTOR = msg({
	message: 'link, embed, image, video, sound, file, sticker, poll, or forward',
	comment: 'Hint shown beside has:. Lists all supported content categories in lowercase.',
});
const HINT_EXCLUDE_HAS_VALUES_DESCRIPTOR = msg({
	message: 'exclude link, embed, image, video, sound, file, sticker, poll, or forward',
	comment: 'Hint shown beside -has: (negation). Lists all supported content categories in lowercase.',
});
const HINT_DATE_DESCRIPTOR = msg({
	message: 'a date or date range',
	comment: 'Hint shown beside before:, on:, during:, and after:. Sentence case, no trailing punctuation.',
});
const HINT_CHANNEL_DESCRIPTOR = msg({
	message: 'a channel',
	comment: 'Hint shown beside in:. Sentence case, no trailing punctuation.',
});
const HINT_EXCLUDE_CHANNEL_DESCRIPTOR = msg({
	message: 'exclude a channel',
	comment: 'Hint shown beside -in: (negation). Sentence case, no trailing punctuation.',
});
const HINT_TRUE_OR_FALSE_DESCRIPTOR = msg({
	message: 'true or false',
	comment: 'Hint shown beside pinned:. Lowercase boolean values.',
});
const HINT_AUTHOR_TYPE_DESCRIPTOR = msg({
	message: 'user, bot, or webhook',
	comment: 'Hint shown beside author-type:. Lowercase categories.',
});
const HINT_LINK_HOSTNAME_DESCRIPTOR = msg({
	message: 'a hostname, e.g. {exampleDomain}',
	comment: 'Hint shown beside link: with an example domain. Sentence case, no trailing punctuation.',
});
const HINT_EXCLUDE_LINK_HOSTNAME_DESCRIPTOR = msg({
	message: 'exclude a hostname, e.g. {exampleDomain}',
	comment: 'Hint shown beside -link: (negation) with an example domain. Sentence case, no trailing punctuation.',
});
const HINT_FILENAME_DESCRIPTOR = msg({
	message: 'part of an attachment filename',
	comment: 'Hint shown beside filename:. Sentence case, no trailing punctuation.',
});
const HINT_EXCLUDE_FILENAME_DESCRIPTOR = msg({
	message: 'exclude part of an attachment filename',
	comment: 'Hint shown beside -filename: (negation). Sentence case, no trailing punctuation.',
});
const HINT_EXT_DESCRIPTOR = msg({
	message: 'a file extension, e.g. png',
	comment: 'Hint shown beside ext:. Sentence case, no trailing punctuation.',
});
const HINT_EXCLUDE_EXT_DESCRIPTOR = msg({
	message: 'exclude a file extension, e.g. png',
	comment: 'Hint shown beside -ext: (negation). Sentence case, no trailing punctuation.',
});
const HINT_SORT_DESCRIPTOR = msg({
	message: 'timestamp or relevance',
	comment: 'Hint shown beside sort:. Lowercase values.',
});
const HINT_ORDER_DESCRIPTOR = msg({
	message: 'asc or desc',
	comment: 'Hint shown beside order:. Lowercase values.',
});

export interface SearchOption {
	value: string;
	label: string;
	description?: string;
	isDefault?: boolean;
}

export interface SearchValueOption {
	value: string;
	label: string;
	description?: string;
	isDefault?: boolean;
}

export type MessageSearchScope = 'current' | 'all_dms' | 'open_dms' | 'all' | 'open_dms_and_all_guilds' | 'all_guilds';

export interface MessageSearchParams {
	hitsPerPage?: number;
	page?: number;
	maxId?: string;
	minId?: string;
	content?: string;
	contents?: Array<string>;
	exactPhrases?: Array<string>;
	channelId?: Array<string>;
	excludeChannelId?: Array<string>;
	authorType?: Array<'user' | 'bot' | 'webhook'>;
	excludeAuthorType?: Array<'user' | 'bot' | 'webhook'>;
	authorId?: Array<string>;
	excludeAuthorId?: Array<string>;
	mentions?: Array<string>;
	excludeMentions?: Array<string>;
	mentionEveryone?: boolean;
	pinned?: boolean;
	has?: Array<'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link' | 'poll' | 'snapshot'>;
	excludeHas?: Array<'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link' | 'poll' | 'snapshot'>;
	embedType?: Array<'image' | 'video' | 'sound' | 'article'>;
	excludeEmbedType?: Array<'image' | 'video' | 'sound' | 'article'>;
	embedProvider?: Array<string>;
	excludeEmbedProvider?: Array<string>;
	linkHostname?: Array<string>;
	excludeLinkHostname?: Array<string>;
	attachmentFilename?: Array<string>;
	excludeAttachmentFilename?: Array<string>;
	attachmentExtension?: Array<string>;
	excludeAttachmentExtension?: Array<string>;
	sortBy?: 'timestamp' | 'relevance';
	sortOrder?: 'asc' | 'desc';
	scope?: MessageSearchScope;
	includeNsfw?: boolean;
}

interface ApiMessageSearchResponse {
	messages: Array<WireMessage>;
	channels?: Array<WireChannel>;
	total: number;
	hits_per_page?: number;
	page?: number;
}

interface MessageSearchResponse {
	messages: Array<Message>;
	channels: Array<Channel>;
	total: number;
	hitsPerPage: number;
	page: number;
}

interface IndexingResponse {
	indexing: true;
	message: string;
}

type SearchResult = MessageSearchResponse | IndexingResponse;
type MessageSearchApiParams = Record<string, string | number | boolean | Array<string> | undefined>;

export interface SearchContext {
	contextChannelId?: string;
	contextGuildId?: string | null;
}

interface SearchQueryExecutionContext {
	historyKey?: string;
	guildId?: string | null;
}

export function isIndexing(result: SearchResult): result is IndexingResponse {
	return 'indexing' in result && result.indexing === true;
}

export async function searchMessages(
	i18n: I18n,
	context: SearchContext,
	params: MessageSearchParams,
): Promise<SearchResult> {
	const extraParams: MessageSearchApiParams = {};
	if (context.contextChannelId) {
		extraParams.context_channel_id = context.contextChannelId;
	}
	if (context.contextGuildId) {
		extraParams.context_guild_id = context.contextGuildId;
	}
	if (params.channelId && params.channelId.length > 0) {
		extraParams.channel_ids = params.channelId;
	}
	const response = await http.post<
		| ApiMessageSearchResponse
		| {
				indexing: true;
		  }
	>('/search/messages', {body: toApiParams(params, extraParams)});
	const body = response.body;
	if (!body) {
		throw new Error(i18n._(NO_RESPONSE_BODY_RECEIVED_DESCRIPTOR));
	}
	if ('indexing' in body && body.indexing === true) {
		return {indexing: true, message: i18n._(STILL_INDEXING_DESCRIPTOR)};
	}
	const searchResponse = body as ApiMessageSearchResponse;
	return {
		messages: searchResponse.messages?.map((msg) => new Message(msg)) ?? [],
		channels: searchResponse.channels?.map((channel) => new Channel(channel)) ?? [],
		total: searchResponse.total ?? 0,
		hitsPerPage: searchResponse.hits_per_page ?? 25,
		page: searchResponse.page ?? 1,
	};
}

export interface SearchFilterOption {
	key: string;
	label: string;
	description: string;
	syntax: string;
	values?: Array<SearchValueOption>;
	requiresValue?: boolean;
	requiresGuild?: boolean;
	dmEligible?: boolean;
}

export function getSearchFilterOptions(i18n: I18n): Array<SearchFilterOption> {
	const hasContentValues: Array<SearchValueOption> = [
		{
			value: 'image',
			label: i18n._(VALUE_IMAGE_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_IMAGE_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'video',
			label: i18n._(VALUE_VIDEO_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_VIDEO_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'link',
			label: i18n._(VALUE_LINK_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_LINK_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'file',
			label: i18n._(VALUE_FILE_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_FILE_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'embed',
			label: i18n._(VALUE_EMBED_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_EMBED_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'sound',
			label: i18n._(VALUE_SOUND_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_SOUND_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'poll',
			label: i18n._(VALUE_POLL_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_POLL_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'sticker',
			label: i18n._(VALUE_STICKER_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_STICKER_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'forward',
			label: i18n._(VALUE_FORWARD_LABEL_DESCRIPTOR),
			description: i18n._(VALUE_FORWARD_DESCRIPTION_DESCRIPTOR),
		},
	];
	return [
		{
			key: 'from',
			label: 'from:',
			description: i18n._(HINT_USER_DESCRIPTOR),
			syntax: 'from:',
			requiresValue: true,
		},
		{
			key: '-from',
			label: '-from:',
			description: i18n._(HINT_EXCLUDE_USER_DESCRIPTOR),
			syntax: '-from:',
			requiresValue: true,
		},
		{
			key: 'mentions',
			label: 'mentions:',
			description: i18n._(HINT_USER_DESCRIPTOR),
			syntax: 'mentions:',
			requiresValue: true,
		},
		{
			key: '-mentions',
			label: '-mentions:',
			description: i18n._(HINT_EXCLUDE_USER_DESCRIPTOR),
			syntax: '-mentions:',
			requiresValue: true,
		},
		{
			key: 'has',
			label: 'has:',
			description: i18n._(HINT_HAS_VALUES_DESCRIPTOR),
			syntax: 'has:',
			values: hasContentValues,
		},
		{
			key: '-has',
			label: '-has:',
			description: i18n._(HINT_EXCLUDE_HAS_VALUES_DESCRIPTOR),
			syntax: '-has:',
			values: hasContentValues,
		},
		{
			key: 'before',
			label: 'before:',
			description: i18n._(HINT_DATE_DESCRIPTOR),
			syntax: 'before:',
			requiresValue: true,
		},
		{
			key: 'on',
			label: 'on:',
			description: i18n._(HINT_DATE_DESCRIPTOR),
			syntax: 'on:',
			requiresValue: true,
		},
		{
			key: 'during',
			label: 'during:',
			description: i18n._(HINT_DATE_DESCRIPTOR),
			syntax: 'during:',
			requiresValue: true,
		},
		{
			key: 'after',
			label: 'after:',
			description: i18n._(HINT_DATE_DESCRIPTOR),
			syntax: 'after:',
			requiresValue: true,
		},
		{
			key: 'in',
			label: 'in:',
			description: i18n._(HINT_CHANNEL_DESCRIPTOR),
			syntax: 'in:',
			requiresValue: true,
			requiresGuild: true,
			dmEligible: true,
		},
		{
			key: '-in',
			label: '-in:',
			description: i18n._(HINT_EXCLUDE_CHANNEL_DESCRIPTOR),
			syntax: '-in:',
			requiresValue: true,
			requiresGuild: true,
			dmEligible: true,
		},
		{
			key: 'pinned',
			label: 'pinned:',
			description: i18n._(HINT_TRUE_OR_FALSE_DESCRIPTOR),
			syntax: 'pinned:',
			values: [
				{value: 'true', label: 'true'},
				{value: 'false', label: 'false'},
			],
		},
		{
			key: 'authorType',
			label: 'author-type:',
			description: i18n._(HINT_AUTHOR_TYPE_DESCRIPTOR),
			syntax: 'author-type:',
			values: [
				{value: 'user', label: 'user'},
				{value: 'bot', label: 'bot'},
				{value: 'webhook', label: 'webhook'},
			],
		},
		{
			key: 'link',
			label: 'link:',
			description: i18n._(HINT_LINK_HOSTNAME_DESCRIPTOR, {exampleDomain: EXAMPLE_DOMAIN}),
			syntax: 'link:',
			requiresValue: true,
		},
		{
			key: '-link',
			label: '-link:',
			description: i18n._(HINT_EXCLUDE_LINK_HOSTNAME_DESCRIPTOR, {exampleDomain: EXAMPLE_DOMAIN}),
			syntax: '-link:',
			requiresValue: true,
		},
		{
			key: 'filename',
			label: 'filename:',
			description: i18n._(HINT_FILENAME_DESCRIPTOR),
			syntax: 'filename:',
			requiresValue: true,
		},
		{
			key: '-filename',
			label: '-filename:',
			description: i18n._(HINT_EXCLUDE_FILENAME_DESCRIPTOR),
			syntax: '-filename:',
			requiresValue: true,
		},
		{
			key: 'ext',
			label: 'ext:',
			description: i18n._(HINT_EXT_DESCRIPTOR),
			syntax: 'ext:',
			requiresValue: true,
		},
		{
			key: '-ext',
			label: '-ext:',
			description: i18n._(HINT_EXCLUDE_EXT_DESCRIPTOR),
			syntax: '-ext:',
			requiresValue: true,
		},
		{
			key: 'sort',
			label: 'sort:',
			description: i18n._(HINT_SORT_DESCRIPTOR),
			syntax: 'sort:',
			values: [
				{value: 'timestamp', label: 'timestamp', isDefault: true},
				{value: 'relevance', label: 'relevance'},
			],
		},
		{
			key: 'order',
			label: 'order:',
			description: i18n._(HINT_ORDER_DESCRIPTOR),
			syntax: 'order:',
			values: [
				{value: 'desc', label: 'desc', isDefault: true},
				{value: 'asc', label: 'asc'},
			],
		},
	];
}

export function toApiParams(params: MessageSearchParams, extraParams?: MessageSearchApiParams): MessageSearchApiParams {
	const hitsPerPage = params.hitsPerPage ?? 25;
	const page = params.page ?? 1;
	const apiParams: MessageSearchApiParams = {
		hits_per_page: hitsPerPage,
		page,
		max_id: params.maxId,
		min_id: params.minId,
		content: params.content,
		contents: params.contents,
		exact_phrases: params.exactPhrases,
		channel_id: params.channelId,
		exclude_channel_id: params.excludeChannelId,
		author_type: params.authorType,
		exclude_author_type: params.excludeAuthorType,
		author_id: params.authorId,
		exclude_author_id: params.excludeAuthorId,
		mentions: params.mentions,
		exclude_mentions: params.excludeMentions,
		mention_everyone: params.mentionEveryone,
		pinned: params.pinned,
		has: params.has,
		exclude_has: params.excludeHas,
		embed_type: params.embedType,
		exclude_embed_type: params.excludeEmbedType,
		embed_provider: params.embedProvider,
		exclude_embed_provider: params.excludeEmbedProvider,
		link_hostname: params.linkHostname,
		exclude_link_hostname: params.excludeLinkHostname,
		attachment_filename: params.attachmentFilename,
		exclude_attachment_filename: params.excludeAttachmentFilename,
		attachment_extension: params.attachmentExtension,
		exclude_attachment_extension: params.excludeAttachmentExtension,
		sort_by: params.sortBy,
		sort_order: params.sortOrder,
		scope: params.scope,
		include_nsfw: params.includeNsfw,
		...extraParams,
	};
	Object.keys(apiParams).forEach((key) => {
		if (apiParams[key] === undefined) {
			delete apiParams[key];
		}
	});
	return apiParams;
}

function extractSegmentValue(segment: SearchSegment): string | null {
	const colonIndex = segment.displayText.indexOf(':');
	if (colonIndex === -1) {
		return null;
	}
	const rawValue = segment.displayText.slice(colonIndex + 1).trim();
	if (rawValue.startsWith('"') && rawValue.endsWith('"') && rawValue.length >= 2) {
		return rawValue.slice(1, -1);
	}
	return rawValue.length > 0 ? rawValue : null;
}

function buildHintsFromSegments(segments: Array<SearchSegment>) {
	const usersByTag: Record<string, string> = {};
	const channelsByName: Record<string, string> = {};
	for (const segment of segments) {
		const value = extractSegmentValue(segment);
		if (!value) {
			continue;
		}
		if (segment.type === 'user') {
			usersByTag[value] = segment.id;
			continue;
		}
		if (segment.type === 'channel') {
			channelsByName[value] = segment.id;
		}
	}
	return {
		usersByTag: Object.keys(usersByTag).length > 0 ? usersByTag : undefined,
		channelsByName: Object.keys(channelsByName).length > 0 ? channelsByName : undefined,
	};
}

function segmentMatchesQuery(query: string, segment: SearchSegment): boolean {
	return (
		segment.start >= 0 && segment.end <= query.length && query.slice(segment.start, segment.end) === segment.displayText
	);
}

function applySegmentsToParams(
	params: MessageSearchParams,
	query: string,
	segments: Array<SearchSegment>,
): MessageSearchParams {
	for (const segment of segments) {
		if (!segmentMatchesQuery(query, segment)) {
			continue;
		}
		const isExclude = segment.filterKey.startsWith('-');
		const filterKey = isExclude ? segment.filterKey.slice(1) : segment.filterKey;
		if (segment.type === 'user') {
			switch (filterKey) {
				case 'from':
					if (isExclude) {
						params.excludeAuthorId = addUnique(params.excludeAuthorId, segment.id);
					} else {
						params.authorId = addUnique(params.authorId, segment.id);
					}
					break;
				case 'mentions':
					if (isExclude) {
						params.excludeMentions = addUnique(params.excludeMentions, segment.id);
					} else {
						params.mentions = addUnique(params.mentions, segment.id);
					}
					break;
			}
			continue;
		}
		if (segment.type === 'channel' && filterKey === 'in') {
			if (isExclude) {
				params.excludeChannelId = addUnique(params.excludeChannelId, segment.id);
			} else {
				params.channelId = addUnique(params.channelId, segment.id);
			}
		}
	}
	return params;
}

export function hasSearchableParams(params: MessageSearchParams): boolean {
	for (const value of Object.values(params)) {
		if (value !== undefined) {
			return true;
		}
	}
	return false;
}

export function parseSearchQueryWithSegments(
	query: string,
	segments: Array<SearchSegment>,
	ctx?: SearchQueryExecutionContext,
): MessageSearchParams {
	const entry = SearchHistory.recent(ctx?.historyKey).find((historyEntry) => historyEntry.query === query);
	const segmentHints = buildHintsFromSegments(segments);
	const segmentUserValues = new Set<string>();
	for (const segment of segments) {
		if (segment.type !== 'user' || !segmentMatchesQuery(query, segment)) {
			continue;
		}
		const value = extractSegmentValue(segment);
		if (value) {
			segmentUserValues.add(value);
		}
	}
	const entryUsersByTag = {...(entry?.hints?.usersByTag ?? {})};
	for (const value of segmentUserValues) {
		delete entryUsersByTag[value];
	}
	const usersByTag = {
		...entryUsersByTag,
	};
	const channelsByName = {
		...(entry?.hints?.channelsByName ?? {}),
		...(segmentHints.channelsByName ?? {}),
	};
	const params = parseQuery(
		query,
		{
			usersByTag: Object.keys(usersByTag).length > 0 ? usersByTag : undefined,
			channelsByName: Object.keys(channelsByName).length > 0 ? channelsByName : undefined,
		},
		{guildId: ctx?.guildId ?? null},
	);
	return applySegmentsToParams(params, query, segments);
}
