// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {MentionFilters} from '@app/features/notification/state/MentionFeed';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import {loadThreadsForMessages} from '@app/features/threads/commands/ThreadCommands';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {compare as compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeUtils';

const logger = new Logger('Mentions');

interface MentionFetchOptions {
	before?: string;
}

function mentionQuery(
	filters: MentionFilters,
	options: MentionFetchOptions = {},
): Record<string, string | number | boolean | null | undefined> {
	return {
		everyone: filters.includeEveryone,
		roles: filters.includeRoles,
		guilds: filters.includeGuilds,
		limit: MAX_MESSAGES_PER_CHANNEL,
		...options,
	};
}

async function requestMentions(options: MentionFetchOptions = {}): Promise<Array<Message>> {
	const response = await http.get<Array<Message>>(Endpoints.USER_MENTIONS, {
		query: mentionQuery(MentionFeed.getFilters(), options),
	});
	return response.body ?? [];
}

async function runMentionFetch(label: string, options: MentionFetchOptions = {}): Promise<Array<Message>> {
	const requestId = MentionFeed.handleFetchPending();
	try {
		logger.debug(label);
		const data = await requestMentions(options);
		if (ThreadGuilds.anyActive) {
			await loadThreadsForMessages(data);
		}
		MentionFeed.handleRecentMentionsFetchSuccess(requestId, data);
		logger.debug(`Successfully loaded ${data.length} mentions`);
		return data;
	} catch (error) {
		MentionFeed.handleRecentMentionsFetchError(requestId);
		logger.error(`${label} failed:`, error);
		throw error;
	}
}

export async function fetch(): Promise<Array<Message>> {
	return runMentionFetch('Fetching recent mentions');
}

export async function loadMore(): Promise<Array<Message>> {
	const recentMentions = MentionFeed.recentMentions;
	if (recentMentions.length === 0) {
		return [];
	}
	const lastMessage = recentMentions[recentMentions.length - 1];
	return runMentionFetch(`Loading more mentions before ${lastMessage.id}`, {before: lastMessage.id});
}

export function updateFilters(filters: Partial<MentionFilters>): void {
	MentionFeed.updateFilters(filters);
}

async function deleteMention(messageId: string): Promise<void> {
	await http.delete(Endpoints.USER_MENTION(messageId));
}

async function markMentionsRead(messageIds: Array<string>): Promise<void> {
	await http.post(Endpoints.USER_MENTIONS_READ, {
		body: {message_ids: messageIds},
	});
}

function buildMentionAckEntries(
	mentions: ReadonlyArray<{
		channelId: string;
		id: string;
	}>,
): Array<ReadStateCommands.BulkAckEntry> {
	const latestByChannel = new Map<string, string>();
	for (const mention of mentions) {
		const current = latestByChannel.get(mention.channelId);
		if (!current || compareSnowflakes(mention.id, current) > 0) {
			latestByChannel.set(mention.channelId, mention.id);
		}
	}
	return Array.from(latestByChannel, ([channelId, messageId]) => ({channelId, messageId}));
}

export async function remove(messageId: string): Promise<void> {
	try {
		MentionFeed.handleMessageDelete(messageId);
		logger.debug(`Removing message ${messageId} from recent mentions`);
		await deleteMention(messageId);
		logger.debug(`Successfully removed message ${messageId} from recent mentions`);
	} catch (error) {
		logger.error(`Failed to remove message ${messageId} from recent mentions:`, error);
		throw error;
	}
}

export async function markLoadedAsRead(): Promise<void> {
	const mentions = [...MentionFeed.getAccessibleMentions()];
	if (mentions.length === 0) return;
	const messageIds = mentions.map((mention) => mention.id);
	const ackEntries = buildMentionAckEntries(mentions);
	try {
		MentionFeed.handleMessagesDelete(messageIds);
		logger.debug(`Marking ${messageIds.length} recent mentions as read`);
		await Promise.all([ReadStateCommands.bulkAckEntries(ackEntries), markMentionsRead(messageIds)]);
		logger.debug(`Successfully marked ${messageIds.length} recent mentions as read`);
	} catch (error) {
		logger.error('Failed to mark recent mentions as read:', error);
		throw error;
	}
}
