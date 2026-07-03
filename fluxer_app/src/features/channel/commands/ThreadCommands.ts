// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ThreadResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

const logger = new Logger('Threads');

export interface CreateThreadParams {
	name: string;
	expires_in_ms?: number;
	source_message_id?: string;
}

export async function create(channelId: string, params: CreateThreadParams): Promise<ThreadResponse> {
	const response = await http.post<ThreadResponse>(
		`/channels/${channelId}/threads`,
		{body: params},
	);
	return response.body;
}

export async function update(
	channelId: string,
	threadId: string,
	params: {name?: string; state?: number; archived?: boolean; locked?: boolean; auto_archive_duration?: number; expires_in_ms?: number; rate_limit_per_user?: number},
): Promise<ThreadResponse> {
	try {
		logger.info(`Updating thread ${threadId} in channel ${channelId} with params: ${JSON.stringify(params)}`);
		const response = await http.patch<ThreadResponse>(
			`/channels/${channelId}/threads/${threadId}`,
			{body: params},
		);
		Threads.handleThreadUpdate(response.body);
		return response.body;
	} catch (error) {
		logger.error(`Failed to update thread ${threadId} (parent: ${channelId}):`, error);
		throw error;
	}
}

export async function remove(channelId: string, threadId: string): Promise<void> {
	await http.delete(`/channels/${channelId}/threads/${threadId}`);
}

export async function fetchList(channelId: string): Promise<ThreadResponse[]> {
	try {
		const response = await http.get<ThreadResponse[]>(`/channels/${channelId}/threads`);
		const threads = response.body ?? [];
		for (const thread of threads) {
			Threads.handleThreadCreate(thread);
		}
		return threads;
	} catch (error) {
		logger.error(`Failed to fetch threads for channel ${channelId}:`, error);
		throw error;
	}
}

export async function fetchJoined(): Promise<void> {
	try {
		const response = await http.get<ThreadResponse[]>('/users/@me/thread-members');
		const threads = response.body ?? [];
		const joinedIds: string[] = [];
		for (const thread of threads) {
			Threads.handleThreadCreate(thread);
			joinedIds.push(thread.id);
		}
		Threads.handleThreadListSync({threads, joinedThreadIds: joinedIds});
	} catch (error) {
		logger.error('Failed to fetch joined threads:', error);
	}
}

export async function join(channelId: string, threadId: string): Promise<void> {
	await http.post(`/channels/${channelId}/threads/${threadId}/members/@me`, {body: {}});
	Threads.handleThreadMemberAdd({threadId});
}

export async function leave(channelId: string, threadId: string): Promise<void> {
	await http.delete(`/channels/${channelId}/threads/${threadId}/members/@me`);
	Threads.handleThreadMemberRemove({threadId});
}
