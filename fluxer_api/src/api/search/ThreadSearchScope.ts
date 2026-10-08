// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {ThreadMembersByUserRow, ThreadsByParentRow} from '@app/api/database/types/ThreadTypes';
import {isGuildMemberTimedOut} from '@app/api/guild/GuildModel';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {ThreadMembersByUser, ThreadsByParent} from '@app/api/Tables';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {PUBLIC_THREAD_CHANNEL_TYPES, THREAD_SCOPE_MAX} from '@fluxer/constants/src/ThreadConstants';
import {isThreadModerator, withImplicitThreadBits} from '@fluxer/constants/src/ThreadPermissionUtils';

const SCOPE_PAGE_SIZE = 250;
const SCOPE_FETCH_CONCURRENCY = 16;

const FETCH_THREADS_FIRST = ThreadsByParent.select({
	where: ThreadsByParent.where.eq('parent_id'),
	orderBy: {col: 'thread_id', direction: 'DESC'},
	limit: SCOPE_PAGE_SIZE,
});
const FETCH_THREADS_BEFORE = ThreadsByParent.select({
	where: [ThreadsByParent.where.eq('parent_id'), ThreadsByParent.where.lt('thread_id', 'before')],
	orderBy: {col: 'thread_id', direction: 'DESC'},
	limit: SCOPE_PAGE_SIZE,
});
const FETCH_JOINED_THREADS = ThreadMembersByUser.select({
	columns: ['thread_id'],
	where: [ThreadMembersByUser.where.eq('user_id'), ThreadMembersByUser.where.eq('guild_id')],
});

async function joinedThreadIds(userId: UserID, guildId: GuildID): Promise<Set<string>> {
	return new Set(
		(
			await fetchMany<Pick<ThreadMembersByUserRow, 'thread_id'>>(
				FETCH_JOINED_THREADS.bind({user_id: userId, guild_id: guildId}),
			)
		).map((row) => row.thread_id.toString()),
	);
}

function threadModeratorCheck(
	gatewayService: IGatewayService,
	userId: UserID,
	guildId: GuildID,
): (parentId: ChannelID) => Promise<boolean> {
	const byParent = new Map<string, Promise<boolean>>();
	let timedOut: Promise<boolean> | undefined;
	const check = async (parentId: ChannelID): Promise<boolean> => {
		const permissions = withImplicitThreadBits(
			await gatewayService.getUserPermissions({guildId, userId, channelId: parentId}),
		);
		if (isThreadModerator(permissions, {isOwner: false, timedOut: true})) return true;
		if (!isThreadModerator(permissions, {isOwner: false, timedOut: false})) return false;
		timedOut ??= gatewayService
			.getGuildMember({guildId, userId})
			.then((result) => isGuildMemberTimedOut(result.memberData));
		return !(await timedOut);
	};
	return (parentId) => {
		const key = parentId.toString();
		let result = byParent.get(key);
		if (result === undefined) {
			result = check(parentId);
			byParent.set(key, result);
		}
		return result;
	};
}

export async function accessibleRequestedThreadIds(params: {
	gatewayService: IGatewayService;
	userId: UserID;
	guildId: GuildID;
	threads: ReadonlyArray<{id: ChannelID; parentId: ChannelID | null; type: number}>;
}): Promise<Set<string>> {
	const {gatewayService, userId, guildId} = params;
	const accessible = new Set<string>();
	const pending = params.threads.filter((thread) => {
		if (thread.parentId === null) return false;
		if (!PUBLIC_THREAD_CHANNEL_TYPES.has(thread.type)) return true;
		accessible.add(thread.id.toString());
		return false;
	});
	if (pending.length === 0) return accessible;
	const joined = await joinedThreadIds(userId, guildId);
	const moderates = threadModeratorCheck(gatewayService, userId, guildId);
	for (const thread of pending) {
		const threadId = thread.id.toString();
		if (!joined.has(threadId) && !(await moderates(thread.parentId!))) continue;
		accessible.add(threadId);
	}
	return accessible;
}

function fetchParentPage(parentId: ChannelID, before: ChannelID | null): Promise<Array<ThreadsByParentRow>> {
	return fetchMany<ThreadsByParentRow>(
		before === null
			? FETCH_THREADS_FIRST.bind({parent_id: parentId})
			: FETCH_THREADS_BEFORE.bind({parent_id: parentId, before}),
	);
}

export async function accessibleThreadIds(params: {
	gatewayService: IGatewayService;
	userId: UserID;
	groups: ReadonlyArray<{guildId: GuildID; parentIds: ReadonlyArray<ChannelID>}>;
}): Promise<Map<string, ChannelID>> {
	const {gatewayService, userId} = params;
	const scope = new Map<string, ChannelID>();
	const pending = params.groups.flatMap(({guildId, parentIds}) => {
		if (parentIds.length === 0) return [];
		let joinedIds: Promise<Set<string>> | undefined;
		const joined = () => (joinedIds ??= joinedThreadIds(userId, guildId));
		const moderates = threadModeratorCheck(gatewayService, userId, guildId);
		return parentIds.map((parentId) => ({parentId, joined, moderates}));
	});
	if (pending.length === 0) return scope;
	const cursors = await mapWithConcurrency(pending, SCOPE_FETCH_CONCURRENCY, async (entry) => ({
		...entry,
		rows: await fetchParentPage(entry.parentId, null),
		index: 0,
	}));
	const heap = cursors.filter((cursor) => cursor.rows.length > 0);
	const headId = (at: number) => heap[at]!.rows[heap[at]!.index]!.thread_id;
	const siftDown = (from: number) => {
		let at = from;
		for (;;) {
			let largest = at;
			for (const child of [2 * at + 1, 2 * at + 2]) {
				if (child < heap.length && headId(child) > headId(largest)) largest = child;
			}
			if (largest === at) return;
			[heap[at], heap[largest]] = [heap[largest]!, heap[at]!];
			at = largest;
		}
	};
	for (let at = Math.floor(heap.length / 2) - 1; at >= 0; at--) siftDown(at);
	while (scope.size < THREAD_SCOPE_MAX && heap.length > 0) {
		const head = heap[0]!;
		const row = head.rows[head.index++]!;
		if (head.index === head.rows.length && head.rows.length === SCOPE_PAGE_SIZE) {
			head.rows = await fetchParentPage(head.parentId, row.thread_id);
			head.index = 0;
		}
		if (head.index >= head.rows.length) {
			heap[0] = heap[heap.length - 1]!;
			heap.pop();
		}
		siftDown(0);
		const threadId = row.thread_id.toString();
		if (
			!PUBLIC_THREAD_CHANNEL_TYPES.has(row.type) &&
			!(await head.joined()).has(threadId) &&
			!(await head.moderates(head.parentId))
		) {
			continue;
		}
		scope.set(threadId, head.parentId);
	}
	return scope;
}
