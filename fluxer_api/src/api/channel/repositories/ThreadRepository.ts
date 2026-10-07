// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, channelIdToMessageId, type GuildID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import type {IMessageRepository} from '@app/api/channel/repositories/IMessageRepository';
import {
	type ArchivedThreadPage,
	type CreateThreadMember,
	type CreateThreadParams,
	IThreadRepository,
	type ThreadMemberAddResult,
	type ThreadMemberRemoveResult,
	type ThreadMemberSettingsPatch,
	type ThreadParentConfigPatch,
	type ThreadStatePatch,
	type ThreadStateTransition,
} from '@app/api/channel/repositories/IThreadRepository';
import {
	BatchBuilder,
	deleteOneOrMany,
	executeConditional,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp, type PreparedQuery} from '@app/api/database/CassandraTypes';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {
	ActiveThreadsByGuildRow,
	ArchivedThreadsByParentRow,
	GuildThreadStateRow,
	ThreadMemberRow,
	ThreadMembersByUserRow,
	ThreadParentConfigRow,
	ThreadStateRow,
	ThreadStatsRow,
	ThreadsByParentRow,
} from '@app/api/database/types/ThreadTypes';
import {insertGuildThreadMarker, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import {ThreadMember} from '@app/api/models/ThreadMember';
import {ThreadParentConfig} from '@app/api/models/ThreadParentConfig';
import {ThreadState} from '@app/api/models/ThreadState';
import {ThreadStats} from '@app/api/models/ThreadStats';
import {
	ActiveThreadsByGuild,
	ArchivedThreadsByParent,
	Channels,
	ForumPinnedThread,
	GuildThreadState,
	ThreadMembers,
	ThreadMembersByUser,
	ThreadOnlyChannelsByGuild,
	ThreadParentConfig as ThreadParentConfigTable,
	ThreadState as ThreadStateTable,
	ThreadStats as ThreadStatsTable,
	ThreadsByParent,
} from '@app/api/Tables';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	ChannelFlags,
	MAX_THREAD_MEMBERS,
	THREAD_MEMBER_IDS_PREVIEW_SIZE,
	THREAD_ONLY_CHANNEL_TYPES,
	THREAD_PARENT_CHANNEL_TYPES,
} from '@fluxer/constants/src/ThreadConstants';
import {MaxThreadMembersError} from '@fluxer/errors/src/domains/channel/MaxThreadMembersError';
import {ThreadAlreadyCreatedForMessageError} from '@fluxer/errors/src/domains/channel/ThreadAlreadyCreatedForMessageError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

const STATE_CAS_ATTEMPTS = 3;
const DANGLING_CREATE_REPAIR_MS = 30_000;
const ENUMERATION_PAGE_SIZE = 1000;
const MEMBER_PAGE_SIZE = 1000;

const FETCH_STATE = ThreadStateTable.select({where: ThreadStateTable.where.eq('thread_id'), limit: 1});
const FETCH_STATES = ThreadStateTable.select({where: ThreadStateTable.where.in('thread_id', 'thread_ids')});
const FETCH_STATS = ThreadStatsTable.select({where: ThreadStatsTable.where.eq('thread_id'), limit: 1});
const FETCH_STATS_MANY = ThreadStatsTable.select({where: ThreadStatsTable.where.in('thread_id', 'thread_ids')});
const FETCH_CHANNEL = Channels.select({
	where: [Channels.where.eq('channel_id'), Channels.where.eq('soft_deleted')],
	limit: 1,
});
const FETCH_CHANNELS = Channels.select({
	columns: ['channel_id', 'last_message_id'],
	where: [Channels.where.in('channel_id', 'channel_ids'), Channels.where.eq('soft_deleted')],
});
const FETCH_FORUM_PIN = ForumPinnedThread.select({where: ForumPinnedThread.where.eq('parent_id'), limit: 1});
const FETCH_ACTIVE = ActiveThreadsByGuild.select({where: ActiveThreadsByGuild.where.eq('guild_id')});
const COUNT_ACTIVE = ActiveThreadsByGuild.selectCount({where: ActiveThreadsByGuild.where.eq('guild_id')});
const FETCH_THREADS_BY_PARENT_FIRST = ThreadsByParent.select({
	where: ThreadsByParent.where.eq('parent_id'),
	limit: ENUMERATION_PAGE_SIZE,
});
const FETCH_THREADS_BY_PARENT_AFTER = ThreadsByParent.select({
	where: [ThreadsByParent.where.eq('parent_id'), ThreadsByParent.where.gt('thread_id', 'after')],
	limit: ENUMERATION_PAGE_SIZE,
});
const FETCH_MEMBER = ThreadMembers.select({
	where: [ThreadMembers.where.eq('thread_id'), ThreadMembers.where.eq('user_id')],
	limit: 1,
});
const FETCH_MEMBERS_BY_IDS = ThreadMembers.select({
	where: [ThreadMembers.where.eq('thread_id'), ThreadMembers.where.in('user_id', 'user_ids')],
});
const FETCH_JOINED_BY_GUILD = ThreadMembersByUser.select({
	where: [ThreadMembersByUser.where.eq('user_id'), ThreadMembersByUser.where.eq('guild_id')],
});
const FETCH_PARENT_CONFIG = ThreadParentConfigTable.select({
	where: [ThreadParentConfigTable.where.eq('guild_id'), ThreadParentConfigTable.where.eq('channel_id')],
	limit: 1,
});
const FETCH_GUILD_MARKER = GuildThreadState.select({where: GuildThreadState.where.eq('guild_id'), limit: 1});
const FETCH_PARENT_CONFIGS = ThreadParentConfigTable.select({where: ThreadParentConfigTable.where.eq('guild_id')});

function archivedQuery(limit: number, bounded: boolean) {
	return ArchivedThreadsByParent.select({
		where: bounded
			? [
					ArchivedThreadsByParent.where.eq('parent_id'),
					ArchivedThreadsByParent.where.eq('is_private'),
					ArchivedThreadsByParent.where.lte('archive_timestamp', 'before'),
				]
			: [ArchivedThreadsByParent.where.eq('parent_id'), ArchivedThreadsByParent.where.eq('is_private')],
		orderBy: {col: 'archive_timestamp', direction: 'DESC'},
		limit,
	});
}

function joinedPrivateQuery(limit: number, bounded: boolean) {
	const base = [
		ThreadMembersByUser.where.eq('user_id'),
		ThreadMembersByUser.where.eq('guild_id'),
		ThreadMembersByUser.where.eq('parent_id'),
		ThreadMembersByUser.where.eq('is_private'),
	];
	return ThreadMembersByUser.select({
		where: bounded ? [...base, ThreadMembersByUser.where.lt('thread_id', 'before')] : base,
		orderBy: {col: 'thread_id', direction: 'DESC'},
		limit,
	});
}

function membersQuery(limit: number, bounded: boolean) {
	return ThreadMembers.select({
		where: bounded
			? [ThreadMembers.where.eq('thread_id'), ThreadMembers.where.gt('user_id', 'after')]
			: ThreadMembers.where.eq('thread_id'),
		limit,
	});
}

function isPrivateType(type: number): boolean {
	return type === ChannelTypes.PRIVATE_THREAD;
}

function sameTime(left: Date | null, right: Date | null): boolean {
	if (left === null || right === null) return left === right;
	return left.getTime() === right.getTime();
}

function sameValue(left: unknown, right: unknown): boolean {
	if (left instanceof Date && right instanceof Date) return left.getTime() === right.getTime();
	if (Array.isArray(left) || Array.isArray(right)) {
		const a = (left as Array<unknown> | null) ?? [];
		const b = (right as Array<unknown> | null) ?? [];
		return a.length === b.length && a.every((value, index) => value === b[index]);
	}
	return left === right;
}

function previewWith(preview: Array<UserID>, added: Array<UserID>): Array<UserID> {
	const fresh = [...added].reverse();
	const kept = preview.filter((id) => !added.includes(id));
	return [...fresh, ...kept].slice(0, THREAD_MEMBER_IDS_PREVIEW_SIZE);
}

function listOrNull<T>(values: Array<T>): Array<T> | null {
	return values.length > 0 ? values : null;
}

type StatePatchOps = Partial<{[K in Exclude<keyof ThreadStateRow, 'thread_id'>]: DbOp<ThreadStateRow[K]>}>;

function toStateOps(patch: ThreadStatePatch): StatePatchOps {
	const ops: StatePatchOps = {};
	for (const [key, value] of Object.entries(patch) as Array<[keyof ThreadStatePatch, unknown]>) {
		if (value === undefined) continue;
		if (key === 'applied_tags') {
			const tags = value as Array<bigint> | null;
			ops.applied_tags = tags && tags.length > 0 ? Db.set(tags) : Db.clear();
			continue;
		}
		(ops as Record<string, DbOp<unknown>>)[key] = value === null ? Db.clear() : Db.set(value);
	}
	return ops;
}

export class ThreadRepository extends IThreadRepository {
	constructor(
		private readonly channelData: ChannelDataRepository,
		private readonly messages: IMessageRepository,
		private readonly onIndexDrift?: (threadIds: Array<ChannelID>) => void,
	) {
		super();
	}

	async getState(threadId: ChannelID): Promise<ThreadState | null> {
		const row = await fetchOne<ThreadStateRow>(FETCH_STATE.bind({thread_id: threadId}));
		return row ? new ThreadState(row) : null;
	}

	async getStates(threadIds: Array<ChannelID>): Promise<Array<ThreadState>> {
		const rows = await fetchManyInChunks<ThreadStateRow>(FETCH_STATES, threadIds, (chunk) => ({thread_ids: chunk}));
		const byId = new Map(rows.map((row) => [row.thread_id, new ThreadState(row)]));
		return threadIds.flatMap((id) => {
			const state = byId.get(id);
			return state ? [state] : [];
		});
	}

	async getStats(threadId: ChannelID): Promise<ThreadStats> {
		const row = await fetchOne<ThreadStatsRow>(FETCH_STATS.bind({thread_id: threadId}));
		return row ? new ThreadStats(row) : ThreadStats.empty(threadId);
	}

	async getStatsMany(threadIds: Array<ChannelID>): Promise<Map<ChannelID, ThreadStats>> {
		const rows = await fetchManyInChunks<ThreadStatsRow>(FETCH_STATS_MANY, threadIds, (chunk) => ({
			thread_ids: chunk,
		}));
		const result = new Map<ChannelID, ThreadStats>();
		for (const id of threadIds) result.set(id, ThreadStats.empty(id));
		for (const row of rows) result.set(row.thread_id, new ThreadStats(row));
		return result;
	}

	async adjustMessageCount(threadId: ChannelID, delta: number): Promise<void> {
		if (delta === 0) return;
		await this.channelData.adjustThreadStats(threadId, delta, 0);
	}

	async create(params: CreateThreadParams): Promise<ThreadState> {
		const channel = params.channel;
		const threadId = channel.channel_id;
		const guildId = channel.guild_id;
		const parentId = channel.parent_id;
		if (guildId == null || parentId == null) {
			throw new Error('Thread channel rows require a guild and a parent');
		}
		await upsertOne(
			ThreadsByParent.upsertAll({parent_id: parentId, thread_id: threadId, guild_id: guildId, type: channel.type}),
		);
		const memberIds = params.members.map((member) => member.userId);
		const stateRow: ThreadStateRow = {
			thread_id: threadId,
			guild_id: guildId,
			parent_id: parentId,
			type: channel.type,
			archived: false,
			locked: false,
			invitable: isPrivateType(channel.type) ? (params.invitable ?? true) : null,
			auto_archive_duration: params.autoArchiveDuration,
			archive_timestamp: params.createdAt,
			created_at: params.createdAt,
			flags: params.flags,
			applied_tags: listOrNull(params.appliedTags),
			member_count: memberIds.length,
			member_ids_preview: listOrNull(previewWith([], memberIds)),
			has_starter: params.hasStarter,
			state_version: 1,
		};
		const claim = await this.claimState(stateRow);
		if (claim === null) {
			throw new ThreadAlreadyCreatedForMessageError();
		}
		if (claim.repaired) {
			await this.purgeMembers(threadId, claim.repaired.type);
		}
		const members = [...new Map(params.members.map((member) => [member.userId, member])).values()];
		if (members.length > 0) {
			const inserted = await executeConditional(
				ThreadMembers.conditionalBatch(
					members.map((member) => ({
						action: 'insert' as const,
						row: this.memberRow(stateRow, member, params.createdAt),
					})),
				),
			);
			if (!inserted) throw new ServiceUnavailableError();
		}
		const batch = new BatchBuilder();
		batch.addPrepared(Channels.upsertAll(channel));
		batch.addPrepared(
			ActiveThreadsByGuild.upsertAll({guild_id: guildId, thread_id: threadId, parent_id: parentId, type: channel.type}),
		);
		for (const member of members) {
			batch.addPrepared(ThreadMembersByUser.upsertAll(this.memberByUserRow(stateRow, member.userId)));
		}
		batch.addPrepared(
			ThreadParentConfigTable.patchByPk({guild_id: guildId, channel_id: parentId}, {has_threads: Db.set(true)}),
		);
		await batch.execute();
		if (THREAD_ONLY_CHANNEL_TYPES.has(params.parentType)) {
			await this.channelData.updateLastMessageId(parentId, channelIdToMessageId(threadId));
		}
		return new ThreadState(stateRow);
	}

	private async claimState(row: ThreadStateRow): Promise<{repaired: ThreadStateRow | null} | null> {
		if (await executeConditional(ThreadStateTable.insertIfNotExists(row))) return {repaired: null};
		const existing = await fetchOne<ThreadStateRow>(FETCH_STATE.bind({thread_id: row.thread_id}));
		if (!existing) {
			return (await executeConditional(ThreadStateTable.insertIfNotExists(row))) ? {repaired: null} : null;
		}
		if (Date.now() - existing.created_at.getTime() < DANGLING_CREATE_REPAIR_MS) return null;
		const channel = await fetchOne<ChannelRow>(FETCH_CHANNEL.bind({channel_id: row.thread_id, soft_deleted: false}));
		if (channel) return null;
		const cleared = await executeConditional(
			ThreadStateTable.conditionalDeleteByPk({thread_id: row.thread_id}, {state_version: existing.state_version}),
		);
		if (!cleared) return null;
		Logger.warn({threadId: row.thread_id.toString()}, 'Repaired a dangling thread create');
		return (await executeConditional(ThreadStateTable.insertIfNotExists(row))) ? {repaired: existing} : null;
	}

	private memberRow(state: ThreadStateRow, member: CreateThreadMember, joinTimestamp: Date): ThreadMemberRow {
		return {
			thread_id: state.thread_id,
			user_id: member.userId,
			guild_id: state.guild_id,
			parent_id: state.parent_id,
			join_timestamp: joinTimestamp,
			flags: member.flags,
			muted: false,
			mute_config: null,
		};
	}

	private memberByUserRow(
		state: Pick<ThreadStateRow, 'thread_id' | 'guild_id' | 'parent_id' | 'type'>,
		userId: UserID,
	): ThreadMembersByUserRow {
		return {
			user_id: userId,
			guild_id: state.guild_id,
			parent_id: state.parent_id,
			is_private: isPrivateType(state.type),
			thread_id: state.thread_id,
		};
	}

	async updateState(
		threadId: ChannelID,
		mutate: (current: ThreadState) => ThreadStatePatch | null,
	): Promise<ThreadStateTransition | null> {
		for (let attempt = 0; attempt < STATE_CAS_ATTEMPTS; attempt++) {
			const current = await this.getState(threadId);
			if (!current) return null;
			const requested = mutate(current);
			if (requested === null) return {previous: current, state: current};
			const patch = this.withArchiveRules(current, requested);
			if (Object.keys(patch).length === 0) return {previous: current, state: current};
			const nextRow: ThreadStateRow = {...current.toRow(), ...patch, state_version: current.stateVersion + 1};
			const applied = await executeConditional(
				ThreadStateTable.conditionalPatchByPk(
					{thread_id: threadId},
					{...toStateOps(patch), state_version: Db.set(current.stateVersion + 1)},
					{state_version: current.stateVersion},
				),
			);
			if (!applied) continue;
			const next = new ThreadState(nextRow);
			await this.moveIndexes(current, next);
			return {previous: current, state: next};
		}
		throw new ServiceUnavailableError();
	}

	private withArchiveRules(current: ThreadState, requested: ThreadStatePatch): ThreadStatePatch {
		const patch: ThreadStatePatch = {};
		for (const [key, value] of Object.entries(requested) as Array<[keyof ThreadStatePatch, unknown]>) {
			if (value === undefined) continue;
			if (sameValue(current.toRow()[key], value)) continue;
			(patch as Record<string, unknown>)[key] = value;
		}
		const archiving = patch.archived === true && !current.archived;
		const unarchiving = patch.archived === false && current.archived;
		if ((archiving || unarchiving) && patch.archive_timestamp === undefined) {
			patch.archive_timestamp = new Date();
		}
		if (archiving) {
			const flags = patch.flags ?? current.flags;
			if ((flags & ChannelFlags.PINNED) !== 0) patch.flags = flags & ~ChannelFlags.PINNED;
		}
		return patch;
	}

	private async moveIndexes(previous: ThreadState, next: ThreadState): Promise<void> {
		const batch = new BatchBuilder();
		const timestampMoved = !sameTime(previous.archiveTimestamp, next.archiveTimestamp);
		if (previous.archived && previous.archiveTimestamp && (!next.archived || timestampMoved)) {
			batch.addPrepared(ArchivedThreadsByParent.deleteByPk(this.archivedKey(previous, previous.archiveTimestamp)));
		}
		if (next.archived && next.archiveTimestamp && (!previous.archived || timestampMoved)) {
			batch.addPrepared(
				ArchivedThreadsByParent.upsertAll({
					...this.archivedKey(next, next.archiveTimestamp),
					guild_id: next.guildId,
				}),
			);
		}
		if (!previous.archived && next.archived) {
			batch.addPrepared(ActiveThreadsByGuild.deleteByPk({guild_id: next.guildId, thread_id: next.threadId}));
		}
		if (previous.archived && !next.archived) {
			batch.addPrepared(ActiveThreadsByGuild.upsertAll(this.activeRow(next)));
		}
		await batch.execute(false);
		if (previous.isPinned && !next.isPinned) {
			await this.releaseForumPin(next.parentId, next.threadId);
		}
		const latest = await this.getState(next.threadId);
		if (latest && latest.stateVersion !== next.stateVersion) {
			await this.repairThreadIndexes([next.threadId]);
		}
	}

	private archivedKey(
		state: ThreadState,
		archiveTimestamp: Date,
	): Pick<ArchivedThreadsByParentRow, 'parent_id' | 'is_private' | 'archive_timestamp' | 'thread_id'> {
		return {
			parent_id: state.parentId,
			is_private: state.isPrivate,
			archive_timestamp: archiveTimestamp,
			thread_id: state.threadId,
		};
	}

	private activeRow(state: ThreadState): ActiveThreadsByGuildRow {
		return {guild_id: state.guildId, thread_id: state.threadId, parent_id: state.parentId, type: state.type};
	}

	async claimForumPin(parentId: ChannelID, threadId: ChannelID): Promise<boolean> {
		for (let attempt = 0; attempt < 2; attempt++) {
			if (await executeConditional(ForumPinnedThread.insertIfNotExists({parent_id: parentId, thread_id: threadId}))) {
				return true;
			}
			const holder = await this.getForumPin(parentId);
			if (holder === null) continue;
			if (holder === threadId) return true;
			const holderState = await this.getState(holder);
			if (holderState?.isPinned && !holderState.archived) return false;
			await executeConditional(ForumPinnedThread.conditionalDeleteByPk({parent_id: parentId}, {thread_id: holder}));
		}
		return false;
	}

	async releaseForumPin(parentId: ChannelID, threadId: ChannelID): Promise<void> {
		await executeConditional(ForumPinnedThread.conditionalDeleteByPk({parent_id: parentId}, {thread_id: threadId}));
	}

	async getForumPin(parentId: ChannelID): Promise<ChannelID | null> {
		const row = await fetchOne<{parent_id: ChannelID; thread_id: ChannelID}>(
			FETCH_FORUM_PIN.bind({parent_id: parentId}),
		);
		return row?.thread_id ?? null;
	}

	async countActiveThreads(guildId: GuildID): Promise<number> {
		const row = await fetchOne<{count: bigint | number}>(COUNT_ACTIVE.bind({guild_id: guildId}));
		return Number(row?.count ?? 0);
	}

	async listActiveThreads(guildId: GuildID): Promise<Array<ThreadState>> {
		const rows = await fetchMany<ActiveThreadsByGuildRow>(FETCH_ACTIVE.bind({guild_id: guildId}));
		if (rows.length === 0) return [];
		const states = await this.getStates(rows.map((row) => row.thread_id));
		const byId = new Map(states.map((state) => [state.threadId, state]));
		const result: Array<ThreadState> = [];
		const stale: Array<ActiveThreadsByGuildRow> = [];
		for (const row of rows) {
			const state = byId.get(row.thread_id);
			if (state && !state.archived && state.guildId === guildId) {
				result.push(state);
			} else {
				stale.push(row);
			}
		}
		if (stale.length > 0) {
			await this.dropStaleRows(stale.map((row) => ActiveThreadsByGuild.deleteByPk(row)));
			this.reportDrift(stale.filter((row) => byId.has(row.thread_id)).map((row) => row.thread_id));
		}
		return result;
	}

	async listArchivedThreads(
		parentId: ChannelID,
		isPrivate: boolean,
		opts: {before?: Date; limit: number},
	): Promise<ArchivedThreadPage> {
		const want = opts.limit + 1;
		const collected: Array<ThreadState> = [];
		const seen = new Set<ChannelID>();
		let before = opts.before ? new Date(opts.before.getTime() - 1) : null;
		let pageSize = want;
		for (;;) {
			const query = archivedQuery(pageSize, before !== null);
			const rows = await fetchMany<ArchivedThreadsByParentRow>(
				query.bind(
					before !== null
						? {parent_id: parentId, is_private: isPrivate, before}
						: {parent_id: parentId, is_private: isPrivate},
				),
			);
			const fresh = rows.filter((row) => !seen.has(row.thread_id));
			for (const row of fresh) seen.add(row.thread_id);
			const states = await this.getStates(fresh.map((row) => row.thread_id));
			const byId = new Map(states.map((state) => [state.threadId, state]));
			const stale: Array<ArchivedThreadsByParentRow> = [];
			for (const row of fresh) {
				const state = byId.get(row.thread_id);
				if (
					state?.archived &&
					state.parentId === parentId &&
					state.isPrivate === isPrivate &&
					sameTime(state.archiveTimestamp, row.archive_timestamp)
				) {
					if (collected.length < want) collected.push(state);
				} else {
					stale.push(row);
				}
			}
			if (stale.length > 0) {
				await this.dropStaleRows(stale.map((row) => ArchivedThreadsByParent.deleteByPk(row)));
				this.reportDrift(stale.filter((row) => byId.has(row.thread_id)).map((row) => row.thread_id));
			}
			if (collected.length >= want || rows.length < pageSize) break;
			const last = rows[rows.length - 1]!.archive_timestamp;
			before = fresh.length === 0 ? new Date(last.getTime() - 1) : last;
			pageSize = Math.min(pageSize * 2, 1000);
		}
		return {threads: collected.slice(0, opts.limit), hasMore: collected.length > opts.limit};
	}

	async listJoinedPrivateArchivedThreads(
		userId: UserID,
		guildId: GuildID,
		parentId: ChannelID,
		opts: {before?: ChannelID; limit: number},
	): Promise<ArchivedThreadPage> {
		const want = opts.limit + 1;
		const collected: Array<ThreadState> = [];
		let before: ChannelID | null = opts.before ?? null;
		let pageSize = want;
		for (;;) {
			const base = {user_id: userId, guild_id: guildId, parent_id: parentId, is_private: true};
			const rows = await fetchMany<ThreadMembersByUserRow>(
				joinedPrivateQuery(pageSize, before !== null).bind(before !== null ? {...base, before} : base),
			);
			const states = await this.getStates(rows.map((row) => row.thread_id));
			for (const state of states) {
				if (state.archived && state.isPrivate && collected.length < want) collected.push(state);
			}
			if (collected.length >= want || rows.length < pageSize) break;
			before = rows[rows.length - 1]!.thread_id;
			pageSize = Math.min(pageSize * 2, 1000);
		}
		return {threads: collected.slice(0, opts.limit), hasMore: collected.length > opts.limit};
	}

	async listJoinedPrivateThreadIds(
		userId: UserID,
		guildId: GuildID,
		parentId: ChannelID,
		limit: number,
	): Promise<Array<ChannelID>> {
		const rows = await fetchMany<ThreadMembersByUserRow>(
			joinedPrivateQuery(limit, false).bind({
				user_id: userId,
				guild_id: guildId,
				parent_id: parentId,
				is_private: true,
			}),
		);
		return rows.map((row) => row.thread_id);
	}

	async listJoinedThreadIds(userId: UserID, guildId: GuildID): Promise<Array<ChannelID>> {
		const rows = await fetchMany<ThreadMembersByUserRow>(
			FETCH_JOINED_BY_GUILD.bind({user_id: userId, guild_id: guildId}),
		);
		return rows.map((row) => row.thread_id);
	}

	async repairThreadIndexes(threadIds: Array<ChannelID>): Promise<void> {
		const states = await this.getStates(threadIds);
		for (const state of states) {
			const batch = new BatchBuilder();
			if (state.archived) {
				batch.addPrepared(ActiveThreadsByGuild.deleteByPk({guild_id: state.guildId, thread_id: state.threadId}));
				if (state.archiveTimestamp) {
					batch.addPrepared(
						ArchivedThreadsByParent.upsertAll({
							...this.archivedKey(state, state.archiveTimestamp),
							guild_id: state.guildId,
						}),
					);
				}
			} else {
				batch.addPrepared(ActiveThreadsByGuild.upsertAll(this.activeRow(state)));
			}
			batch.addPrepared(
				ThreadsByParent.upsertAll({
					parent_id: state.parentId,
					thread_id: state.threadId,
					guild_id: state.guildId,
					type: state.type,
				}),
			);
			await batch.execute(false);
		}
	}

	private async dropStaleRows(deletes: Array<PreparedQuery>): Promise<void> {
		try {
			const batch = new BatchBuilder();
			for (const query of deletes) batch.addPrepared(query);
			await batch.execute(false);
		} catch (error) {
			Logger.warn(
				{error: error instanceof Error ? error.message : String(error)},
				'Failed to drop stale thread index rows',
			);
		}
	}

	private reportDrift(threadIds: Array<ChannelID>): void {
		if (threadIds.length === 0 || !this.onIndexDrift) return;
		try {
			this.onIndexDrift(threadIds);
		} catch (error) {
			Logger.warn({error: error instanceof Error ? error.message : String(error)}, 'Thread index drift hook failed');
		}
	}

	async getMember(threadId: ChannelID, userId: UserID): Promise<ThreadMember | null> {
		const row = await fetchOne<ThreadMemberRow>(FETCH_MEMBER.bind({thread_id: threadId, user_id: userId}));
		return row ? new ThreadMember(row) : null;
	}

	async getMembers(threadId: ChannelID, userIds: Array<UserID>): Promise<Array<ThreadMember>> {
		if (userIds.length === 0) return [];
		const rows = await fetchManyInChunks<ThreadMemberRow>(FETCH_MEMBERS_BY_IDS, userIds, (chunk) => ({
			thread_id: threadId,
			user_ids: chunk,
		}));
		return rows.map((row) => new ThreadMember(row));
	}

	async listMembers(threadId: ChannelID, opts: {after?: UserID; limit: number}): Promise<Array<ThreadMember>> {
		const bounded = opts.after !== undefined;
		const rows = await fetchMany<ThreadMemberRow>(
			membersQuery(opts.limit, bounded).bind(
				bounded ? {thread_id: threadId, after: opts.after as UserID} : {thread_id: threadId},
			),
		);
		return rows.map((row) => new ThreadMember(row));
	}

	async addMembers(
		threadId: ChannelID,
		members: Array<CreateThreadMember>,
		opts?: {joinTimestamp?: Date},
	): Promise<ThreadMemberAddResult | null> {
		const unique = [...new Map(members.map((member) => [member.userId, member])).values()];
		const joinTimestamp = opts?.joinTimestamp ?? new Date();
		for (let attempt = 0; attempt < STATE_CAS_ATTEMPTS; attempt++) {
			const state = await this.getState(threadId);
			if (!state) return null;
			if (unique.length === 0) return {added: [], state};
			const existing = new Set(
				(
					await this.getMembers(
						threadId,
						unique.map((m) => m.userId),
					)
				).map((m) => m.userId),
			);
			const toAdd = unique.filter((member) => !existing.has(member.userId));
			if (toAdd.length === 0) return {added: [], state};
			if (state.memberCount + toAdd.length > MAX_THREAD_MEMBERS) {
				throw new MaxThreadMembersError(MAX_THREAD_MEMBERS);
			}
			const stateRow = state.toRow();
			const rows = toAdd.map((member) => this.memberRow(stateRow, member, joinTimestamp));
			const inserted = await executeConditional(
				ThreadMembers.conditionalBatch(rows.map((row) => ({action: 'insert' as const, row}))),
			);
			if (!inserted) continue;
			const next = await this.bumpMembership(
				threadId,
				rows.map((row) => row.user_id),
				[],
			).catch(async (error: unknown) => {
				await executeConditional(
					ThreadMembers.conditionalBatch(
						rows.map((row) => ({
							action: 'delete' as const,
							pk: {thread_id: threadId, user_id: row.user_id},
							expected: {join_timestamp: row.join_timestamp},
						})),
					),
				);
				throw error;
			});
			const byUser = new BatchBuilder();
			for (const row of rows)
				byUser.addPrepared(ThreadMembersByUser.upsertAll(this.memberByUserRow(stateRow, row.user_id)));
			await byUser.execute(false);
			return {added: rows.map((row) => new ThreadMember(row)), state: next ?? state};
		}
		throw new ServiceUnavailableError();
	}

	async removeMembers(threadId: ChannelID, userIds: Array<UserID>): Promise<ThreadMemberRemoveResult> {
		const unique = [...new Set(userIds)];
		for (let attempt = 0; attempt < STATE_CAS_ATTEMPTS; attempt++) {
			const existing = await this.getMembers(threadId, unique);
			if (existing.length === 0) return {removed: [], state: await this.getState(threadId)};
			const removed = await executeConditional(
				ThreadMembers.conditionalBatch(
					existing.map((member) => ({
						action: 'delete' as const,
						pk: {thread_id: threadId, user_id: member.userId},
						expected: {join_timestamp: member.joinTimestamp},
					})),
				),
			);
			if (!removed) continue;
			const state = await this.bumpMembership(
				threadId,
				[],
				existing.map((member) => member.userId),
			).catch(async (error: unknown) => {
				await executeConditional(
					ThreadMembers.conditionalBatch(existing.map((member) => ({action: 'insert' as const, row: member.toRow()}))),
				);
				throw error;
			});
			const byUser = new BatchBuilder();
			const types = state ? [state.type] : [ChannelTypes.PUBLIC_THREAD, ChannelTypes.PRIVATE_THREAD];
			for (const member of existing) {
				for (const type of types) {
					byUser.addPrepared(
						ThreadMembersByUser.deleteByPk(
							this.memberByUserRow(
								{thread_id: threadId, guild_id: member.guildId, parent_id: member.parentId, type},
								member.userId,
							),
						),
					);
				}
			}
			await byUser.execute(false);
			return {removed: existing, state};
		}
		throw new ServiceUnavailableError();
	}

	private async bumpMembership(
		threadId: ChannelID,
		added: Array<UserID>,
		removed: Array<UserID>,
	): Promise<ThreadState | null> {
		for (let attempt = 0; attempt < STATE_CAS_ATTEMPTS * 2; attempt++) {
			const state = await this.getState(threadId);
			if (!state) return null;
			const memberCount = Math.max(0, state.memberCount + added.length - removed.length);
			if (added.length > 0 && memberCount > MAX_THREAD_MEMBERS) throw new MaxThreadMembersError(MAX_THREAD_MEMBERS);
			const preview = previewWith(state.memberIdsPreview, added).filter((id) => !removed.includes(id));
			const applied = await executeConditional(
				ThreadStateTable.conditionalPatchByPk(
					{thread_id: threadId},
					{
						member_count: Db.set(memberCount),
						member_ids_preview: preview.length > 0 ? Db.set(preview) : Db.clear(),
						state_version: Db.set(state.stateVersion + 1),
					},
					{state_version: state.stateVersion},
				),
			);
			if (applied) {
				return new ThreadState({
					...state.toRow(),
					member_count: memberCount,
					member_ids_preview: listOrNull(preview),
					state_version: state.stateVersion + 1,
				});
			}
		}
		throw new ServiceUnavailableError();
	}

	async updateMemberSettings(expected: ThreadMember, patch: ThreadMemberSettingsPatch): Promise<ThreadMember | null> {
		const ops: Partial<{flags: DbOp<number>; muted: DbOp<boolean>; mute_config: DbOp<ThreadMemberRow['mute_config']>}> =
			{};
		if (patch.flags !== undefined) ops.flags = Db.set(patch.flags);
		if (patch.muted !== undefined) ops.muted = Db.set(patch.muted);
		if (patch.muteConfig !== undefined) ops.mute_config = patch.muteConfig ? Db.set(patch.muteConfig) : Db.clear();
		if (Object.keys(ops).length === 0) return expected;
		const applied = await executeConditional(
			ThreadMembers.conditionalPatchByPk({thread_id: expected.threadId, user_id: expected.userId}, ops, {
				join_timestamp: expected.joinTimestamp,
				flags: expected.flags,
				muted: expected.muted,
			}),
		);
		if (!applied) return null;
		const row = expected.toRow();
		return new ThreadMember({
			...row,
			flags: patch.flags ?? row.flags,
			muted: patch.muted ?? row.muted,
			mute_config: patch.muteConfig !== undefined ? patch.muteConfig : row.mute_config,
		});
	}

	async listThreadIdsByParent(
		parentId: ChannelID,
		opts: {after?: ChannelID; limit: number},
	): Promise<Array<ChannelID>> {
		const bounded = opts.after !== undefined;
		const query = ThreadsByParent.select({
			where: bounded
				? [ThreadsByParent.where.eq('parent_id'), ThreadsByParent.where.gt('thread_id', 'after')]
				: ThreadsByParent.where.eq('parent_id'),
			limit: opts.limit,
		});
		const rows = await fetchMany<ThreadsByParentRow>(
			query.bind(bounded ? {parent_id: parentId, after: opts.after as ChannelID} : {parent_id: parentId}),
		);
		return rows.map((row) => row.thread_id);
	}

	async listParentThreads(parentId: ChannelID): Promise<Array<{threadId: ChannelID; type: number}>> {
		const threads: Array<{threadId: ChannelID; type: number}> = [];
		let after: ChannelID | null = null;
		for (;;) {
			const rows: Array<ThreadsByParentRow> = await fetchMany<ThreadsByParentRow>(
				after === null
					? FETCH_THREADS_BY_PARENT_FIRST.bind({parent_id: parentId})
					: FETCH_THREADS_BY_PARENT_AFTER.bind({parent_id: parentId, after}),
			);
			for (const row of rows) threads.push({threadId: row.thread_id, type: row.type});
			if (rows.length < ENUMERATION_PAGE_SIZE) break;
			after = rows[rows.length - 1]!.thread_id;
		}
		return threads;
	}

	async setThreadType(threadId: ChannelID, type: number): Promise<ThreadState | null> {
		for (let attempt = 0; attempt < STATE_CAS_ATTEMPTS; attempt++) {
			const current = await this.getState(threadId);
			if (!current) return null;
			if (current.type !== type) {
				const applied = await executeConditional(
					ThreadStateTable.conditionalPatchByPk(
						{thread_id: threadId},
						{type: Db.set(type), state_version: Db.set(current.stateVersion + 1)},
						{state_version: current.stateVersion},
					),
				);
				if (!applied) continue;
			}
			const next = new ThreadState({
				...current.toRow(),
				type,
				state_version: current.type !== type ? current.stateVersion + 1 : current.stateVersion,
			});
			const batch = new BatchBuilder();
			batch.addPrepared(Channels.patchByPk({channel_id: threadId, soft_deleted: false}, {type: Db.set(type)}));
			batch.addPrepared(
				ThreadsByParent.upsertAll({parent_id: next.parentId, thread_id: threadId, guild_id: next.guildId, type}),
			);
			if (!next.archived) batch.addPrepared(ActiveThreadsByGuild.upsertAll(this.activeRow(next)));
			await batch.execute();
			const channel = await fetchOne<ChannelRow>(FETCH_CHANNEL.bind({channel_id: threadId, soft_deleted: false}));
			if (channel && channel.guild_id == null) {
				await deleteOneOrMany(Channels.deleteByPk({channel_id: threadId, soft_deleted: false}));
			}
			const latest = await this.getState(threadId);
			if (!latest) {
				const cleanup = new BatchBuilder();
				cleanup.addPrepared(ActiveThreadsByGuild.deleteByPk({guild_id: next.guildId, thread_id: threadId}));
				cleanup.addPrepared(ThreadsByParent.deleteByPk({parent_id: next.parentId, thread_id: threadId}));
				await cleanup.execute(false);
				return null;
			}
			if (!next.archived && latest.archived) {
				await deleteOneOrMany(ActiveThreadsByGuild.deleteByPk({guild_id: next.guildId, thread_id: threadId}));
			}
			return next;
		}
		throw new ServiceUnavailableError();
	}

	async listGuildThreadIds(
		guildId: GuildID,
		opts?: {activeSince?: Date; parents?: ReadonlyArray<{id: ChannelID; type: number}>},
	): Promise<Array<ChannelID>> {
		if (!(await isTainted(guildId, {fresh: true}))) return [];
		const parents =
			opts?.parents ??
			(await this.channelData.listGuildChannels(guildId, 'maintenance')).map((channel) => ({
				id: channel.id,
				type: channel.type,
			}));
		const threadIds: Array<ChannelID> = [];
		for (const parent of parents) {
			if (!THREAD_PARENT_CHANNEL_TYPES.has(parent.type)) continue;
			let after: ChannelID | null = null;
			for (;;) {
				const rows: Array<ThreadsByParentRow> = await fetchMany<ThreadsByParentRow>(
					after === null
						? FETCH_THREADS_BY_PARENT_FIRST.bind({parent_id: parent.id})
						: FETCH_THREADS_BY_PARENT_AFTER.bind({parent_id: parent.id, after}),
				);
				for (const row of rows) threadIds.push(row.thread_id);
				if (rows.length < ENUMERATION_PAGE_SIZE) break;
				after = rows[rows.length - 1]!.thread_id;
			}
		}
		const activeSince = opts?.activeSince;
		if (!activeSince || threadIds.length === 0) return threadIds;
		const channels = await fetchManyInChunks<Pick<ChannelRow, 'channel_id' | 'last_message_id'>>(
			FETCH_CHANNELS,
			threadIds,
			(chunk) => ({channel_ids: chunk, soft_deleted: false}),
		);
		const lastActivity = new Map(channels.map((row) => [row.channel_id, row.last_message_id ?? row.channel_id]));
		const cutoff = activeSince.getTime();
		return threadIds.filter((id) => snowflakeToDate(lastActivity.get(id) ?? id).getTime() >= cutoff);
	}

	async purgeThread(threadId: ChannelID): Promise<void> {
		const state = await this.getState(threadId);
		const channel = await fetchOne<ChannelRow>(FETCH_CHANNEL.bind({channel_id: threadId, soft_deleted: false}));
		const parentId = state?.parentId ?? channel?.parent_id ?? null;
		await this.messages.deleteAllChannelMessages(threadId);
		await this.purgeMembers(threadId, state?.type ?? channel?.type ?? ChannelTypes.PUBLIC_THREAD);
		const guildId = state?.guildId ?? channel?.guild_id ?? null;
		const side = new BatchBuilder();
		if (guildId !== null) {
			side.addPrepared(ActiveThreadsByGuild.deleteByPk({guild_id: guildId, thread_id: threadId}));
		}
		if (state?.archived && state.archiveTimestamp) {
			side.addPrepared(ArchivedThreadsByParent.deleteByPk(this.archivedKey(state, state.archiveTimestamp)));
		}
		side.addPrepared(ThreadStatsTable.deleteByPk({thread_id: threadId}));
		await side.execute(false);
		if (state?.isPinned) await this.releaseForumPin(state.parentId, threadId);
		await this.channelData.delete(threadId, channel?.guild_id ?? state?.guildId, channel?.type ?? state?.type);
		await deleteOneOrMany(ThreadStateTable.deleteByPk({thread_id: threadId}));
		if (guildId !== null) {
			await deleteOneOrMany(ActiveThreadsByGuild.deleteByPk({guild_id: guildId, thread_id: threadId}));
		}
		if (parentId !== null) {
			await deleteOneOrMany(ThreadsByParent.deleteByPk({parent_id: parentId, thread_id: threadId}));
		}
	}

	async revertParentLastMessageId(parentId: ChannelID, threadId: ChannelID, previous: MessageID | null): Promise<void> {
		const parent = await fetchOne<ChannelRow>(FETCH_CHANNEL.bind({channel_id: parentId, soft_deleted: false}));
		if (parent?.last_message_id !== channelIdToMessageId(threadId)) return;
		await upsertOne(
			Channels.patchByPk(
				{channel_id: parentId, soft_deleted: false},
				{last_message_id: previous === null ? Db.clear() : Db.set(previous)},
			),
		);
	}

	private async purgeMembers(threadId: ChannelID, type: number): Promise<void> {
		let after: UserID | undefined;
		for (;;) {
			const page = await this.listMembers(threadId, {after, limit: MEMBER_PAGE_SIZE});
			if (page.length === 0) break;
			const batch = new BatchBuilder();
			for (const member of page) {
				batch.addPrepared(
					ThreadMembersByUser.deleteByPk(
						this.memberByUserRow(
							{thread_id: threadId, guild_id: member.guildId, parent_id: member.parentId, type},
							member.userId,
						),
					),
				);
			}
			await batch.executeChunked(100);
			if (page.length < MEMBER_PAGE_SIZE) break;
			after = page[page.length - 1]!.userId;
		}
		await deleteOneOrMany(ThreadMembers.deletePartition({thread_id: threadId}));
	}

	async purgeGuild(guildId: GuildID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(ThreadParentConfigTable.deletePartition({guild_id: guildId}));
		batch.addPrepared(ThreadOnlyChannelsByGuild.deletePartition({guild_id: guildId}));
		batch.addPrepared(ActiveThreadsByGuild.deletePartition({guild_id: guildId}));
		batch.addPrepared(GuildThreadState.deleteByPk({guild_id: guildId}));
		await batch.execute(false);
	}

	async getGuildMarker(guildId: GuildID): Promise<GuildThreadStateRow | null> {
		return fetchOne<GuildThreadStateRow>(FETCH_GUILD_MARKER.bind({guild_id: guildId}));
	}

	async ensureGuildMarker(guildId: GuildID, opts?: {permsSeededAt?: Date}): Promise<void> {
		const created = await insertGuildThreadMarker(guildId, opts?.permsSeededAt ?? null);
		if (!created && opts?.permsSeededAt) {
			await this.markGuildPermsSeeded(guildId, opts.permsSeededAt);
		}
	}

	async markGuildPermsSeeded(guildId: GuildID, at: Date): Promise<void> {
		for (let attempt = 0; attempt < STATE_CAS_ATTEMPTS; attempt++) {
			const marker = await this.getGuildMarker(guildId);
			if (!marker) {
				if (await insertGuildThreadMarker(guildId, at)) return;
				continue;
			}
			if (marker.perms_seeded_at) return;
			const patched = await executeConditional(
				GuildThreadState.conditionalPatchByPk(
					{guild_id: guildId},
					{perms_seeded_at: Db.set(at)},
					{first_active_at: marker.first_active_at},
				),
			);
			if (patched) return;
		}
		throw new ServiceUnavailableError();
	}

	async markGuildSearchBackfilled(guildId: GuildID, at: Date): Promise<void> {
		await this.patchGuildMarker(guildId, {search_backfilled_at: Db.set(at)});
	}

	async clearGuildSearchBackfilled(guildId: GuildID): Promise<void> {
		const marker = await this.getGuildMarker(guildId);
		if (!marker?.search_backfilled_at) return;
		await this.patchGuildMarker(guildId, {search_backfilled_at: Db.clear()});
	}

	private async patchGuildMarker(guildId: GuildID, patch: {search_backfilled_at: DbOp<Date | null>}): Promise<void> {
		for (let attempt = 0; attempt < STATE_CAS_ATTEMPTS; attempt++) {
			const marker = await this.getGuildMarker(guildId);
			if (!marker) {
				await insertGuildThreadMarker(guildId, null);
				continue;
			}
			const patched = await executeConditional(
				GuildThreadState.conditionalPatchByPk({guild_id: guildId}, patch, {first_active_at: marker.first_active_at}),
			);
			if (patched) return;
		}
		throw new ServiceUnavailableError();
	}

	async getParentConfig(guildId: GuildID, channelId: ChannelID): Promise<ThreadParentConfig | null> {
		const row = await fetchOne<ThreadParentConfigRow>(
			FETCH_PARENT_CONFIG.bind({guild_id: guildId, channel_id: channelId}),
		);
		return row ? new ThreadParentConfig(row) : null;
	}

	async listParentConfigs(guildId: GuildID): Promise<Array<ThreadParentConfig>> {
		const rows = await fetchMany<ThreadParentConfigRow>(FETCH_PARENT_CONFIGS.bind({guild_id: guildId}));
		return rows.map((row) => new ThreadParentConfig(row));
	}

	async patchParentConfig(guildId: GuildID, channelId: ChannelID, patch: ThreadParentConfigPatch): Promise<void> {
		const ops: Record<string, DbOp<unknown>> = {};
		for (const [key, value] of Object.entries(patch)) {
			if (value === undefined) continue;
			const empty = value === null || (Array.isArray(value) && value.length === 0);
			ops[key] = empty ? Db.clear() : Db.set(value);
		}
		if (Object.keys(ops).length === 0) return;
		await upsertOne(
			ThreadParentConfigTable.patchByPk(
				{guild_id: guildId, channel_id: channelId},
				ops as Parameters<typeof ThreadParentConfigTable.patchByPk>[1],
			),
		);
	}

	async deleteParentConfig(guildId: GuildID, channelId: ChannelID): Promise<void> {
		await deleteOneOrMany(ThreadParentConfigTable.deleteByPk({guild_id: guildId, channel_id: channelId}));
	}
}
