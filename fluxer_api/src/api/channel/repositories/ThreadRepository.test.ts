// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {readFileSync} from 'node:fs';
import {createServer} from 'node:net';
import {fileURLToPath} from 'node:url';
import {
	type ChannelID,
	channelIdToMessageId,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type GuildID,
} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/repositories/ChannelRepository';
import type {CreateThreadParams} from '@app/api/channel/repositories/IThreadRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {
	type CassandraQueryExecutorForTesting,
	deleteOneOrMany,
	executeConditional,
	fetchMany,
	fetchOne,
	setCassandraQueryExecutorForTesting,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import {CHANNEL_COLUMNS, type ChannelRow} from '@app/api/database/types/ChannelTypes';
import {MESSAGE_COLUMNS} from '@app/api/database/types/MessageTypes';
import {
	clearChannelThreadsTaintCacheForTesting,
	syncChannelThreadsConfig,
} from '@app/api/experiment/ChannelThreadsGate';
import {
	ActiveThreadsByGuild,
	ArchivedThreadsByParent,
	Channels,
	ChannelsByGuild,
	ForumPinnedThread,
	GuildThreadState,
	ThreadMembers,
	ThreadMembersByUser,
	ThreadOnlyChannelsByGuild,
	ThreadParentConfig,
	ThreadState,
	ThreadStats,
	ThreadsByParent,
} from '@app/api/Tables';
import {startDockerContainer} from '@app/api/test/DockerTestContainer';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelFlags, MAX_THREAD_MEMBERS} from '@fluxer/constants/src/ThreadConstants';
import {MaxThreadMembersError} from '@fluxer/errors/src/domains/channel/MaxThreadMembersError';
import {ThreadAlreadyCreatedForMessageError} from '@fluxer/errors/src/domains/channel/ThreadAlreadyCreatedForMessageError';
import {
	type ChannelThreadsConfig,
	ChannelThreadsConfigSchema,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const GUILD_ID = createGuildID(1_900_000_000_000_000_000n);
const PARENT_ID = createChannelID(1_900_000_000_000_000_100n);
const FORUM_ID = createChannelID(1_900_000_000_000_000_200n);
const OWNER_ID = createUserID(1_900_000_000_000_001_000n);

class RecordingExecutor implements CassandraQueryExecutorForTesting {
	readonly statements: Array<string> = [];

	constructor(private readonly inner: CassandraQueryExecutorForTesting) {}

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		const meta = query.kvMeta;
		this.record(meta, meta?.conditions || meta?.ifNotExists || meta?.batchEntries ? 'cas' : undefined);
		return this.inner.executeQuery<T, P>(query);
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic?: boolean,
	): Promise<void> {
		for (const entry of queries) this.record(entry.meta);
		await this.inner.executeBatch(queries, atomic);
	}

	count(statement: string): number {
		return this.statements.filter((entry) => entry === statement).length;
	}

	private record(meta: KvQueryMeta | undefined, prefix?: string): void {
		if (!meta) return;
		this.statements.push(`${prefix ?? meta.action}:${meta.table.name}`);
	}
}

function parseConfig(raw: string | null): ChannelThreadsConfig {
	return ChannelThreadsConfigSchema.parse(raw ? JSON.parse(raw) : {});
}

function setThreadsConfig(patch: Partial<ChannelThreadsConfig> | null): void {
	clearChannelThreadsTaintCacheForTesting();
	syncChannelThreadsConfig(patch === null ? null : JSON.stringify(patch), parseConfig);
}

function channelRow(channelId: ChannelID, type: number, overrides: Partial<ChannelRow> = {}): ChannelRow {
	return {
		channel_id: channelId,
		guild_id: GUILD_ID,
		type,
		name: `channel-${channelId}`,
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: null,
		position: 0,
		owner_id: null,
		recipient_ids: null,
		nsfw: false,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: 0,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 1,
		...overrides,
	};
}

let lastId = 0n;
function freshThreadId(offsetMs = 0): ChannelID {
	const base = createSnowflakeFromTimestamp(Date.now() + offsetMs);
	if (offsetMs !== 0) return createChannelID(base);
	lastId = base > lastId ? base : lastId + 1n;
	return createChannelID(lastId);
}

function createParams(threadId: ChannelID, overrides: Partial<CreateThreadParams> = {}): CreateThreadParams {
	const type = overrides.channel?.type ?? ChannelTypes.PUBLIC_THREAD;
	return {
		channel: channelRow(threadId, type, {parent_id: PARENT_ID, owner_id: OWNER_ID, indexed_at: new Date()}),
		parentType: ChannelTypes.GUILD_TEXT,
		autoArchiveDuration: 4320,
		invitable: null,
		flags: 0,
		appliedTags: [],
		hasStarter: false,
		createdAt: new Date(),
		members: [{userId: OWNER_ID, flags: 1}],
		...overrides,
	};
}

function describeThreadRepository(backend: string, makeExecutor: () => Promise<CassandraQueryExecutorForTesting>) {
	describe(`ThreadRepository (${backend})`, () => {
		let executor: RecordingExecutor;
		let repositories: ChannelRepository;

		beforeEach(async () => {
			executor = new RecordingExecutor(await makeExecutor());
			setCassandraQueryExecutorForTesting(executor);
			setThreadsConfig(null);
			repositories = new ChannelRepository();
			await upsertOne(Channels.upsertAll(channelRow(PARENT_ID, ChannelTypes.GUILD_TEXT)));
			await upsertOne(ChannelsByGuild.upsertAll({guild_id: GUILD_ID, channel_id: PARENT_ID}));
			executor.statements.length = 0;
		});

		afterEach(() => {
			setThreadsConfig(null);
		});

		async function taint(guildId: GuildID = GUILD_ID): Promise<void> {
			setThreadsConfig({enabled: true, ever_enabled: true, enabled_guild_ids: [guildId.toString()]});
			await upsertOne(
				GuildThreadState.upsertAll({
					guild_id: guildId,
					first_active_at: new Date(),
					perms_seeded_at: null,
					search_backfilled_at: null,
				}),
			);
		}

		it('writes a thread into its side tables and never into the guild channel index', async () => {
			const threadId = freshThreadId();
			const state = await repositories.threads.create(createParams(threadId));
			expect(state.stateVersion).toBe(1);
			expect(state.memberCount).toBe(1);
			expect(state.memberIdsPreview).toEqual([OWNER_ID]);
			const channel = await repositories.channelData.findUnique(threadId);
			expect(channel?.isThread()).toBe(true);
			expect(await repositories.channelData.listGuildChannels(GUILD_ID, 'enrolled')).toHaveLength(1);
			expect(
				await fetchOne(
					ChannelsByGuild.selectCql({
						where: [ChannelsByGuild.where.eq('guild_id'), ChannelsByGuild.where.eq('channel_id')],
					}),
					{guild_id: GUILD_ID, channel_id: threadId},
				),
			).toBeNull();
			expect((await repositories.threads.listActiveThreads(GUILD_ID)).map((t) => t.threadId)).toEqual([threadId]);
			expect(await repositories.threads.listThreadIdsByParent(PARENT_ID, {limit: 10})).toEqual([threadId]);
			expect((await repositories.threads.getParentConfig(GUILD_ID, PARENT_ID))?.hasThreads).toBe(true);
			expect((await repositories.threads.getMember(threadId, OWNER_ID))?.flags).toBe(1);
		});

		it('lets exactly one of two concurrent creates win', async () => {
			const threadId = freshThreadId();
			const results = await Promise.allSettled([
				repositories.threads.create(createParams(threadId)),
				repositories.threads.create(createParams(threadId)),
			]);
			expect(results.filter((result) => result.status === 'fulfilled')).toHaveLength(1);
			const rejected = results.find((result) => result.status === 'rejected') as PromiseRejectedResult;
			expect(rejected.reason).toBeInstanceOf(ThreadAlreadyCreatedForMessageError);
		});

		it('refuses a create over a fresh dangling state and repairs one older than 30 seconds', async () => {
			const threadId = freshThreadId();
			const dangling = {
				thread_id: threadId,
				guild_id: GUILD_ID,
				parent_id: PARENT_ID,
				type: ChannelTypes.PUBLIC_THREAD,
				archived: false,
				locked: false,
				invitable: null,
				auto_archive_duration: 4320,
				archive_timestamp: new Date(),
				created_at: new Date(),
				flags: 0,
				applied_tags: null,
				member_count: 0,
				member_ids_preview: null,
				has_starter: false,
				state_version: 1,
			};
			expect(await executeConditional(ThreadState.insertIfNotExists(dangling))).toBe(true);
			await expect(repositories.threads.create(createParams(threadId))).rejects.toBeInstanceOf(
				ThreadAlreadyCreatedForMessageError,
			);
			const staleId = freshThreadId();
			expect(
				await executeConditional(
					ThreadState.insertIfNotExists({...dangling, thread_id: staleId, created_at: new Date(Date.now() - 31_000)}),
				),
			).toBe(true);
			const repaired = await repositories.threads.create(createParams(staleId));
			expect(repaired.memberCount).toBe(1);
			expect(await repositories.channelData.findUnique(staleId)).not.toBeNull();
		});

		it('clears the crashed creator memberships when repairing a dangling create', async () => {
			const threadId = freshThreadId();
			const crashed = createUserID(1_900_000_000_000_003_000n);
			const createdAt = new Date(Date.now() - 31_000);
			expect(
				await executeConditional(
					ThreadState.insertIfNotExists({
						thread_id: threadId,
						guild_id: GUILD_ID,
						parent_id: PARENT_ID,
						type: ChannelTypes.PRIVATE_THREAD,
						archived: false,
						locked: false,
						invitable: true,
						auto_archive_duration: 4320,
						archive_timestamp: createdAt,
						created_at: createdAt,
						flags: 0,
						applied_tags: null,
						member_count: 1,
						member_ids_preview: [crashed],
						has_starter: false,
						state_version: 1,
					}),
				),
			).toBe(true);
			const memberRow = {
				thread_id: threadId,
				guild_id: GUILD_ID,
				parent_id: PARENT_ID,
				join_timestamp: createdAt,
				flags: 1,
				muted: false,
				mute_config: null,
			};
			await upsertOne(ThreadMembers.upsertAll({...memberRow, user_id: crashed}));
			await upsertOne(ThreadMembers.upsertAll({...memberRow, user_id: OWNER_ID}));
			await upsertOne(
				ThreadMembersByUser.upsertAll({
					user_id: crashed,
					guild_id: GUILD_ID,
					parent_id: PARENT_ID,
					is_private: true,
					thread_id: threadId,
				}),
			);
			const repaired = await repositories.threads.create(createParams(threadId));
			expect(repaired.memberCount).toBe(1);
			const members = await repositories.threads.listMembers(threadId, {limit: 10});
			expect(members.map((member) => member.userId)).toEqual([OWNER_ID]);
			expect(members[0]!.joinTimestamp.getTime()).not.toBe(createdAt.getTime());
			expect(await repositories.threads.listJoinedThreadIds(crashed, GUILD_ID)).toEqual([]);
		});

		it('removes a private membership index row when the thread state is gone', async () => {
			const threadId = freshThreadId();
			const params = createParams(threadId);
			params.channel.type = ChannelTypes.PRIVATE_THREAD;
			await repositories.threads.create(params);
			await deleteOneOrMany(ThreadState.deleteByPk({thread_id: threadId}));
			const result = await repositories.threads.removeMembers(threadId, [OWNER_ID]);
			expect(result.removed.map((member) => member.userId)).toEqual([OWNER_ID]);
			expect(await repositories.threads.listJoinedThreadIds(OWNER_ID, GUILD_ID)).toEqual([]);
		});

		it('ignores a dangling threads_by_parent row in lists but enumerates it for maintenance', async () => {
			await taint();
			const threadId = freshThreadId();
			await upsertOne(
				ThreadsByParent.upsertAll({
					parent_id: PARENT_ID,
					thread_id: threadId,
					guild_id: GUILD_ID,
					type: ChannelTypes.PUBLIC_THREAD,
				}),
			);
			expect(await repositories.threads.listActiveThreads(GUILD_ID)).toEqual([]);
			expect(await repositories.threads.listGuildThreadIds(GUILD_ID)).toEqual([threadId]);
		});

		it('moves index rows on archive and unarchive and clears the pin', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId, {flags: ChannelFlags.PINNED}));
			expect(await repositories.threads.claimForumPin(FORUM_ID, threadId)).toBe(true);
			await upsertOne(ThreadState.patchByPk({thread_id: threadId}, {parent_id: {kind: 'set', value: FORUM_ID}}));
			const archived = await repositories.threads.updateState(threadId, () => ({archived: true}));
			expect(archived?.state.archived).toBe(true);
			expect(archived?.state.isPinned).toBe(false);
			expect(archived?.state.stateVersion).toBe(2);
			expect(await repositories.threads.getForumPin(FORUM_ID)).toBeNull();
			expect(await repositories.threads.listActiveThreads(GUILD_ID)).toEqual([]);
			const page = await repositories.threads.listArchivedThreads(FORUM_ID, false, {limit: 10});
			expect(page.threads.map((t) => t.threadId)).toEqual([threadId]);
			const unarchived = await repositories.threads.updateState(threadId, () => ({archived: false}));
			expect(unarchived?.state.archived).toBe(false);
			expect((await repositories.threads.listActiveThreads(GUILD_ID)).map((t) => t.threadId)).toEqual([threadId]);
			expect((await repositories.threads.listArchivedThreads(FORUM_ID, false, {limit: 10})).threads).toEqual([]);
		});

		it('serialises racing transitions through the state version', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId));
			const [archive, lock] = await Promise.all([
				repositories.threads.updateState(threadId, () => ({archived: true})),
				repositories.threads.updateState(threadId, () => ({locked: true})),
			]);
			expect(archive).not.toBeNull();
			expect(lock).not.toBeNull();
			const state = await repositories.threads.getState(threadId);
			expect(state?.archived).toBe(true);
			expect(state?.locked).toBe(true);
			expect(state?.stateVersion).toBe(3);
			expect(await repositories.threads.listActiveThreads(GUILD_ID)).toEqual([]);
			expect((await repositories.threads.listArchivedThreads(PARENT_ID, false, {limit: 10})).threads).toHaveLength(1);
		});

		it('drops and reports stale index rows, and repairs missing ones', async () => {
			const drift = vi.fn();
			const threads = new ThreadRepository(repositories.channelData, repositories.messages, drift);
			const threadId = freshThreadId();
			await threads.create(createParams(threadId));
			await threads.updateState(threadId, () => ({archived: true}));
			await upsertOne(
				ActiveThreadsByGuild.upsertAll({
					guild_id: GUILD_ID,
					thread_id: threadId,
					parent_id: PARENT_ID,
					type: ChannelTypes.PUBLIC_THREAD,
				}),
			);
			expect(await threads.listActiveThreads(GUILD_ID)).toEqual([]);
			expect(drift).toHaveBeenCalledWith([threadId]);
			expect(
				await fetchMany(ActiveThreadsByGuild.selectCql({where: ActiveThreadsByGuild.where.eq('guild_id')}), {
					guild_id: GUILD_ID,
				}),
			).toEqual([]);
			const state = await threads.getState(threadId);
			await executeConditional(
				ArchivedThreadsByParent.conditionalDeleteByPk(
					{parent_id: PARENT_ID, is_private: false, archive_timestamp: state!.archiveTimestamp!, thread_id: threadId},
					{guild_id: GUILD_ID},
				),
			);
			expect((await threads.listArchivedThreads(PARENT_ID, false, {limit: 10})).threads).toEqual([]);
			await threads.repairThreadIndexes([threadId]);
			expect((await threads.listArchivedThreads(PARENT_ID, false, {limit: 10})).threads).toHaveLength(1);
		});

		it('claims one pinned post per forum and recovers a stale claim', async () => {
			const first = freshThreadId();
			const second = freshThreadId();
			await repositories.threads.create(createParams(first, {flags: ChannelFlags.PINNED}));
			await repositories.threads.create(createParams(second));
			expect(await repositories.threads.claimForumPin(FORUM_ID, first)).toBe(true);
			expect(await repositories.threads.claimForumPin(FORUM_ID, first)).toBe(true);
			expect(await repositories.threads.claimForumPin(FORUM_ID, second)).toBe(false);
			await upsertOne(ThreadState.patchByPk({thread_id: first}, {flags: {kind: 'set', value: 0}}));
			expect(await repositories.threads.claimForumPin(FORUM_ID, second)).toBe(true);
			expect(await repositories.threads.getForumPin(FORUM_ID)).toBe(second);
		});

		it('pages archived threads newest first with before cursors and limits 2 and 100', async () => {
			const ids: Array<ChannelID> = [];
			for (let i = 0; i < 5; i++) {
				const threadId = freshThreadId();
				ids.push(threadId);
				await repositories.threads.create(createParams(threadId));
				await repositories.threads.updateState(threadId, () => ({
					archived: true,
					archive_timestamp: new Date(1_700_000_000_000 + i * 1000),
				}));
			}
			const newestFirst = [...ids].reverse();
			const first = await repositories.threads.listArchivedThreads(PARENT_ID, false, {limit: 2});
			expect(first.threads.map((t) => t.threadId)).toEqual(newestFirst.slice(0, 2));
			expect(first.hasMore).toBe(true);
			const second = await repositories.threads.listArchivedThreads(PARENT_ID, false, {
				limit: 2,
				before: first.threads[1]!.archiveTimestamp!,
			});
			expect(second.threads.map((t) => t.threadId)).toEqual(newestFirst.slice(2, 4));
			expect(second.hasMore).toBe(true);
			const all = await repositories.threads.listArchivedThreads(PARENT_ID, false, {limit: 100});
			expect(all.threads.map((t) => t.threadId)).toEqual(newestFirst);
			expect(all.hasMore).toBe(false);
			expect((await repositories.threads.listArchivedThreads(PARENT_ID, true, {limit: 100})).threads).toEqual([]);
		});

		it('pages past stale rows when several archived threads share one timestamp', async () => {
			const tied = new Date(1_700_000_000_000);
			const ids: Array<ChannelID> = [];
			for (let i = 0; i < 4; i++) {
				const threadId = freshThreadId();
				ids.push(threadId);
				await repositories.threads.create(createParams(threadId));
				await repositories.threads.updateState(threadId, () => ({archived: true, archive_timestamp: tied}));
			}
			const older = freshThreadId();
			await repositories.threads.create(createParams(older));
			await repositories.threads.updateState(older, () => ({
				archived: true,
				archive_timestamp: new Date(tied.getTime() - 1000),
			}));
			for (const offset of [1n, 2n]) {
				await upsertOne(
					ArchivedThreadsByParent.upsertAll({
						parent_id: PARENT_ID,
						is_private: false,
						archive_timestamp: tied,
						thread_id: createChannelID(ids[3]! + 1000n * offset),
						guild_id: GUILD_ID,
					}),
				);
			}
			const newestFirst = [...ids].reverse();
			const first = await repositories.threads.listArchivedThreads(PARENT_ID, false, {limit: 2});
			expect(first.threads.map((t) => t.threadId)).toEqual(newestFirst.slice(0, 2));
			expect(first.hasMore).toBe(true);
			const all = await repositories.threads.listArchivedThreads(PARENT_ID, false, {limit: 4});
			expect(all.threads.map((t) => t.threadId)).toEqual(newestFirst);
			expect(all.hasMore).toBe(true);
		});

		it('lists only joined private archived threads, newest id first', async () => {
			const member = createUserID(1_900_000_000_000_002_000n);
			const privateIds: Array<ChannelID> = [];
			for (let i = 0; i < 3; i++) {
				const threadId = freshThreadId();
				privateIds.push(threadId);
				const params = createParams(threadId, {members: [{userId: member, flags: 1}]});
				params.channel.type = ChannelTypes.PRIVATE_THREAD;
				await repositories.threads.create(params);
				await repositories.threads.updateState(threadId, () => ({archived: true}));
			}
			const publicId = freshThreadId();
			await repositories.threads.create(createParams(publicId, {members: [{userId: member, flags: 1}]}));
			await repositories.threads.updateState(publicId, () => ({archived: true}));
			executor.statements.length = 0;
			const page = await repositories.threads.listJoinedPrivateArchivedThreads(member, GUILD_ID, PARENT_ID, {limit: 2});
			expect(page.threads.map((t) => t.threadId)).toEqual([privateIds[2], privateIds[1]]);
			expect(page.hasMore).toBe(true);
			const next = await repositories.threads.listJoinedPrivateArchivedThreads(member, GUILD_ID, PARENT_ID, {
				limit: 2,
				before: privateIds[1],
			});
			expect(next.threads.map((t) => t.threadId)).toEqual([privateIds[0]]);
			expect(next.hasMore).toBe(false);
			expect(page.threads.every((t) => t.isPrivate)).toBe(true);
		});

		it('adds a 250 member batch with one state compare-and-set', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId));
			executor.statements.length = 0;
			const members = Array.from({length: 250}, (_, index) => ({
				userId: createUserID(1_900_000_000_010_000_000n + BigInt(index)),
				flags: 0,
			}));
			const result = await repositories.threads.addMembers(threadId, members);
			expect(result?.added).toHaveLength(250);
			expect(result?.state.memberCount).toBe(251);
			expect(result?.state.memberIdsPreview).toHaveLength(8);
			expect(result?.state.memberIdsPreview[0]).toBe(members[249]!.userId);
			expect(executor.count('cas:thread_state')).toBe(1);
			expect(executor.count('cas:thread_members')).toBe(1);
			const again = await repositories.threads.addMembers(threadId, members.slice(0, 3));
			expect(again?.added).toEqual([]);
			expect((await repositories.threads.listMembers(threadId, {limit: 1000})).length).toBe(251);
		});

		it('writes creator memberships and the seeded stamp only under compare-and-set', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(
				createParams(threadId, {
					members: [
						{userId: OWNER_ID, flags: 1},
						{userId: OWNER_ID, flags: 1},
					],
				}),
			);
			expect(executor.count('cas:thread_members')).toBe(1);
			expect(executor.statements.some((entry) => entry.endsWith(':thread_members') && !entry.startsWith('cas:'))).toBe(
				false,
			);
			expect((await repositories.threads.listMembers(threadId, {limit: 10})).map((m) => m.userId)).toEqual([OWNER_ID]);
			const guildId = createGuildID(1_900_000_000_000_000_777n);
			await repositories.threads.ensureGuildMarker(guildId);
			expect((await repositories.threads.getGuildMarker(guildId))?.perms_seeded_at ?? null).toBeNull();
			executor.statements.length = 0;
			const seededAt = new Date(1_800_000_000_000);
			await repositories.threads.markGuildPermsSeeded(guildId, seededAt);
			await repositories.threads.markGuildPermsSeeded(guildId, new Date());
			expect((await repositories.threads.getGuildMarker(guildId))?.perms_seeded_at).toEqual(seededAt);
			expect(
				executor.statements.filter((entry) => entry.endsWith(':guild_thread_state') && !entry.startsWith('select:')),
			).toEqual(['cas:guild_thread_state']);
			const fresh = createGuildID(1_900_000_000_000_000_778n);
			await repositories.threads.markGuildPermsSeeded(fresh, seededAt);
			expect((await repositories.threads.getGuildMarker(fresh))?.perms_seeded_at).toEqual(seededAt);
		});

		it('marks search backfill on a full marker row and clears it for a reindex', async () => {
			const guildId = createGuildID(1_900_000_000_000_000_779n);
			const at = new Date(1_800_000_000_000);
			await repositories.threads.markGuildSearchBackfilled(guildId, at);
			const marker = await repositories.threads.getGuildMarker(guildId);
			expect(marker?.first_active_at).toBeInstanceOf(Date);
			expect(marker?.search_backfilled_at).toEqual(at);
			await repositories.threads.clearGuildSearchBackfilled(guildId);
			const cleared = await repositories.threads.getGuildMarker(guildId);
			expect(cleared?.search_backfilled_at ?? null).toBeNull();
			expect(cleared?.first_active_at).toEqual(marker?.first_active_at);
		});

		it('refuses joins past the member cap', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId));
			await upsertOne(
				ThreadState.patchByPk({thread_id: threadId}, {member_count: {kind: 'set', value: MAX_THREAD_MEMBERS - 1}}),
			);
			await expect(
				repositories.threads.addMembers(threadId, [
					{userId: createUserID(1_900_000_000_020_000_001n), flags: 0},
					{userId: createUserID(1_900_000_000_020_000_002n), flags: 0},
				]),
			).rejects.toBeInstanceOf(MaxThreadMembersError);
			const single = await repositories.threads.addMembers(threadId, [
				{userId: createUserID(1_900_000_000_020_000_003n), flags: 0},
			]);
			expect(single?.state.memberCount).toBe(MAX_THREAD_MEMBERS);
		});

		it('lets only one of two concurrent joins take the last member slot', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId));
			await upsertOne(
				ThreadState.patchByPk({thread_id: threadId}, {member_count: {kind: 'set', value: MAX_THREAD_MEMBERS - 1}}),
			);
			const joiners = [createUserID(1_900_000_000_025_000_001n), createUserID(1_900_000_000_025_000_002n)];
			const results = await Promise.allSettled(
				joiners.map((userId) => repositories.threads.addMembers(threadId, [{userId, flags: 0}])),
			);
			expect(results.filter((result) => result.status === 'fulfilled')).toHaveLength(1);
			const rejected = results.find((result) => result.status === 'rejected') as PromiseRejectedResult;
			expect(rejected.reason).toBeInstanceOf(MaxThreadMembersError);
			expect((await repositories.threads.getState(threadId))?.memberCount).toBe(MAX_THREAD_MEMBERS);
			expect(await repositories.threads.listMembers(threadId, {limit: 100})).toHaveLength(2);
		});

		it('keeps the member count consistent under concurrent adds and removes', async () => {
			const threadId = freshThreadId();
			const leaving = createUserID(1_900_000_000_030_000_001n);
			await repositories.threads.create(
				createParams(threadId, {
					members: [
						{userId: OWNER_ID, flags: 1},
						{userId: leaving, flags: 0},
					],
				}),
			);
			const joiners = [createUserID(1_900_000_000_030_000_002n), createUserID(1_900_000_000_030_000_003n)];
			await Promise.all([
				repositories.threads.addMembers(
					threadId,
					joiners.map((userId) => ({userId, flags: 0})),
				),
				repositories.threads.removeMembers(threadId, [leaving]),
				repositories.threads.removeMembers(threadId, [leaving]),
			]);
			const state = await repositories.threads.getState(threadId);
			const rows = await repositories.threads.listMembers(threadId, {limit: 100});
			expect(rows.map((row) => row.userId).sort()).toEqual([OWNER_ID, ...joiners].sort());
			expect(state?.memberCount).toBe(3);
			expect(state?.memberIdsPreview).not.toContain(leaving);
			expect(await repositories.threads.listJoinedThreadIds(leaving, GUILD_ID)).toEqual([]);
		});

		it('rolls back member rows when the member count compare-and-set gives up', async () => {
			const threadId = freshThreadId();
			const staying = createUserID(1_900_000_000_040_000_001n);
			const joining = createUserID(1_900_000_000_040_000_002n);
			await repositories.threads.create(
				createParams(threadId, {
					members: [
						{userId: OWNER_ID, flags: 1},
						{userId: staying, flags: 0},
					],
				}),
			);
			const executeQuery = executor.executeQuery.bind(executor);
			const spy = vi.spyOn(executor, 'executeQuery').mockImplementation(async (query) => {
				const meta = query.kvMeta;
				if (meta?.table.name === 'thread_state' && meta.conditions) {
					return [{'[applied]': false}] as never;
				}
				return executeQuery(query);
			});
			await expect(repositories.threads.addMembers(threadId, [{userId: joining, flags: 0}])).rejects.toThrow();
			await expect(repositories.threads.removeMembers(threadId, [staying])).rejects.toThrow();
			spy.mockRestore();
			const rows = await repositories.threads.listMembers(threadId, {limit: 100});
			expect(rows.map((row) => row.userId).sort()).toEqual([OWNER_ID, staying].sort());
			expect((await repositories.threads.getState(threadId))?.memberCount).toBe(2);
			const joined = await repositories.threads.addMembers(threadId, [{userId: joining, flags: 0}]);
			expect(joined?.added.map((member) => member.userId)).toEqual([joining]);
			expect(joined?.state.memberCount).toBe(3);
			expect(await repositories.threads.listJoinedThreadIds(joining, GUILD_ID)).toEqual([threadId]);
			const left = await repositories.threads.removeMembers(threadId, [staying]);
			expect(left.removed.map((member) => member.userId)).toEqual([staying]);
			expect(left.state?.memberCount).toBe(2);
		});

		it('counts inserted thread messages only, excludes the starter id and floors at zero', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId));
			await repositories.channelData.updateLastMessageId(threadId, channelIdToMessageId(threadId), {isInsert: true});
			expect((await repositories.threads.getStats(threadId)).messageCount).toBe(0);
			await repositories.channelData.updateLastMessageId(threadId, createMessageID(threadId + 10n));
			expect((await repositories.threads.getStats(threadId)).messageCount).toBe(0);
			await repositories.channelData.updateLastMessageId(threadId, createMessageID(threadId + 20n), {isInsert: true});
			await repositories.channelData.updateLastMessageId(threadId, createMessageID(threadId + 30n), {isInsert: true});
			const stats = await repositories.threads.getStats(threadId);
			expect(stats.messageCount).toBe(2);
			expect(stats.totalMessageSent).toBe(2);
			await repositories.threads.adjustMessageCount(threadId, -5);
			const floored = await repositories.threads.getStats(threadId);
			expect(floored.messageCount).toBe(0);
			expect(floored.totalMessageSent).toBe(2);
			executor.statements.length = 0;
			await repositories.channelData.updateLastMessageId(PARENT_ID, createMessageID(threadId + 40n), {isInsert: true});
			expect(executor.count('select:thread_stats')).toBe(0);
		});

		it('keeps thread message counters exact under concurrent sends and deletes', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId));
			await Promise.all(
				Array.from({length: 8}, (_, index) =>
					repositories.channelData.updateLastMessageId(threadId, createMessageID(threadId + BigInt(index + 1)), {
						isInsert: true,
					}),
				),
			);
			const sent = await repositories.threads.getStats(threadId);
			expect(sent.messageCount).toBe(8);
			expect(sent.totalMessageSent).toBe(8);
			await Promise.all([
				repositories.threads.adjustMessageCount(threadId, -1),
				repositories.threads.adjustMessageCount(threadId, -1),
				repositories.channelData.updateLastMessageId(threadId, createMessageID(threadId + 100n), {isInsert: true}),
			]);
			const mixed = await repositories.threads.getStats(threadId);
			expect(mixed.messageCount).toBe(7);
			expect(mixed.totalMessageSent).toBe(9);
		});

		it('enumerates guild threads with and without an activity cutoff', async () => {
			await taint();
			const oldThread = freshThreadId(-86_400_000);
			const newThread = freshThreadId();
			await repositories.threads.create(createParams(oldThread));
			await repositories.threads.create(createParams(newThread));
			await repositories.channelData.updateLastMessageId(
				newThread,
				createMessageID(createSnowflakeFromTimestamp(Date.now())),
			);
			expect((await repositories.threads.listGuildThreadIds(GUILD_ID)).sort()).toEqual([oldThread, newThread].sort());
			expect(
				await repositories.threads.listGuildThreadIds(GUILD_ID, {activeSince: new Date(Date.now() - 3_600_000)}),
			).toEqual([newThread]);
		});

		it('does no thread IO for never-enabled guilds', async () => {
			const threadId = freshThreadId();
			await upsertOne(
				ThreadsByParent.upsertAll({parent_id: PARENT_ID, thread_id: threadId, guild_id: GUILD_ID, type: 11}),
			);
			executor.statements.length = 0;
			expect(await repositories.threads.listGuildThreadIds(GUILD_ID)).toEqual([]);
			expect(executor.statements).toEqual([]);
			await repositories.channelData.listGuildChannels(GUILD_ID, 'maintenance');
			await repositories.channelData.listGuildChannels(GUILD_ID, 'enrolled');
			expect(executor.statements.every((entry) => /:(channels|channels_by_guild_id)$/.test(entry))).toBe(true);
			expect(executor.count('select:guild_thread_state')).toBe(0);
		});

		it('indexes forums separately and merges them only for active or tainted guilds', async () => {
			await repositories.channelData.upsert(channelRow(FORUM_ID, ChannelTypes.GUILD_FORUM));
			expect(
				await fetchMany(ThreadOnlyChannelsByGuild.selectCql({where: ThreadOnlyChannelsByGuild.where.eq('guild_id')}), {
					guild_id: GUILD_ID,
				}),
			).toHaveLength(1);
			expect((await repositories.channelData.listGuildChannels(GUILD_ID, 'enrolled')).map((c) => c.id)).toEqual([
				PARENT_ID,
			]);
			expect((await repositories.channelData.listGuildChannels(GUILD_ID, 'maintenance')).map((c) => c.id)).toEqual([
				PARENT_ID,
			]);
			await taint();
			expect(
				new Set((await repositories.channelData.listGuildChannels(GUILD_ID, 'enrolled')).map((c) => c.id)),
			).toEqual(new Set([PARENT_ID, FORUM_ID]));
			setThreadsConfig({enabled: false, ever_enabled: true});
			expect((await repositories.channelData.listGuildChannels(GUILD_ID, 'enrolled')).map((c) => c.id)).toEqual([
				PARENT_ID,
			]);
			expect(
				new Set((await repositories.channelData.listGuildChannels(GUILD_ID, 'maintenance')).map((c) => c.id)),
			).toEqual(new Set([PARENT_ID, FORUM_ID]));
			await repositories.channelData.delete(FORUM_ID, GUILD_ID, ChannelTypes.GUILD_FORUM);
			expect(
				await fetchMany(ThreadOnlyChannelsByGuild.selectCql({where: ThreadOnlyChannelsByGuild.where.eq('guild_id')}), {
					guild_id: GUILD_ID,
				}),
			).toEqual([]);
		});

		it('purges every thread row with threads_by_parent last', async () => {
			const threadId = freshThreadId();
			await repositories.threads.create(createParams(threadId, {members: [{userId: OWNER_ID, flags: 1}]}));
			await repositories.threads.updateState(threadId, () => ({archived: true}));
			executor.statements.length = 0;
			await repositories.threads.purgeThread(threadId);
			expect(executor.statements.at(-1)).toBe('delete:threads_by_parent');
			expect(await repositories.threads.getState(threadId)).toBeNull();
			expect(await repositories.channelData.findUnique(threadId)).toBeNull();
			expect(await repositories.threads.listMembers(threadId, {limit: 10})).toEqual([]);
			expect(await repositories.threads.listJoinedThreadIds(OWNER_ID, GUILD_ID)).toEqual([]);
			expect((await repositories.threads.listArchivedThreads(PARENT_ID, false, {limit: 10})).threads).toEqual([]);
			expect(await repositories.threads.listThreadIdsByParent(PARENT_ID, {limit: 10})).toEqual([]);
		});

		it('stores parent config through plain patches', async () => {
			await repositories.threads.patchParentConfig(GUILD_ID, FORUM_ID, {
				flags: ChannelFlags.REQUIRE_TAG,
				available_tags: [{id: 5n, name: 'bug', moderated: false, emoji_id: null, emoji_name: '🐛'}],
				default_sort_order: 1,
			});
			const config = await repositories.threads.getParentConfig(GUILD_ID, FORUM_ID);
			expect(config?.flags).toBe(ChannelFlags.REQUIRE_TAG);
			expect(config?.availableTags.map((tag) => tag.toUdt())).toEqual([
				{id: 5n, name: 'bug', moderated: false, emoji_id: null, emoji_name: '🐛'},
			]);
			await repositories.threads.patchParentConfig(GUILD_ID, FORUM_ID, {available_tags: []});
			expect((await repositories.threads.getParentConfig(GUILD_ID, FORUM_ID))?.availableTags).toEqual([]);
			expect(await repositories.threads.listParentConfigs(GUILD_ID)).toHaveLength(1);
			await repositories.threads.deleteParentConfig(GUILD_ID, FORUM_ID);
			expect(
				await fetchOne(
					ThreadParentConfig.selectCql({
						where: [ThreadParentConfig.where.eq('guild_id'), ThreadParentConfig.where.eq('channel_id')],
					}),
					{guild_id: GUILD_ID, channel_id: FORUM_ID},
				),
			).toBeNull();
		});
	});
}

const THREAD_TABLES = [
	ThreadState,
	ThreadStats,
	ThreadsByParent,
	ActiveThreadsByGuild,
	ArchivedThreadsByParent,
	ThreadMembers,
	ThreadMembersByUser,
	ThreadParentConfig,
	ForumPinnedThread,
	ThreadOnlyChannelsByGuild,
	GuildThreadState,
];

describe('thread storage leaves control storage untouched', () => {
	it('keeps the channel and message column lists byte-identical', () => {
		expect([...CHANNEL_COLUMNS]).toEqual([
			'channel_id',
			'guild_id',
			'type',
			'name',
			'topic',
			'icon_hash',
			'url',
			'parent_id',
			'position',
			'owner_id',
			'recipient_ids',
			'nsfw',
			'content_warning_level',
			'content_warning_text',
			'rate_limit_per_user',
			'bitrate',
			'user_limit',
			'voice_connection_limit',
			'rtc_region',
			'last_message_id',
			'last_pin_timestamp',
			'permission_overwrites',
			'nicks',
			'soft_deleted',
			'indexed_at',
			'version',
		]);
		expect([...MESSAGE_COLUMNS]).toEqual([
			'channel_id',
			'bucket',
			'message_id',
			'author_id',
			'type',
			'webhook_id',
			'webhook_name',
			'webhook_avatar_hash',
			'content',
			'edited_timestamp',
			'pinned_timestamp',
			'flags',
			'mention_everyone',
			'mention_users',
			'mention_roles',
			'mention_channels',
			'attachments',
			'embeds',
			'sticker_items',
			'message_reference',
			'message_snapshots',
			'call',
			'has_reaction',
			'version',
		]);
	});

	it('declares every thread table in the target schema with the same columns and no TTL', () => {
		const schema = JSON.parse(
			readFileSync(
				fileURLToPath(new URL('../../../../../tools/dev/cassandra_target_schema.json', import.meta.url)),
				'utf8',
			),
		) as {tables: Array<{name: string; columns: Array<{name: string}>; options: string}>};
		for (const table of THREAD_TABLES) {
			const declared = schema.tables.find((entry) => entry.name === table.name);
			expect(declared, table.name).toBeDefined();
			expect(new Set(declared!.columns.map((column) => column.name))).toEqual(new Set(table.columns));
			expect(declared!.options).not.toContain('default_time_to_live');
			expect(table.defaultTtlSeconds).toBeUndefined();
		}
	});
});

describeThreadRepository('in-memory cassandra', async () => new InMemoryCassandraQueryExecutor());

const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;
const KV_TABLE = 'kv_thread_repository';
const CONTAINER = `fluxer-threads-${process.pid.toString(36)}-${Date.now().toString(36)}`;

async function freePort(): Promise<number> {
	return new Promise((resolve, reject) => {
		const server = createServer();
		server.on('error', reject);
		server.listen(0, '127.0.0.1', () => {
			const address = server.address();
			if (typeof address === 'string' || address === null) {
				reject(new Error('no port'));
				return;
			}
			const port = address.port;
			server.close(() => resolve(port));
		});
	});
}

describe.skipIf(!dockerAvailable)('ThreadRepository against postgres kv', () => {
	beforeAll(async () => {
		const port = await freePort();
		startDockerContainer([
			'run',
			'-d',
			'--name',
			CONTAINER,
			'-e',
			'POSTGRES_USER=fluxer',
			'-e',
			'POSTGRES_PASSWORD=fluxer',
			'-e',
			'POSTGRES_DB=fluxer',
			'-p',
			`127.0.0.1:${port}:5432`,
			'postgres:16-alpine',
			'-c',
			'fsync=off',
		]);
		let ready = false;
		for (let attempt = 0; attempt < 180 && !ready; attempt += 1) {
			await new Promise((resolve) => setTimeout(resolve, 500));
			const probe = spawnSync('docker', ['exec', CONTAINER, 'pg_isready', '-U', 'fluxer', '-d', 'fluxer'], {
				stdio: 'ignore',
			});
			if (probe.status !== 0) continue;
			try {
				await initPostgres({
					url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`,
					maxConnections: 8,
					kvTable: KV_TABLE,
				});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('postgres never came up');
		await ensurePostgresKvSchema(getDefaultPostgresClient());
	}, 900_000);

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	describeThreadRepository('postgres kv', async () => {
		const client = getDefaultPostgresClient();
		await client.query(`DELETE FROM ${KV_TABLE}`);
		return new PostgresKvQueryExecutor(client);
	});
});
