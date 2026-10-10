// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	channelIdToMessageId,
	createChannelID,
	createGuildID,
	createUserID,
} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/repositories/ChannelRepository';
import {setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import {
	KVThreadAutoArchiveQueueService,
	THREAD_ARCHIVE_GUILDS_KEY,
	threadArchiveQueueKey,
	threadAutoArchiveDueAt,
} from '@app/api/infrastructure/KVThreadAutoArchiveQueueService';
import {Channels} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelFlags} from '@fluxer/constants/src/ThreadConstants';
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';
import {beforeEach, describe, expect, it} from 'vitest';

const GUILD_ID = createGuildID(1_910_000_000_000_000_000n);
const PARENT_ID = createChannelID(1_910_000_000_000_000_100n);
const OWNER_ID = createUserID(1_910_000_000_000_001_000n);
const HOUR = 3_600_000;

function threadRow(threadId: ChannelID): ChannelRow {
	return {
		channel_id: threadId,
		guild_id: GUILD_ID,
		type: ChannelTypes.PUBLIC_THREAD,
		name: 'thread',
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: PARENT_ID,
		position: 0,
		owner_id: OWNER_ID,
		recipient_ids: null,
		nsfw: null,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: 0,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		rtc_p2p: null,
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 1,
	};
}

describe('KVThreadAutoArchiveQueueService', () => {
	let kv: MockKVProvider;
	let repositories: ChannelRepository;
	let queue: KVThreadAutoArchiveQueueService;
	let base: number;

	beforeEach(async () => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		kv = new MockKVProvider();
		repositories = new ChannelRepository();
		queue = new KVThreadAutoArchiveQueueService(kv);
		base = Date.now() - 10 * HOUR;
		await upsertOne(Channels.upsertAll({...threadRow(PARENT_ID), type: ChannelTypes.GUILD_TEXT, parent_id: null}));
	});

	async function createThread(offsetMs: number, duration: number, flags = 0) {
		const threadId = createChannelID(createSnowflakeFromTimestamp(base + offsetMs));
		return repositories.threads.create({
			channel: threadRow(threadId),
			parentType: ChannelTypes.GUILD_TEXT,
			autoArchiveDuration: duration,
			invitable: null,
			flags,
			appliedTags: [],
			hasStarter: false,
			createdAt: new Date(base + offsetMs),
			members: [{userId: OWNER_ID, flags: 1}],
		});
	}

	it('scores by the later of the last message and the archive timestamp plus the duration', async () => {
		const state = await createThread(0, 60);
		expect(threadAutoArchiveDueAt(state, null)).toBe(base + HOUR);
		const later = channelIdToMessageId(createChannelID(createSnowflakeFromTimestamp(base + 2 * HOUR)));
		expect(threadAutoArchiveDueAt(state, later)).toBe(base + 3 * HOUR);
	});

	it('returns due threads per guild and tracks the guild index', async () => {
		const due = await createThread(0, 60);
		const notDue = await createThread(1000, 10080);
		await queue.schedule(due, null);
		await queue.schedule(notDue, null);
		expect(await queue.listGuilds()).toEqual([GUILD_ID]);
		expect(await queue.getDue(GUILD_ID, Date.now(), 200)).toEqual([due.threadId]);
		await queue.remove(GUILD_ID, due.threadId);
		expect(await queue.getDue(GUILD_ID, Date.now(), 200)).toEqual([]);
		await queue.remove(GUILD_ID, notDue.threadId);
		expect(await kv.smembers(THREAD_ARCHIVE_GUILDS_KEY)).toEqual([]);
	});

	it('keeps the guild indexed when a schedule lands between the empty check and the index removal', async () => {
		const removed = await createThread(0, 60);
		const added = await createThread(1000, 60);
		await queue.schedule(removed, null);
		const srem = kv.srem.bind(kv);
		kv.srem = async (key, ...members) => {
			kv.srem = srem;
			await queue.schedule(added, null);
			return srem(key, ...members);
		};
		await queue.remove(GUILD_ID, removed.threadId);
		expect(await queue.listGuilds()).toEqual([GUILD_ID]);
		expect(await queue.getDue(GUILD_ID, Date.now(), 200)).toEqual([added.threadId]);
	});

	it('never schedules pinned or archived threads', async () => {
		const pinned = await createThread(0, 60, ChannelFlags.PINNED);
		await queue.schedule(pinned, null);
		expect(await kv.zcard(threadArchiveQueueKey(GUILD_ID))).toBe(0);
		const archived = await createThread(10, 60);
		const transition = await repositories.threads.updateState(archived.threadId, () => ({archived: true}));
		await queue.schedule(transition!.state, null);
		expect(await kv.zcard(threadArchiveQueueKey(GUILD_ID))).toBe(0);
	});
});
