// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createGuildID, createMessageID, type GuildID} from '@app/api/BrandedTypes';
import {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import {setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {Channel} from '@app/api/models/Channel';
import {Channels, ChannelsByGuild} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const GUILD_ID: GuildID = createGuildID(1810000000000000000n);

function makeChannelRow(channelId: ChannelID): ChannelRow {
	return {
		channel_id: channelId,
		guild_id: GUILD_ID,
		type: ChannelTypes.GUILD_TEXT,
		name: 'general',
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

describe('ChannelDataRepository request cache', () => {
	beforeEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});
	afterEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});
	it('drops the prefetched channel when the same request writes it', async () => {
		const channelId = createChannelID(1930000000000000000n);
		const row = makeChannelRow(channelId);
		await upsertOne(Channels.upsertAll(row));
		const requestCache = createRequestCache();
		requestCache.channels.set(channelId, null);
		const repository = new ChannelDataRepository(requestCache);

		await repository.updateLastMessageId(channelId, createMessageID(1940000000000000000n));

		expect(requestCache.channels.has(channelId)).toBe(false);
		expect(await repository.findUnique(channelId)).not.toBeNull();
	});
});

describe('ChannelDataRepository writes after delete', () => {
	beforeEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});
	afterEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});

	async function seed(channelId: ChannelID): Promise<Channel> {
		const row = makeChannelRow(channelId);
		await upsertOne(Channels.upsertAll(row));
		await upsertOne(ChannelsByGuild.upsertAll({guild_id: GUILD_ID, channel_id: channelId}));
		return new Channel(row);
	}

	async function expectGone(repository: ChannelDataRepository, channelId: ChannelID): Promise<void> {
		expect(await repository.findUnique(channelId)).toBeNull();
		const listed = await repository.listGuildChannels(GUILD_ID, 'enrolled');
		expect(listed.map((channel) => channel.id)).not.toContain(channelId);
	}

	it('does not bring back a deleted channel from a stale copy', async () => {
		const channelId = createChannelID(1930000000000000001n);
		const repository = new ChannelDataRepository();
		const created = await seed(channelId);
		const stale = created.toRow();
		await repository.delete(channelId, GUILD_ID, ChannelTypes.GUILD_TEXT);

		await expect(
			repository.upsert({...stale, parent_id: createChannelID(1930000000000000009n), position: 4}, stale),
		).rejects.toBeInstanceOf(UnknownChannelError);
		await expect(repository.upsert(stale, stale)).rejects.toBeInstanceOf(UnknownChannelError);

		await expectGone(repository, channelId);
	});

	it('does not recreate a deleted channel when the update reads the row itself', async () => {
		const channelId = createChannelID(1930000000000000002n);
		const repository = new ChannelDataRepository();
		const created = await seed(channelId);
		await repository.delete(channelId, GUILD_ID, ChannelTypes.GUILD_TEXT);

		await expect(repository.upsert({...created.toRow(), position: 7})).rejects.toBeInstanceOf(UnknownChannelError);

		await expectGone(repository, channelId);
	});

	it('creates a channel with its guild index row', async () => {
		const channelId = createChannelID(1930000000000000005n);
		const repository = new ChannelDataRepository();

		const created = await repository.create(makeChannelRow(channelId));

		expect(created.version).toBe(1);
		expect((await repository.findUnique(channelId))?.type).toBe(ChannelTypes.GUILD_TEXT);
		const listed = await repository.listGuildChannels(GUILD_ID, 'enrolled');
		expect(listed.map((channel) => channel.id)).toContain(channelId);
	});

	it('updates a live channel and keeps it listed', async () => {
		const channelId = createChannelID(1930000000000000003n);
		const repository = new ChannelDataRepository();
		const created = await seed(channelId);

		const updated = await repository.upsert({...created.toRow(), position: 3}, created.toRow());

		expect(updated.version).toBe(2);
		const stored = await repository.findUnique(channelId);
		expect(stored?.position).toBe(3);
		expect(stored?.type).toBe(ChannelTypes.GUILD_TEXT);
		expect(stored?.guildId).toBe(GUILD_ID);
		const listed = await repository.listGuildChannels(GUILD_ID, 'enrolled');
		expect(listed.map((channel) => channel.id)).toContain(channelId);
	});

	it('changes the type of a live channel', async () => {
		const channelId = createChannelID(1930000000000000004n);
		const repository = new ChannelDataRepository();
		const created = await seed(channelId);

		await repository.upsert({...created.toRow(), type: ChannelTypes.GUILD_ANNOUNCEMENT}, created.toRow());

		expect((await repository.findUnique(channelId))?.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});
});
