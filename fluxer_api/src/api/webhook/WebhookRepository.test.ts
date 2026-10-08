// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createUserID,
	createWebhookID,
	createWebhookToken,
	type GuildID,
	type WebhookID,
} from '@app/api/BrandedTypes';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const SOURCE_GUILD = createGuildID(1n);
const SOURCE_CHANNEL = createChannelID(10n);

let executor: InMemoryCassandraQueryExecutor;
let repository: WebhookRepository;

async function createFollower(
	webhookId: WebhookID,
	guildId: GuildID,
	channelId: ChannelID,
	sourceChannelId: ChannelID = SOURCE_CHANNEL,
) {
	return repository.create({
		webhookId,
		token: createWebhookToken(`token-${webhookId}`),
		type: WebhookTypes.CHANNEL_FOLLOWER,
		guildId,
		channelId,
		creatorId: createUserID(7n),
		name: 'Source #news',
		avatarHash: null,
		sourceGuildId: SOURCE_GUILD,
		sourceChannelId,
	});
}

describe('WebhookRepository source channel index', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		repository = new WebhookRepository();
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('stores and returns the source columns of a follower webhook', async () => {
		const created = await createFollower(createWebhookID(100n), createGuildID(2n), createChannelID(20n));
		expect(created.sourceGuildId).toBe(1n);
		expect(created.sourceChannelId).toBe(10n);
		expect(created.type).toBe(WebhookTypes.CHANNEL_FOLLOWER);
		const found = await repository.findUnique(createWebhookID(100n));
		expect(found?.sourceGuildId).toBe(1n);
		expect(found?.sourceChannelId).toBe(10n);
		expect(found?.toRow().source_channel_id).toBe(10n);
	});

	it('leaves incoming webhooks out of the index with null source columns', async () => {
		const created = await repository.create({
			webhookId: createWebhookID(101n),
			token: createWebhookToken('incoming'),
			type: WebhookTypes.INCOMING,
			guildId: createGuildID(2n),
			channelId: SOURCE_CHANNEL,
			creatorId: createUserID(7n),
			name: 'Incoming',
			avatarHash: null,
		});
		expect(created.sourceGuildId).toBeNull();
		expect(created.sourceChannelId).toBeNull();
		expect(await repository.listIdsBySourceChannel(SOURCE_CHANNEL, {limit: 10})).toEqual([]);
		expect(await repository.countBySourceChannel(SOURCE_CHANNEL)).toEqual({channelCount: 0, guildCount: 0});
	});

	it('keeps the source columns across an update and a move', async () => {
		await createFollower(createWebhookID(100n), createGuildID(2n), createChannelID(20n));
		const updated = await repository.update(createWebhookID(100n), {
			name: 'Renamed',
			channelId: createChannelID(21n),
		});
		expect(updated?.name).toBe('Renamed');
		expect(updated?.sourceGuildId).toBe(1n);
		expect(updated?.sourceChannelId).toBe(10n);
		const found = await repository.findUnique(createWebhookID(100n));
		expect(found?.channelId).toBe(21n);
		expect(found?.sourceChannelId).toBe(10n);
		expect(await repository.listIdsBySourceChannel(SOURCE_CHANNEL, {limit: 10})).toEqual([
			{webhookId: 100n, guildId: 2n},
		]);
	});

	it('pages follower ids by source channel in webhook id order', async () => {
		await createFollower(createWebhookID(104n), createGuildID(3n), createChannelID(30n));
		await createFollower(createWebhookID(101n), createGuildID(2n), createChannelID(20n));
		await createFollower(createWebhookID(103n), createGuildID(2n), createChannelID(21n));
		await createFollower(createWebhookID(102n), createGuildID(4n), createChannelID(40n));
		await createFollower(createWebhookID(105n), createGuildID(2n), createChannelID(22n), createChannelID(11n));
		const first = await repository.listIdsBySourceChannel(SOURCE_CHANNEL, {limit: 3});
		expect(first).toEqual([
			{webhookId: 101n, guildId: 2n},
			{webhookId: 102n, guildId: 4n},
			{webhookId: 103n, guildId: 2n},
		]);
		const second = await repository.listIdsBySourceChannel(SOURCE_CHANNEL, {
			afterWebhookId: first[first.length - 1]!.webhookId,
			limit: 3,
		});
		expect(second).toEqual([{webhookId: 104n, guildId: 3n}]);
	});

	it('counts follower channels and distinct target guilds', async () => {
		await createFollower(createWebhookID(101n), createGuildID(2n), createChannelID(20n));
		await createFollower(createWebhookID(102n), createGuildID(2n), createChannelID(21n));
		await createFollower(createWebhookID(103n), createGuildID(3n), createChannelID(30n));
		expect(await repository.countBySourceChannel(SOURCE_CHANNEL)).toEqual({channelCount: 3, guildCount: 2});
	});

	it('removes the index row when a follower webhook is deleted', async () => {
		await createFollower(createWebhookID(101n), createGuildID(2n), createChannelID(20n));
		await createFollower(createWebhookID(102n), createGuildID(3n), createChannelID(30n));
		await repository.delete(createWebhookID(101n));
		expect(await repository.findUnique(createWebhookID(101n))).toBeNull();
		expect(await repository.listIdsBySourceChannel(SOURCE_CHANNEL, {limit: 10})).toEqual([
			{webhookId: 102n, guildId: 3n},
		]);
		expect(await repository.countBySourceChannel(SOURCE_CHANNEL)).toEqual({channelCount: 1, guildCount: 1});
	});

	it('finds many webhooks by id', async () => {
		await createFollower(createWebhookID(101n), createGuildID(2n), createChannelID(20n));
		await createFollower(createWebhookID(102n), createGuildID(3n), createChannelID(30n));
		const found = await repository.findManyByIds([createWebhookID(101n), createWebhookID(102n), createWebhookID(999n)]);
		expect(found.map((webhook) => webhook.id).sort()).toEqual([101n, 102n]);
		expect(await repository.findManyByIds([])).toEqual([]);
	});
});
