// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {type ChannelID, createChannelID, createGuildID, createMessageID} from '../../BrandedTypes';
import {setCassandraQueryExecutorForTesting, upsertOne} from '../../database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '../../database/CassandraTypes';
import type {ChannelRow} from '../../database/types/ChannelTypes';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import {Channel} from '../../models/Channel';
import {Channels} from '../../Tables';
import {InMemoryCassandraQueryExecutor} from '../../test/InMemoryCassandraQueryExecutor';
import {ChannelDataRepository} from './ChannelDataRepository';

class RecordingCassandraQueryExecutor {
	readonly statements: Array<string> = [];
	private readonly inner = new InMemoryCassandraQueryExecutor();

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		this.record(query.kvMeta);
		return this.inner.executeQuery<T>(query);
	}

	async executeBatch(queries: Array<{query: string; params: object; meta?: KvQueryMeta}>): Promise<void> {
		for (const entry of queries) {
			this.record(entry.meta);
		}
		await this.inner.executeBatch(queries);
	}

	countStatements(statement: string): number {
		return this.statements.filter((entry) => entry === statement).length;
	}

	private record(meta: KvQueryMeta | undefined): void {
		if (!meta) return;
		this.statements.push(`${meta.action}:${meta.table.name}`);
	}
}

function makeChannelRow(channelId: ChannelID): ChannelRow {
	return {
		channel_id: channelId,
		guild_id: createGuildID(1810000000000000000n),
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
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 1,
	};
}

let executor: RecordingCassandraQueryExecutor;

describe('ChannelDataRepository request cache', () => {
	beforeEach(() => {
		executor = new RecordingCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});
	it('reads the channel row once per lookup without a request cache', async () => {
		const channelId = createChannelID(1910000000000000000n);
		await upsertOne(Channels.upsertAll(makeChannelRow(channelId)));
		executor.statements.length = 0;
		const repository = new ChannelDataRepository();

		await repository.findUnique(channelId);
		await repository.findUnique(channelId);

		expect(executor.countStatements('select:channels')).toBe(2);
	});
	it('serves the prefetched channel once and falls back to the database afterwards', async () => {
		const channelId = createChannelID(1910000000000000000n);
		const row = makeChannelRow(channelId);
		await upsertOne(Channels.upsertAll(row));
		executor.statements.length = 0;
		const requestCache = createRequestCache();
		requestCache.channels.set(channelId, new Channel(row));
		const repository = new ChannelDataRepository(requestCache);

		const prefetched = await repository.findUnique(channelId);
		expect(prefetched?.id).toBe(channelId);
		expect(executor.countStatements('select:channels')).toBe(0);

		const reread = await repository.findUnique(channelId);
		expect(reread?.id).toBe(channelId);
		expect(executor.countStatements('select:channels')).toBe(1);
	});
	it('keeps a prefetched miss distinguishable from an absent entry', async () => {
		const channelId = createChannelID(1920000000000000000n);
		const requestCache = createRequestCache();
		requestCache.channels.set(channelId, null);
		const repository = new ChannelDataRepository(requestCache);

		expect(await repository.findUnique(channelId)).toBeNull();
		expect(executor.countStatements('select:channels')).toBe(0);
	});
	it('drops the prefetched channel when the same request writes it', async () => {
		const channelId = createChannelID(1930000000000000000n);
		const row = makeChannelRow(channelId);
		await upsertOne(Channels.upsertAll(row));
		executor.statements.length = 0;
		const requestCache = createRequestCache();
		requestCache.channels.set(channelId, null);
		const repository = new ChannelDataRepository(requestCache);

		await repository.updateLastMessageId(channelId, createMessageID(1940000000000000000n));

		expect(requestCache.channels.has(channelId)).toBe(false);
		expect(await repository.findUnique(channelId)).not.toBeNull();
	});
});
