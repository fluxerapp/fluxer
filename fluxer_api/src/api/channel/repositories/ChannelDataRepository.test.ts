// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createGuildID, createMessageID} from '@app/api/BrandedTypes';
import {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import {setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {Channels} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

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
