// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {markGuildChannelsAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {MessageSearchResultsResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const FEATURES = 'X-Fluxer-Features';
const CAPABLE = 'channel_threads';

describe('message search thread permission bits', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
		clearChannelThreadsTaintCacheForTesting();
	});

	afterEach(async () => {
		clearChannelThreadsTaintCacheForTesting();
		await harness.shutdown();
	});

	test('search channel overwrites are masked for non-viewers in tainted guilds', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Search masking');
		const allow = Permissions.SEND_MESSAGES | ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
		const channel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.header(FEATURES, CAPABLE)
			.body({
				name: 'search-overwrites',
				type: ChannelTypes.GUILD_TEXT,
				permission_overwrites: [{id: guild.id, type: 0, allow: allow.toString(), deny: '0'}],
			})
			.execute();
		const marker = `thread-bits-${Date.now()}`;
		await sendMessage(harness, owner.token, channel.id, marker);
		await markGuildChannelsAsIndexed(harness, owner.token, guild.id);

		const searchAllow = async (body: Record<string, unknown>, capable: boolean) => {
			const builder = createBuilder<MessageSearchResultsResponse>(harness, owner.token)
				.post('/search/messages')
				.body({content: marker, ...body});
			const result = await (capable ? builder.header(FEATURES, CAPABLE) : builder).execute();
			const entry = result.channels.find((candidate) => candidate.id === channel.id);
			expect(entry).toBeDefined();
			return BigInt(entry?.permission_overwrites?.[0]?.allow ?? '0');
		};
		const scopes: Array<Record<string, unknown>> = [
			{context_channel_id: channel.id},
			{context_guild_id: guild.id},
			{scope: 'all_guilds'},
		];
		for (const scope of scopes) {
			expect(await searchAllow(scope, true)).toBe(allow);
			expect(await searchAllow(scope, false)).toBe(Permissions.SEND_MESSAGES);
		}
	});
});
