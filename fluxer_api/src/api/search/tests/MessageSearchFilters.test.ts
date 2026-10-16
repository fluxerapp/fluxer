// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {markGuildChannelsAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, test} from 'vitest';

interface MessageSearchResult {
	messages: Array<{
		id: string;
		channel_id: string;
		content: string;
		author: {
			id: string;
			username: string;
			bot?: boolean;
		};
		timestamp: string;
		pinned?: boolean;
		mention_everyone?: boolean;
		mentions?: Array<{
			id: string;
		}>;
		attachments?: Array<{
			id: string;
			filename: string;
		}>;
		embeds?: Array<unknown>;
		stickers?: Array<{
			id: string;
		}>;
		message_snapshots?: Array<{
			content?: string | null;
			attachments?: Array<{
				id: string;
				filename: string;
			}>;
		}>;
	}>;
	total: number;
	hits_per_page: number;
	page: number;
}

interface SearchIndexingResponse {
	indexing: true;
}

type MessageSearchResponse = MessageSearchResult | SearchIndexingResponse;

describe('Message Search Filters', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('Channel Filtering', () => {
		test('channel_id outside the resolved scope returns 403', async () => {
			const account = await createTestAccount(harness);
			const outsider = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Channel Id Scope Guild');
			const outsideGuild = await createGuild(harness, outsider.token, 'Channel Id Outside Guild');
			const timestamp = Date.now();
			const baseContent = `channel-id-scope-${timestamp}`;
			await sendMessage(harness, account.token, guild.system_channel_id!, `${baseContent} own msg`);
			await sendMessage(harness, outsider.token, outsideGuild.system_channel_id!, `${baseContent} outside msg`);
			await markGuildChannelsAsIndexed(harness, account.token, guild.id);
			await markGuildChannelsAsIndexed(harness, outsider.token, outsideGuild.id);
			await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					scope: 'all_guilds',
					channel_id: [outsideGuild.system_channel_id!],
				})
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
	});
});
