// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	loadFixture,
	sendMessageWithAttachments,
	setupTestGuildAndChannel,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {createDmChannel, createFriendship, updateChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {TEST_EXPLICIT_MEDIA_MARKER} from '@app/api/test/TestMediaService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildExplicitContentFilterTypes} from '@fluxer/constants/src/GuildConstants';
import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const EXPLICIT_FILENAME = `${TEST_EXPLICIT_MEDIA_MARKER}.png`;
const CLEAN_FILENAME = 'clean.png';

describe('explicit media on messages', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function sendFile(account: TestAccount, channelId: string, filename: string) {
		return sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{content: 'media', attachments: [{id: 0, filename}]},
			[{index: 0, filename, data: loadFixture('yeah.png'), contentType: 'image/png'}],
		);
	}

	function fetchMessage(account: TestAccount, channelId: string, messageId: string) {
		return createBuilder<MessageResponse>(harness, account.token)
			.get(`/channels/${channelId}/messages/${messageId}`)
			.execute();
	}

	async function expectFlagged(account: TestAccount, channelId: string) {
		const sent = await sendFile(account, channelId, EXPLICIT_FILENAME);
		expect(sent.response.status).toBe(200);
		expect(sent.json.attachments?.[0]?.nsfw).toBe(true);
		const stored = await fetchMessage(account, channelId, sent.json.id);
		expect(stored.attachments?.[0]?.nsfw).toBe(true);
	}

	async function expectBlocked(account: TestAccount, channelId: string) {
		const sent = await sendFile(account, channelId, EXPLICIT_FILENAME);
		expect(sent.response.status).toBe(400);
		expect(sent.text).toContain(APIErrorCodes.EXPLICIT_CONTENT_CANNOT_BE_SENT);
	}

	it('flags and keeps explicit attachments in an 18+ channel', async () => {
		const {account, channel} = await setupTestGuildAndChannel(harness);
		await updateChannel(harness, account.token, channel.id, {nsfw: true});
		await expectFlagged(account, channel.id);
	});

	it('flags and keeps explicit attachments when the community filter is off', async () => {
		const {account, guild, channel} = await setupTestGuildAndChannel(harness);
		await updateGuild(harness, account.token, guild.id, {
			explicit_content_filter: GuildExplicitContentFilterTypes.DISABLED,
		});
		await expectFlagged(account, channel.id);
	});

	it('refuses explicit attachments when the community filter covers everyone', async () => {
		const {account, guild, channel} = await setupTestGuildAndChannel(harness);
		await updateGuild(harness, account.token, guild.id, {
			explicit_content_filter: GuildExplicitContentFilterTypes.ALL_MEMBERS,
		});
		await expectBlocked(account, channel.id);
	});

	it('records a clean verdict for clean attachments in a filtered community', async () => {
		const {account, guild, channel} = await setupTestGuildAndChannel(harness);
		await updateGuild(harness, account.token, guild.id, {
			explicit_content_filter: GuildExplicitContentFilterTypes.ALL_MEMBERS,
		});
		const sent = await sendFile(account, channel.id, CLEAN_FILENAME);
		expect(sent.response.status).toBe(200);
		expect(sent.json.attachments?.[0]?.nsfw).toBe(false);
	});

	it('flags explicit attachments between friends', async () => {
		const sender = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, sender, recipient);
		await ensureSessionStarted(harness, sender.token);
		const dm = await createDmChannel(harness, sender.token, recipient.userId);
		await expectFlagged(sender, dm.id);
	});

	it('refuses explicit attachments to a friend whose filter blocks them', async () => {
		const sender = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, sender, recipient);
		await createBuilder(harness, recipient.token)
			.patch('/users/@me/settings')
			.body({sensitive_content_friend_dm_filter: SensitiveMediaFilterLevel.BLOCK})
			.execute();
		await ensureSessionStarted(harness, sender.token);
		const dm = await createDmChannel(harness, sender.token, recipient.userId);
		await expectBlocked(sender, dm.id);
	});

	it('flags explicit embed images in an 18+ channel', async () => {
		const {account, channel} = await setupTestGuildAndChannel(harness);
		await updateChannel(harness, account.token, channel.id, {nsfw: true});
		const sent = await createBuilder<MessageResponse>(harness, account.token)
			.post(`/channels/${channel.id}/messages`)
			.body({
				content: 'embed',
				embeds: [{title: 'pic', image: {url: `https://images.example.com/${EXPLICIT_FILENAME}`}}],
			})
			.execute();
		const stored = await fetchMessage(account, channel.id, sent.id);
		expect(stored.embeds?.[0]?.nsfw).toBe(true);
	});

	it('refuses explicit embed images where the community filter blocks them', async () => {
		const {account, guild, channel} = await setupTestGuildAndChannel(harness);
		await updateGuild(harness, account.token, guild.id, {
			explicit_content_filter: GuildExplicitContentFilterTypes.ALL_MEMBERS,
		});
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages`)
			.body({
				content: 'embed',
				embeds: [{title: 'pic', image: {url: `https://images.example.com/${EXPLICIT_FILENAME}`}}],
			})
			.expect(400, APIErrorCodes.EXPLICIT_CONTENT_CANNOT_BE_SENT)
			.execute();
	});
});
