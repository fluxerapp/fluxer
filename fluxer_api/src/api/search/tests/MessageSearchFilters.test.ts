// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {loadFixture, sendMessageWithAttachments} from '../../channel/tests/AttachmentTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	getChannel,
} from '../../guild/tests/GuildTestUtils';
import {
	markChannelAsIndexed,
	markGuildChannelsAsIndexed,
	pinMessage,
	sendMessage,
} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

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

function isSearchResult(response: MessageSearchResponse): response is MessageSearchResult {
	return 'messages' in response;
}

describe('Message Search Filters', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('Author Filtering', () => {
		test('author_id filter returns only messages from specified author', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Author Filter Guild');
			const channelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			const timestamp = Date.now();
			const baseContent = `author-filter-test-${timestamp}`;
			await sendMessage(harness, owner.token, channelId, `${baseContent} owner message 1`);
			await sendMessage(harness, member.token, channelId, `${baseContent} member message 1`);
			await sendMessage(harness, owner.token, channelId, `${baseContent} owner message 2`);
			await sendMessage(harness, member.token, channelId, `${baseContent} member message 2`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					author_id: [owner.userId],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result) && result.messages.length > 0) {
				for (const msg of result.messages) {
					expect(msg.author.id).toBe(owner.userId);
				}
			}
		});
		test('exclude_author_id filter excludes messages from specified author', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Exclude Author Guild');
			const channelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			const timestamp = Date.now();
			const baseContent = `exclude-author-test-${timestamp}`;
			await sendMessage(harness, owner.token, channelId, `${baseContent} owner msg`);
			await sendMessage(harness, member.token, channelId, `${baseContent} member msg`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					exclude_author_id: [owner.userId],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.author.id).not.toBe(owner.userId);
				}
				if (result.messages.length > 0) {
					expect(result.messages.some((m) => m.author.id === member.userId)).toBe(true);
				}
			}
		});
		test('exclude_author_type filter excludes specified author types', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Exclude Author Type Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `exclude-author-type-${timestamp}`;
			await sendMessage(harness, owner.token, channelId, `${baseContent} user message 1`);
			await sendMessage(harness, owner.token, channelId, `${baseContent} user message 2`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					exclude_author_type: ['bot'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result) && result.messages.length > 0) {
				for (const msg of result.messages) {
					expect(msg.author.bot).toBeFalsy();
				}
			}
		});
	});
	describe('Channel Filtering', () => {
		test('channel_id filter limits search to specific channels', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Channel Filter Guild');
			const channel1 = await getChannel(harness, account.token, guild.system_channel_id!);
			const channel2 = await createChannel(harness, account.token, guild.id, 'second-channel');
			const timestamp = Date.now();
			const baseContent = `channel-filter-${timestamp}`;
			await sendMessage(harness, account.token, channel1.id, `${baseContent} channel1 msg`);
			await sendMessage(harness, account.token, channel2.id, `${baseContent} channel2 msg`);
			await markChannelAsIndexed(harness, channel1.id);
			await markChannelAsIndexed(harness, channel2.id);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_guild_id: guild.id,
					channel_id: [channel1.id],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.channel_id).toBe(channel1.id);
				}
			}
		});
		test('exclude_channel_id excludes specific channels', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exclude Channel Guild');
			const channel1 = await getChannel(harness, account.token, guild.system_channel_id!);
			const channel2 = await createChannel(harness, account.token, guild.id, 'second-channel');
			const timestamp = Date.now();
			const baseContent = `exclude-channel-${timestamp}`;
			await sendMessage(harness, account.token, channel1.id, `${baseContent} channel1 msg`);
			await sendMessage(harness, account.token, channel2.id, `${baseContent} channel2 msg`);
			await markChannelAsIndexed(harness, channel1.id);
			await markChannelAsIndexed(harness, channel2.id);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_guild_id: guild.id,
					exclude_channel_id: [channel1.id],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.channel_id).not.toBe(channel1.id);
				}
			}
		});
		test('context_channel_id provides channel context for search', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Context Channel Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `context-channel-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} test message`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result) && result.messages.length > 0) {
				for (const msg of result.messages) {
					expect(msg.channel_id).toBe(channelId);
				}
			}
		});
		test('context_guild_id limits search to specific guild', async () => {
			const account = await createTestAccount(harness);
			const guild1 = await createGuild(harness, account.token, 'Guild One');
			const guild2 = await createGuild(harness, account.token, 'Guild Two');
			const channel1 = guild1.system_channel_id!;
			const channel2 = guild2.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `context-guild-${timestamp}`;
			await sendMessage(harness, account.token, channel1, `${baseContent} guild1 msg`);
			await sendMessage(harness, account.token, channel2, `${baseContent} guild2 msg`);
			await markChannelAsIndexed(harness, channel1);
			await markChannelAsIndexed(harness, channel2);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_guild_id: guild1.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.channel_id).toBe(channel1);
				}
			}
		});
		test('channel_ids array limits to multiple specific channels', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Multi Channel Guild');
			const channel1 = await getChannel(harness, account.token, guild.system_channel_id!);
			const channel2 = await createChannel(harness, account.token, guild.id, 'channel-two');
			const channel3 = await createChannel(harness, account.token, guild.id, 'channel-three');
			const timestamp = Date.now();
			const baseContent = `multi-channel-${timestamp}`;
			await sendMessage(harness, account.token, channel1.id, `${baseContent} ch1 msg`);
			await sendMessage(harness, account.token, channel2.id, `${baseContent} ch2 msg`);
			await sendMessage(harness, account.token, channel3.id, `${baseContent} ch3 msg`);
			await markChannelAsIndexed(harness, channel1.id);
			await markChannelAsIndexed(harness, channel2.id);
			await markChannelAsIndexed(harness, channel3.id);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_guild_id: guild.id,
					channel_ids: [channel1.id, channel2.id],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				const allowedChannels = [channel1.id, channel2.id];
				for (const msg of result.messages) {
					expect(allowedChannels).toContain(msg.channel_id);
				}
				expect(result.messages.every((m) => m.channel_id !== channel3.id)).toBe(true);
			}
		});
		test('channel_id narrows scope like channel_ids', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Channel Id Alias Guild');
			const channel1 = await getChannel(harness, account.token, guild.system_channel_id!);
			const channel2 = await createChannel(harness, account.token, guild.id, 'alias-second-channel');
			const timestamp = Date.now();
			const baseContent = `channel-id-alias-${timestamp}`;
			await sendMessage(harness, account.token, channel1.id, `${baseContent} channel1 msg`);
			await sendMessage(harness, account.token, channel2.id, `${baseContent} channel2 msg`);
			await markGuildChannelsAsIndexed(harness, account.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					scope: 'all_guilds',
					channel_id: [channel1.id],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBeGreaterThan(0);
			for (const msg of result.messages) {
				expect(msg.channel_id).toBe(channel1.id);
			}
			expect(result.messages.every((m) => m.channel_id !== channel2.id)).toBe(true);
		});
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
	describe('Content Type Filtering (has/exclude_has)', () => {
		test('has: link finds messages with links', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has Link Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `has-link-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} no link here`);
			await sendMessage(harness, account.token, channelId, `${baseContent} check out https://example.com`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					has: ['link'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.content).toMatch(/https?:\/\//);
				}
			}
		});
		test('has: attachment finds messages with attachments', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has Attachment Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `has-attachment-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} no attachment`);
			let testFixture: Buffer;
			try {
				testFixture = loadFixture('test.png');
			} catch {
				testFixture = Buffer.from('fake image data');
			}
			await sendMessageWithAttachments(
				harness,
				account.token,
				channelId,
				{
					content: `${baseContent} with attachment`,
					attachments: [{id: 0, filename: 'test.png'}],
				},
				[{index: 0, filename: 'test.png', data: testFixture}],
			);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					has: ['file'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.attachments).toBeDefined();
					expect(msg.attachments!.length).toBeGreaterThan(0);
				}
			}
		});
		test('has: embed finds messages with embeds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has Embed Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `has-embed-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} no embed`);
			await createBuilder<MessageResponse>(harness, account.token)
				.post(`/channels/${channelId}/messages`)
				.body({
					content: `${baseContent} with embed`,
					embeds: [
						{
							title: 'Test Embed',
							description: 'This is a test embed',
						},
					],
				})
				.execute();
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					has: ['embed'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.embeds).toBeDefined();
					expect(msg.embeds!.length).toBeGreaterThan(0);
				}
			}
		});
		test('has: image finds messages with images', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has Image Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `has-image-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} no image`);
			let imageFixture: Buffer;
			try {
				imageFixture = loadFixture('test.png');
			} catch {
				imageFixture = Buffer.from('PNG fake image data');
			}
			await sendMessageWithAttachments(
				harness,
				account.token,
				channelId,
				{
					content: `${baseContent} with image`,
					attachments: [{id: 0, filename: 'image.png'}],
				},
				[{index: 0, filename: 'image.png', data: imageFixture}],
			);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					has: ['image'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.attachments).toBeDefined();
					const hasImage = msg.attachments!.some(
						(a) => a.filename.endsWith('.png') || a.filename.endsWith('.jpg') || a.filename.endsWith('.gif'),
					);
					expect(hasImage).toBe(true);
				}
			}
		});
		test('has: video finds messages with videos', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has Video Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `has-video-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} no video`);
			let videoFixture: Buffer;
			try {
				videoFixture = loadFixture('test.mp4');
			} catch {
				videoFixture = Buffer.from('fake video data');
			}
			await sendMessageWithAttachments(
				harness,
				account.token,
				channelId,
				{
					content: `${baseContent} with video`,
					attachments: [{id: 0, filename: 'video.mp4'}],
				},
				[{index: 0, filename: 'video.mp4', data: videoFixture}],
			);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					has: ['video'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.attachments).toBeDefined();
					const hasVideo = msg.attachments!.some(
						(a) => a.filename.endsWith('.mp4') || a.filename.endsWith('.webm') || a.filename.endsWith('.mov'),
					);
					expect(hasVideo).toBe(true);
				}
			}
		});
		test('has: file finds messages with file attachments', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has File Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `has-file-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} no file`);
			const fileData = Buffer.from('test file content');
			await sendMessageWithAttachments(
				harness,
				account.token,
				channelId,
				{
					content: `${baseContent} with file`,
					attachments: [{id: 0, filename: 'document.txt'}],
				},
				[{index: 0, filename: 'document.txt', data: fileData}],
			);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					has: ['file'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.attachments).toBeDefined();
					expect(msg.attachments!.length).toBeGreaterThan(0);
				}
			}
		});
		test('has: snapshot combined with has: image finds forwards whose snapshot carries an image', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Forward Image Guild');
			const sourceChannelId = guild.system_channel_id!;
			const destination = await createChannel(harness, account.token, guild.id, 'forward-target');
			const timestamp = Date.now();
			const baseContent = `forward-image-${timestamp}`;
			let imageFixture: Buffer;
			try {
				imageFixture = loadFixture('test.png');
			} catch {
				imageFixture = Buffer.from('PNG fake image data');
			}
			const {json: original} = await sendMessageWithAttachments(
				harness,
				account.token,
				sourceChannelId,
				{
					content: `${baseContent} with image`,
					attachments: [{id: 0, filename: 'image.png'}],
				},
				[{index: 0, filename: 'image.png', data: imageFixture}],
			);
			await sendMessage(harness, account.token, destination.id, `${baseContent} plain note in target`);
			await createBuilder<MessageResponse>(harness, account.token)
				.post(`/channels/${destination.id}/messages`)
				.body({
					message_reference: {
						message_id: original.id,
						channel_id: sourceChannelId,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			await markChannelAsIndexed(harness, destination.id);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					context_channel_id: destination.id,
					channel_id: [destination.id],
					has: ['snapshot', 'image'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				expect(result.messages.length).toBeGreaterThan(0);
				for (const msg of result.messages) {
					const snapshot = msg.message_snapshots?.[0];
					expect(snapshot).toBeDefined();
					const snapshotImage = snapshot?.attachments?.some(
						(a) => a.filename.endsWith('.png') || a.filename.endsWith('.jpg') || a.filename.endsWith('.gif'),
					);
					expect(snapshotImage).toBe(true);
				}
			}
		});
		test('exclude_has filters out messages with specified content types', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exclude Has Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `exclude-has-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} plain text no link`);
			await sendMessage(harness, account.token, channelId, `${baseContent} has link https://example.com`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					exclude_has: ['link'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.content).not.toMatch(/https?:\/\//);
				}
			}
		});
	});
	describe('Mentions Filtering', () => {
		test('mentions filter finds messages mentioning specific users', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Mentions Guild');
			const channelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			const timestamp = Date.now();
			const baseContent = `mentions-filter-${timestamp}`;
			await sendMessage(harness, owner.token, channelId, `${baseContent} no mention`);
			await sendMessage(harness, owner.token, channelId, `${baseContent} hey <@${member.userId}> check this`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					mentions: [member.userId],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					const mentionedUserIds = msg.mentions?.map((m) => m.id) ?? [];
					expect(mentionedUserIds).toContain(member.userId);
				}
			}
		});
		test('exclude_mentions filter excludes messages mentioning specific users', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Exclude Mentions Guild');
			const channelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			const timestamp = Date.now();
			const baseContent = `exclude-mentions-${timestamp}`;
			await sendMessage(harness, owner.token, channelId, `${baseContent} no mention here`);
			await sendMessage(harness, owner.token, channelId, `${baseContent} <@${member.userId}> mentioned`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					exclude_mentions: [member.userId],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					const mentionedUserIds = msg.mentions?.map((m) => m.id) ?? [];
					expect(mentionedUserIds).not.toContain(member.userId);
				}
			}
		});
		test('mention_everyone filter finds messages that mention everyone', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Mention Everyone Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `mention-everyone-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} normal message`);
			await sendMessage(harness, account.token, channelId, `${baseContent} @everyone important announcement`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					mention_everyone: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.mention_everyone).toBe(true);
				}
			}
		});
	});
	describe('Pinned Status Filtering', () => {
		test('pinned: true returns only pinned messages', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Pinned True Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `pinned-true-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} not pinned`);
			const pinnedMsg = await sendMessage(harness, account.token, channelId, `${baseContent} this is pinned`);
			await pinMessage(harness, account.token, channelId, pinnedMsg.id);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					pinned: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result) && result.messages.length > 0) {
				for (const msg of result.messages) {
					expect(msg.pinned).toBe(true);
				}
			}
		});
		test('pinned: false returns only non-pinned messages', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Pinned False Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const baseContent = `pinned-false-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${baseContent} not pinned`);
			const pinnedMsg = await sendMessage(harness, account.token, channelId, `${baseContent} this will be pinned`);
			await pinMessage(harness, account.token, channelId, pinnedMsg.id);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					pinned: false,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result) && result.messages.length > 0) {
				for (const msg of result.messages) {
					expect(msg.pinned).toBe(false);
				}
			}
		});
	});
	describe('Combined Filters', () => {
		test('multiple filters work together correctly', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Combined Filters Guild');
			const channelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			const timestamp = Date.now();
			const baseContent = `combined-filters-${timestamp}`;
			await sendMessage(harness, owner.token, channelId, `${baseContent} owner plain`);
			await sendMessage(harness, member.token, channelId, `${baseContent} member plain`);
			await sendMessage(harness, owner.token, channelId, `${baseContent} owner with link https://test.com`);
			const pinnedOwnerMsg = await sendMessage(
				harness,
				owner.token,
				channelId,
				`${baseContent} owner pinned with link https://example.org`,
			);
			await pinMessage(harness, owner.token, channelId, pinnedOwnerMsg.id);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					author_id: [owner.userId],
					has: ['link'],
					pinned: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.author.id).toBe(owner.userId);
					expect(msg.content).toMatch(/https?:\/\//);
					expect(msg.pinned).toBe(true);
				}
				if (result.messages.length > 0) {
					expect(result.messages.map((m) => m.id)).toContain(pinnedOwnerMsg.id);
				}
			}
		});
		test('author filter with channel filter works correctly', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Author Channel Guild');
			const channel1 = await getChannel(harness, owner.token, guild.system_channel_id!);
			const channel2 = await createChannel(harness, owner.token, guild.id, 'second-channel');
			const invite = await createChannelInvite(harness, owner.token, channel1.id);
			await acceptInvite(harness, member.token, invite.code);
			const timestamp = Date.now();
			const baseContent = `author-channel-${timestamp}`;
			await sendMessage(harness, owner.token, channel1.id, `${baseContent} owner ch1`);
			await sendMessage(harness, member.token, channel1.id, `${baseContent} member ch1`);
			await sendMessage(harness, owner.token, channel2.id, `${baseContent} owner ch2`);
			await markChannelAsIndexed(harness, channel1.id);
			await markChannelAsIndexed(harness, channel2.id);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_guild_id: guild.id,
					author_id: [owner.userId],
					channel_id: [channel1.id],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.author.id).toBe(owner.userId);
					expect(msg.channel_id).toBe(channel1.id);
				}
			}
		});
	});
});
