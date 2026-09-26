// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createTestAccountForAttachmentTests,
	setupTestGuildAndChannel,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	createFavoriteMemeFromMessage,
	createFavoriteMemeFromUrl,
	createMessageWithImageAttachment,
	listFavoriteMemes,
} from '@app/api/user/tests/FavoriteMemeTestUtils';
import {MAX_FAVORITE_MEME_TAGS, MAX_FAVORITE_MEMES_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Favorite Meme Limits', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should enforce maximum favorite memes limit for non-premium users', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		for (let i = 0; i < MAX_FAVORITE_MEMES_NON_PREMIUM; i++) {
			await createFavoriteMemeFromUrl(harness, account.token, {
				url: `https://cdn.example.test/memes/${i + 1}.png`,
				name: `Meme ${i + 1}`,
			});
		}
		const memes = await listFavoriteMemes(harness, account.token);
		expect(memes.length).toBe(MAX_FAVORITE_MEMES_NON_PREMIUM);
		await createBuilder(harness, account.token)
			.post('/users/@me/memes')
			.body({
				url: 'https://cdn.example.test/memes/one-too-many.png',
				name: 'One Too Many',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	}, 10000);
	test('should enforce maximum tags per meme limit', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const tooManyTags = Array.from({length: MAX_FAVORITE_MEME_TAGS + 1}, (_, i) => `tag${i + 1}`);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
				name: 'Too Many Tags',
				tags: tooManyTags,
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should allow exactly max tags per meme', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const maxTags = Array.from({length: MAX_FAVORITE_MEME_TAGS}, (_, i) => `tag${i + 1}`);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Max Tags Meme',
			tags: maxTags,
		});
		expect(meme.tags.length).toBe(MAX_FAVORITE_MEME_TAGS);
	});
	test('should enforce tag limit on update', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Initial Tags',
			tags: ['tag1', 'tag2'],
		});
		const tooManyTags = Array.from({length: MAX_FAVORITE_MEME_TAGS + 1}, (_, i) => `newtag${i + 1}`);
		await createBuilder(harness, account.token)
			.patch(`/users/@me/memes/${meme.id}`)
			.body({
				tags: tooManyTags,
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should reject saving same attachment twice', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Original',
		});
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
				name: 'Duplicate',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should allow different media content', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message1 = await createMessageWithImageAttachment(harness, account.token, channel.id, 'yeah.png');
		await createFavoriteMemeFromMessage(harness, account.token, channel.id, message1.id, {
			attachment_id: message1.attachments[0].id,
			name: 'PNG Meme',
		});
		const message2 = await createMessageWithImageAttachment(harness, account.token, channel.id, 'thisisfine.gif');
		const meme2 = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message2.id, {
			attachment_id: message2.attachments[0].id,
			name: 'GIF Meme',
		});
		expect(meme2.id).toBeTruthy();
		expect(meme2.filename).toBe('thisisfine.gif');
	});
	test('should return error for invalid attachment id', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: '999999999999999999',
				name: 'Invalid Attachment',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should return error for invalid embed index', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				embed_index: 99,
				name: 'Invalid Embed',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should return error for message without media', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const {sendMessage} = await import('@app/api/message/tests/MessageTestUtils');
		const textMessage = await sendMessage(harness, account.token, channel.id, 'No media here');
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${textMessage.id}/memes`)
			.body({
				embed_index: 0,
				name: 'No Media',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should return error for unknown message', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/999999999999999999/memes`)
			.body({
				attachment_id: '123',
				name: 'Unknown Message',
			})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should return error for inaccessible channel', async () => {
		const account1 = await createTestAccountForAttachmentTests(harness);
		const account2 = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account1);
		const message = await createMessageWithImageAttachment(harness, account1.token, channel.id);
		await createBuilder(harness, account2.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
				name: 'Inaccessible',
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should not allow accessing other users memes', async () => {
		const account1 = await createTestAccountForAttachmentTests(harness);
		const account2 = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account1);
		const message = await createMessageWithImageAttachment(harness, account1.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account1.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Private Meme',
		});
		await createBuilder(harness, account2.token)
			.get(`/users/@me/memes/${meme.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should not allow updating other users memes', async () => {
		const account1 = await createTestAccountForAttachmentTests(harness);
		const account2 = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account1);
		const message = await createMessageWithImageAttachment(harness, account1.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account1.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Private Meme',
		});
		await createBuilder(harness, account2.token)
			.patch(`/users/@me/memes/${meme.id}`)
			.body({
				name: 'Stolen Meme',
			})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
