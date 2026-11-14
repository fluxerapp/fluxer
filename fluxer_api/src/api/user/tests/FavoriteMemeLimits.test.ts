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
import {MAX_FAVORITE_MEMES_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
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
