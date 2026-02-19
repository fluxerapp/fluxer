/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {
	createTestAccountForAttachmentTests,
	setupTestGuildAndChannel,
} from '@fluxer/api/src/channel/tests/AttachmentTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import {
	createFavoriteMemeFromMessage,
	createFavoriteMemeFromUrl,
	createMessageWithImageAttachment,
	deleteFavoriteMeme,
	getFavoriteMeme,
	listFavoriteMemes,
	updateFavoriteMeme,
} from '@fluxer/api/src/user/tests/FavoriteMemeTestUtils';
import type {FavoriteMemeResponse} from '@fluxer/schema/src/domains/meme/MemeSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const TEST_IMAGE_URL = 'https://picsum.photos/id/1/100';

describe('Favorite Meme Extended Tests', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	describe('Create from URL', () => {
		test('should create meme from valid image URL', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			const meme = await createFavoriteMemeFromUrl(harness, account.token, {
				url: TEST_IMAGE_URL,
				name: 'My Test Meme',
			});

			expect(meme.id).toBeTruthy();
			expect(meme.name).toBe('My Test Meme');
			expect(meme.user_id).toBe(account.userId);
			expect(meme.url).toBeTruthy();
		});

		test('should create meme with all metadata from URL', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			const meme = await createFavoriteMemeFromUrl(harness, account.token, {
				url: TEST_IMAGE_URL,
				name: 'Full Metadata Meme',
				alt_text: 'A funny meme for testing purposes',
				tags: ['funny', 'test', 'meme'],
			});

			expect(meme.name).toBe('Full Metadata Meme');
			expect(meme.alt_text).toBe('A funny meme for testing purposes');
			expect(meme.tags).toEqual(['funny', 'test', 'meme']);
		});

		test('should reject name over 100 characters', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/memes')
				.body({
					url: TEST_IMAGE_URL,
					name: 'a'.repeat(101),
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should reject alt_text over 500 characters', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/memes')
				.body({
					url: TEST_IMAGE_URL,
					name: 'Test Meme',
					alt_text: 'a'.repeat(501),
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should reject more than 10 tags', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			const tags = Array.from({length: 11}, (_, i) => `tag${i}`);

			await createBuilder(harness, account.token)
				.post('/users/@me/memes')
				.body({
					url: TEST_IMAGE_URL,
					name: 'Test Meme',
					tags,
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should reject tag over 30 characters', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/memes')
				.body({
					url: TEST_IMAGE_URL,
					name: 'Test Meme',
					tags: ['a'.repeat(31)],
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should reject invalid URL', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/memes')
				.body({
					url: 'not-a-valid-url',
					name: 'Test Meme',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should reject missing URL', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/memes')
				.body({name: 'Test Meme'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should require authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/memes')
				.body({
					url: TEST_IMAGE_URL,
					name: 'Test Meme',
				})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});

		test('should derive name from URL filename when not provided', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			const meme = await createFavoriteMemeFromUrl(harness, account.token, {
				url: TEST_IMAGE_URL,
			});

			expect(meme.name).toBeTruthy();
			expect(meme.name.length).toBeGreaterThan(0);
		});
	});

	describe('Personal Notes (DM to self with meme)', () => {
		async function createAccountWithPersonalNotes(harness: ApiTestHarness) {
			const account = await createTestAccountForAttachmentTests(harness);
			await createBuilderWithoutAuth(harness)
				.post('/test/rpc-session-init')
				.body({
					type: 'session',
					token: account.token,
					version: 1,
					ip: '127.0.0.1',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			return account;
		}

		test('should send favorite meme as attachment in personal notes', async () => {
			const account = await createAccountWithPersonalNotes(harness);

			const meme = await createFavoriteMemeFromUrl(harness, account.token, {
				url: TEST_IMAGE_URL,
				name: 'Personal Notes Meme',
			});

			const channelId = account.userId;

			const message = await createBuilder<{
				id: string;
				attachments: Array<{id: string; filename: string}>;
			}>(harness, account.token)
				.post(`/channels/${channelId}/messages`)
				.body({favorite_meme_id: meme.id})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(message.attachments.length).toBe(1);
			expect(message.attachments[0].filename).toBe(meme.filename);
		});

		test('should fetch personal note message with meme attachment', async () => {
			const account = await createAccountWithPersonalNotes(harness);

			const meme = await createFavoriteMemeFromUrl(harness, account.token, {
				url: TEST_IMAGE_URL,
				name: 'Fetchable Meme',
			});

			const channelId = account.userId;

			const sendResponse = await createBuilder<{id: string}>(harness, account.token)
				.post(`/channels/${channelId}/messages`)
				.body({favorite_meme_id: meme.id})
				.execute();

			const fetchResponse = await createBuilder<{
				id: string;
				attachments: Array<{id: string; filename: string}>;
			}>(harness, account.token)
				.get(`/channels/${channelId}/messages/${sendResponse.id}`)
				.execute();

			expect(fetchResponse.id).toBe(sendResponse.id);
			expect(fetchResponse.attachments.length).toBe(1);
			expect(fetchResponse.attachments[0].filename).toBe(meme.filename);
		});
	});

	describe('Update Favorite Meme', () => {
		test('should update only name without affecting other fields', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Original',
				alt_text: 'Original description',
				tags: ['original'],
			});

			const updated = await updateFavoriteMeme(harness, account.token, created.id, {
				name: 'Updated Name',
			});

			expect(updated.name).toBe('Updated Name');
			expect(updated.alt_text).toBe('Original description');
			expect(updated.tags).toEqual(['original']);
		});

		test('should update only alt_text without affecting other fields', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'My Meme',
				tags: ['tag1'],
			});

			const updated = await updateFavoriteMeme(harness, account.token, created.id, {
				alt_text: 'New description',
			});

			expect(updated.name).toBe('My Meme');
			expect(updated.alt_text).toBe('New description');
			expect(updated.tags).toEqual(['tag1']);
		});

		test('should update only tags without affecting other fields', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Tagged Meme',
				alt_text: 'Description',
				tags: ['old'],
			});

			const updated = await updateFavoriteMeme(harness, account.token, created.id, {
				tags: ['new', 'tags'],
			});

			expect(updated.name).toBe('Tagged Meme');
			expect(updated.alt_text).toBe('Description');
			expect(updated.tags).toEqual(['new', 'tags']);
		});

		test('should update multiple fields at once', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Initial Name',
				alt_text: 'Initial description',
				tags: ['initial'],
			});

			const updated = await updateFavoriteMeme(harness, account.token, created.id, {
				name: 'New Name',
				alt_text: 'New description',
				tags: ['new', 'multiple'],
			});

			expect(updated.name).toBe('New Name');
			expect(updated.alt_text).toBe('New description');
			expect(updated.tags).toEqual(['new', 'multiple']);
		});

		test('should reject name over 100 characters on update', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Valid Name',
			});

			await createBuilder(harness, account.token)
				.patch(`/users/@me/memes/${created.id}`)
				.body({name: 'a'.repeat(101)})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should reject alt_text over 500 characters on update', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Valid Name',
			});

			await createBuilder(harness, account.token)
				.patch(`/users/@me/memes/${created.id}`)
				.body({alt_text: 'a'.repeat(501)})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('should return 404 for unknown meme on update', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			await createBuilder(harness, account.token)
				.patch('/users/@me/memes/999999999999999999')
				.body({name: 'New Name'})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});

	describe('Delete Favorite Meme', () => {
		test('should delete meme and remove from list', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'To Be Deleted',
			});

			const beforeList = await listFavoriteMemes(harness, account.token);
			expect(beforeList.some((m) => m.id === created.id)).toBe(true);

			await deleteFavoriteMeme(harness, account.token, created.id);

			const afterList = await listFavoriteMemes(harness, account.token);
			expect(afterList.some((m) => m.id === created.id)).toBe(false);
		});

		test('should delete idempotently without error', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Delete Twice',
			});

			await deleteFavoriteMeme(harness, account.token, created.id);

			await createBuilder(harness, account.token)
				.delete(`/users/@me/memes/${created.id}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});

		test('should return 204 for nonexistent meme', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			await createBuilder(harness, account.token)
				.delete('/users/@me/memes/999999999999999999')
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});

		test('should not delete other users memes', async () => {
			const account1 = await createTestAccountForAttachmentTests(harness);
			const account2 = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account1);

			const message = await createMessageWithImageAttachment(harness, account1.token, channel.id);
			const created = await createFavoriteMemeFromMessage(harness, account1.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Private Meme',
			});

			await createBuilder(harness, account2.token)
				.delete(`/users/@me/memes/${created.id}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();

			const meme = await getFavoriteMeme(harness, account1.token, created.id);
			expect(meme.id).toBe(created.id);
		});
	});

	describe('List Favorite Memes', () => {
		test('should return empty list when no memes exist', async () => {
			const account = await createTestAccountForAttachmentTests(harness);

			const memes = await listFavoriteMemes(harness, account.token);

			expect(memes).toEqual([]);
		});

		test('should return all memes for user', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message1 = await createMessageWithImageAttachment(harness, account.token, channel.id);
			await createFavoriteMemeFromMessage(harness, account.token, channel.id, message1.id, {
				attachment_id: message1.attachments[0].id,
				name: 'First',
			});

			const message2 = await createMessageWithImageAttachment(harness, account.token, channel.id, 'thisisfine.gif');
			await createFavoriteMemeFromMessage(harness, account.token, channel.id, message2.id, {
				attachment_id: message2.attachments[0].id,
				name: 'Second',
			});

			const message3 = await createMessageWithImageAttachment(harness, account.token, channel.id, 'sticker.png');
			await createFavoriteMemeFromMessage(harness, account.token, channel.id, message3.id, {
				attachment_id: message3.attachments[0].id,
				name: 'Third',
			});

			const memes = await listFavoriteMemes(harness, account.token);

			expect(memes.length).toBe(3);
			expect(memes.some((m) => m.name === 'First')).toBe(true);
			expect(memes.some((m) => m.name === 'Second')).toBe(true);
			expect(memes.some((m) => m.name === 'Third')).toBe(true);
		});

		test('should not return memes from other users', async () => {
			const account1 = await createTestAccountForAttachmentTests(harness);
			const account2 = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account1);

			const message = await createMessageWithImageAttachment(harness, account1.token, channel.id);
			await createFavoriteMemeFromMessage(harness, account1.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Account1 Meme',
			});

			const account2Memes = await listFavoriteMemes(harness, account2.token);

			expect(account2Memes).toEqual([]);
		});

		test('should include all fields in list response', async () => {
			const account = await createTestAccountForAttachmentTests(harness);
			const {channel} = await setupTestGuildAndChannel(harness, account);

			const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
			await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
				attachment_id: message.attachments[0].id,
				name: 'Complete Meme',
				alt_text: 'Description text',
				tags: ['tag1', 'tag2'],
			});

			const memes = await listFavoriteMemes(harness, account.token);
			const meme = memes[0];

			expect(meme.id).toBeTruthy();
			expect(meme.user_id).toBe(account.userId);
			expect(meme.name).toBe('Complete Meme');
			expect(meme.alt_text).toBe('Description text');
			expect(meme.tags).toEqual(['tag1', 'tag2']);
			expect(meme.attachment_id).toBeTruthy();
			expect(meme.filename).toBeTruthy();
			expect(meme.content_type).toBeTruthy();
			expect(meme.url).toBeTruthy();
		});

		test('should require authentication for list', async () => {
			await createBuilderWithoutAuth<Array<FavoriteMemeResponse>>(harness)
				.get('/users/@me/memes')
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
});
