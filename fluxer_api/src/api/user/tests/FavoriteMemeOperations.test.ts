// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {AttachmentDecayRepository} from '../../attachment/AttachmentDecayRepository';
import {createAttachmentID, createMemeID, createUserID} from '../../BrandedTypes';
import {
	createTestAccountForAttachmentTests,
	sendMessageWithAttachments,
	setupTestGuildAndChannel,
} from '../../channel/tests/AttachmentTestUtils';
import {fetchOne} from '../../database/CassandraQueryExecution';
import {Db} from '../../database/CassandraTypes';
import {FavoriteMemes} from '../../Tables';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {getExpiryBucket} from '../../utils/AttachmentDecay';
import {
	createFavoriteMemeFromMessage,
	createMessageWithImageAttachment,
	deleteFavoriteMeme,
	getFavoriteMeme,
	listFavoriteMemes,
	updateFavoriteMeme,
} from './FavoriteMemeTestUtils';

function animatedWebpProbeFixture(): Buffer {
	const buffer = Buffer.alloc(48);
	buffer.write('RIFF', 0, 'ascii');
	buffer.writeUInt32LE(40, 4);
	buffer.write('WEBP', 8, 'ascii');
	buffer.write('VP8X', 12, 'ascii');
	buffer.write('ANIM', 30, 'ascii');
	return buffer;
}

interface MessageWithDecayAttachment {
	id: string;
	attachments: Array<{
		id: string;
		filename: string;
		expires_at?: string | null;
		url?: string | null;
	}>;
}

interface MessageWithPlaceholderAttachment {
	id: string;
	attachments: Array<{
		id: string;
		filename: string;
		placeholder?: string | null;
	}>;
}

async function clearFavoriteMemePlaceholder(userId: string, memeId: string) {
	await fetchOne(
		FavoriteMemes.patchByPk(
			{user_id: createUserID(BigInt(userId)), meme_id: createMemeID(BigInt(memeId))},
			{placeholder: Db.set(null)},
		),
	);
}

async function fetchDecayRow(attachmentId: string) {
	return new AttachmentDecayRepository().fetchById(createAttachmentID(BigInt(attachmentId)));
}

async function deleteDecayRow(attachmentId: string): Promise<void> {
	const repo = new AttachmentDecayRepository();
	const row = await repo.fetchById(createAttachmentID(BigInt(attachmentId)));
	expect(row).not.toBeNull();
	await repo.deleteRecords({
		attachment_id: row!.attachment_id,
		expiry_bucket: getExpiryBucket(row!.expires_at),
		expires_at: row!.expires_at,
	});
}

describe('Favorite Meme Operations', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should create favorite meme from message attachment', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'My Favorite Meme',
		});
		expect(meme.id).toBeTruthy();
		expect(meme.name).toBe('My Favorite Meme');
		expect(meme.user_id).toBe(account.userId);
		expect(meme.filename).toBe('yeah.png');
		expect(meme.content_type).toBe('image/png');
	});
	test('should preserve animated metadata when source attachment flags are stale', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const filename = 'animated.webp';
		const {json: message} = await sendMessageWithAttachments(
			harness,
			account.token,
			channel.id,
			{
				content: 'stale animated flag repro',
				attachments: [{id: 0, filename, flags: 0}],
			},
			[{index: 0, filename, data: animatedWebpProbeFixture()}],
		);
		const sourceAttachment = message.attachments?.[0];
		expect(sourceAttachment).toBeDefined();
		expect(sourceAttachment!.flags & MessageAttachmentFlags.IS_ANIMATED).toBe(0);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: sourceAttachment!.id,
			name: 'Animated WebP',
		});
		expect(meme.is_gifv).toBe(true);
		const sent = await createBuilder<{
			attachments: Array<{flags: number; filename: string}>;
		}>(harness, account.token)
			.post(`/channels/${channel.id}/messages`)
			.body({favorite_meme_id: meme.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(sent.attachments[0].filename).toBe(filename);
		expect(sent.attachments[0].flags & MessageAttachmentFlags.IS_ANIMATED).toBe(MessageAttachmentFlags.IS_ANIMATED);
	});
	test('should carry the saved placeholder onto the sent attachment', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Placeholder Meme',
		});
		expect(meme.placeholder).toBeTruthy();
		const sent = await createBuilder<MessageWithPlaceholderAttachment>(harness, account.token)
			.post(`/channels/${channel.id}/messages`)
			.body({favorite_meme_id: meme.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(sent.attachments[0].placeholder).toBe(meme.placeholder);
	});
	test('should read-repair a missing placeholder when sending a favorite meme', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Repair Meme',
		});
		expect(meme.placeholder).toBeTruthy();
		await clearFavoriteMemePlaceholder(account.userId, meme.id);
		const stripped = await getFavoriteMeme(harness, account.token, meme.id);
		expect(stripped.placeholder).toBeNull();
		const sent = await createBuilder<MessageWithPlaceholderAttachment>(harness, account.token)
			.post(`/channels/${channel.id}/messages`)
			.body({favorite_meme_id: meme.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(sent.attachments[0].placeholder).toBeTruthy();
		const repaired = await getFavoriteMeme(harness, account.token, meme.id);
		expect(repaired.placeholder).toBe(sent.attachments[0].placeholder);
	});
	test('should create decay metadata when sending favorite meme', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Decay Meme',
		});
		const sent = await createBuilder<MessageWithDecayAttachment>(harness, account.token)
			.post(`/channels/${channel.id}/messages`)
			.body({favorite_meme_id: meme.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(sent.attachments).toHaveLength(1);
		const attachment = sent.attachments[0];
		expect(attachment.filename).toBe(meme.filename);
		expect(attachment.expires_at).toBeTruthy();
		expect(attachment.url).toBeTruthy();
		const row = await fetchDecayRow(attachment.id);
		expect(row).not.toBeNull();
		expect(row?.channel_id.toString()).toBe(channel.id);
		expect(row?.message_id.toString()).toBe(sent.id);
		expect(row?.filename).toBe(meme.filename);
	});
	test('should read-repair missing decay metadata for sent favorite memes', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Read Repair Meme',
		});
		const sent = await createBuilder<MessageWithDecayAttachment>(harness, account.token)
			.post(`/channels/${channel.id}/messages`)
			.body({favorite_meme_id: meme.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		const attachment = sent.attachments[0];
		await deleteDecayRow(attachment.id);
		expect(await fetchDecayRow(attachment.id)).toBeNull();
		const fetched = await createBuilder<MessageWithDecayAttachment>(harness, account.token)
			.get(`/channels/${channel.id}/messages/${sent.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(fetched.attachments).toHaveLength(1);
		expect(fetched.attachments[0].id).toBe(attachment.id);
		expect(fetched.attachments[0].expires_at).toBeTruthy();
		expect(fetched.attachments[0].url).toBeTruthy();
		const repairedRow = await fetchDecayRow(attachment.id);
		expect(repairedRow).not.toBeNull();
		expect(repairedRow?.channel_id.toString()).toBe(channel.id);
		expect(repairedRow?.message_id.toString()).toBe(sent.id);
	});
	test('should create favorite meme with alt text and tags', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Tagged Meme',
			alt_text: 'A funny image',
			tags: ['funny', 'reaction'],
		});
		expect(meme.name).toBe('Tagged Meme');
		expect(meme.alt_text).toBe('A funny image');
		expect(meme.tags).toEqual(['funny', 'reaction']);
	});
	test('should list favorite memes', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message1 = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createFavoriteMemeFromMessage(harness, account.token, channel.id, message1.id, {
			attachment_id: message1.attachments[0].id,
			name: 'First Meme',
		});
		const message2 = await createMessageWithImageAttachment(harness, account.token, channel.id, 'thisisfine.gif');
		await createFavoriteMemeFromMessage(harness, account.token, channel.id, message2.id, {
			attachment_id: message2.attachments[0].id,
			name: 'Second Meme',
		});
		const memes = await listFavoriteMemes(harness, account.token);
		expect(memes.length).toBe(2);
		expect(memes.some((m) => m.name === 'First Meme')).toBe(true);
		expect(memes.some((m) => m.name === 'Second Meme')).toBe(true);
	});
	test('should get single favorite meme by id', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Get Me',
		});
		const meme = await getFavoriteMeme(harness, account.token, created.id);
		expect(meme.id).toBe(created.id);
		expect(meme.name).toBe('Get Me');
	});
	test('should update favorite meme name', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Original Name',
		});
		const updated = await updateFavoriteMeme(harness, account.token, created.id, {
			name: 'New Name',
		});
		expect(updated.name).toBe('New Name');
	});
	test('should update favorite meme alt text', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Meme Name',
		});
		const updated = await updateFavoriteMeme(harness, account.token, created.id, {
			alt_text: 'Updated description',
		});
		expect(updated.alt_text).toBe('Updated description');
	});
	test('should clear favorite meme alt text by setting to null', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Meme',
			alt_text: 'Has alt text',
		});
		expect(created.alt_text).toBe('Has alt text');
		const updated = await updateFavoriteMeme(harness, account.token, created.id, {
			alt_text: null,
		});
		expect(updated.alt_text).toBeNull();
	});
	test('should update favorite meme tags', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Tagged',
			tags: ['old-tag'],
		});
		const updated = await updateFavoriteMeme(harness, account.token, created.id, {
			tags: ['new-tag', 'another-tag'],
		});
		expect(updated.tags).toEqual(['new-tag', 'another-tag']);
	});
	test('should delete favorite meme', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const created = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'Delete Me',
		});
		await deleteFavoriteMeme(harness, account.token, created.id);
		const memes = await listFavoriteMemes(harness, account.token);
		expect(memes.find((m) => m.id === created.id)).toBeUndefined();
	});
	test('should return 404 for unknown meme id', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		await createBuilder(harness, account.token)
			.get('/users/@me/memes/999999999999999999')
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should require name when creating from message', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should require either attachment_id or embed_index', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				name: 'Test Meme',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should validate name length max 100 characters', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
				name: 'a'.repeat(101),
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should validate alt_text length max 500 characters', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
				name: 'Valid Name',
				alt_text: 'a'.repeat(501),
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should validate tag length max 30 characters', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
				name: 'Valid Name',
				tags: ['a'.repeat(31)],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should reject empty tags', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/memes`)
			.body({
				attachment_id: message.attachments[0].id,
				name: 'Tagged Meme',
				tags: ['valid', '', '  ', 'another'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should delete idempotently', async () => {
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
	test('should return empty list when no memes exist', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const memes = await listFavoriteMemes(harness, account.token);
		expect(memes).toEqual([]);
	});
	test('should include url in meme response', async () => {
		const account = await createTestAccountForAttachmentTests(harness);
		const {channel} = await setupTestGuildAndChannel(harness, account);
		const message = await createMessageWithImageAttachment(harness, account.token, channel.id);
		const meme = await createFavoriteMemeFromMessage(harness, account.token, channel.id, message.id, {
			attachment_id: message.attachments[0].id,
			name: 'URL Test',
		});
		expect(meme.url).toBeTruthy();
		expect(meme.url).toContain(meme.attachment_id);
		expect(meme.url).toContain(meme.filename);
	});
});
