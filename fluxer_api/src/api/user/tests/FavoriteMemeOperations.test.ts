// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {createAttachmentID, createMemeID, createUserID} from '@app/api/BrandedTypes';
import {
	createTestAccountForAttachmentTests,
	sendMessageWithAttachments,
	setupTestGuildAndChannel,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {FavoriteMemes} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	createFavoriteMemeFromMessage,
	createMessageWithImageAttachment,
	getFavoriteMeme,
} from '@app/api/user/tests/FavoriteMemeTestUtils';
import {getExpiryBucket} from '@app/api/utils/AttachmentDecay';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

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
	test('should copy the saved placeholder onto the sent attachment', async () => {
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
});
