// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {createAttachmentID} from '@app/api/BrandedTypes';
import {
	createTestAccountForAttachmentTests,
	setupTestGuildAndChannel,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	createFavoriteMemeFromMessage,
	createMessageWithImageAttachment,
} from '@app/api/user/tests/FavoriteMemeTestUtils';
import {getExpiryBucket} from '@app/api/utils/AttachmentDecay';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface MessageWithDecayAttachment {
	id: string;
	attachments: Array<{
		id: string;
		filename: string;
		expires_at?: string | null;
		url?: string | null;
	}>;
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
