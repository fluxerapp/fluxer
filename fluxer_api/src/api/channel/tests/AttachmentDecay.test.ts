// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createChannel,
	createGuild,
	loadFixture,
	sendMessageWithAttachments,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface AttachmentDecayRow {
	attachment_id: string;
	channel_id: string;
	message_id: string;
	filename: string;
	size_bytes: string;
	expires_at: string;
	expiry_bucket: number;
	status: string | null;
}

describe('Attachment Decay', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should create decay metadata when uploading attachment', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Decay Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const channelId = guild.system_channel_id ?? channel.id;
		const fileData = loadFixture('yeah.png');
		const {response, json} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{
				content: 'Attachment with decay tracking',
				attachments: [{id: 0, filename: 'test.png'}],
			},
			[{index: 0, filename: 'test.png', data: fileData}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(json.attachments).toBeDefined();
		expect(json.attachments).not.toBeNull();
		expect(json.attachments!.length).toBe(1);
		const attachmentId = json.attachments![0].id;
		const decayResponse = await createBuilderWithoutAuth<{
			row: AttachmentDecayRow | null;
		}>(harness)
			.get(`/test/attachment-decay/${attachmentId}`)
			.execute();
		expect(decayResponse.row).not.toBeNull();
		expect(decayResponse.row?.attachment_id).toBe(attachmentId);
		expect(decayResponse.row?.channel_id).toBe(channelId);
		expect(decayResponse.row?.message_id).toBe(json.id);
	});
	test('should track multiple attachments in single message', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Multi Attachment Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const channelId = guild.system_channel_id ?? channel.id;
		const file1Data = loadFixture('yeah.png');
		const file2Data = loadFixture('animated.gif');
		const {response, json} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{
				content: 'Multiple attachments',
				attachments: [
					{id: 0, filename: 'first.png'},
					{id: 1, filename: 'second.gif'},
				],
			},
			[
				{index: 0, filename: 'first.png', data: file1Data},
				{index: 1, filename: 'second.gif', data: file2Data},
			],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(json.attachments).toBeDefined();
		expect(json.attachments).not.toBeNull();
		expect(json.attachments!.length).toBe(2);
		for (const attachment of json.attachments!) {
			const decayResponse = await createBuilderWithoutAuth<{
				row: AttachmentDecayRow | null;
			}>(harness)
				.get(`/test/attachment-decay/${attachment.id}`)
				.execute();
			expect(decayResponse.row).not.toBeNull();
			expect(decayResponse.row?.message_id).toBe(json.id);
		}
	});
	test('should configure decay metadata for forwarded snapshot attachments', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Forwarded Decay Test Guild');
		const destinationChannel = await createChannel(harness, account.token, guild.id, 'forward-destination');
		const sourceChannelId = guild.system_channel_id!;
		const {response, json: originalMessage} = await sendMessageWithAttachments(
			harness,
			account.token,
			sourceChannelId,
			{
				content: 'Forward attachment decay metadata',
				attachments: [{id: 0, filename: 'forwarded.png'}],
			},
			[{index: 0, filename: 'forwarded.png', data: loadFixture('yeah.png')}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(originalMessage.attachments).toHaveLength(1);
		const forwardedMessage = await createBuilder<{
			id: string;
			message_snapshots?: Array<{
				attachments?: Array<{
					id: string;
					expires_at?: string | null;
					url?: string | null;
				}>;
			}>;
		}>(harness, account.token)
			.post(`/channels/${destinationChannel.id}/messages`)
			.body({
				message_reference: {
					message_id: originalMessage.id,
					channel_id: sourceChannelId,
					guild_id: guild.id,
					type: MessageReferenceTypes.FORWARD,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const snapshotAttachment = forwardedMessage.message_snapshots?.[0]?.attachments?.[0];
		expect(snapshotAttachment).toBeDefined();
		expect(snapshotAttachment?.url).toBeTruthy();
		expect(snapshotAttachment?.expires_at).toBeTruthy();
		const decayResponse = await createBuilderWithoutAuth<{
			row: AttachmentDecayRow | null;
		}>(harness)
			.get(`/test/attachment-decay/${snapshotAttachment!.id}`)
			.execute();
		expect(decayResponse.row).not.toBeNull();
		expect(decayResponse.row?.attachment_id).toBe(snapshotAttachment!.id);
		expect(decayResponse.row?.channel_id).toBe(destinationChannel.id);
		expect(decayResponse.row?.message_id).toBe(forwardedMessage.id);
	});
});
