// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {AttachmentDecayRepository} from '../../attachment/AttachmentDecayRepository';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createAttachmentID} from '../../BrandedTypes';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {recalculateAttachmentDecay} from '../../worker/tasks/RecalculateAttachmentDecay';
import {createChannel, createGuild, loadFixture, sendMessageWithAttachments} from './AttachmentTestUtils';

describe('Attachment Decay Retroactive Expiry Bug Reproduction & Fix Verification', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('Scenario 1: If media expiry is enabled, then disabled, old attachments should retroactively NOT have expiry applied', async () => {
		// 1. Ensure media expiry is enabled
		await getInstanceConfigRepository().setInstanceMediaConfig({
			attachment_decay: {
				enabled: true,
				min_size_mb: 0,
				max_size_mb: 10,
				min_lifetime_days: 1,
				max_lifetime_days: 30,
			},
		});

		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Scenario 1 Guild');
		const channel = await createChannel(harness, account.token, guild.id, 's1-channel');
		const channelId = guild.system_channel_id ?? channel.id;
		const fileData = loadFixture('yeah.png');

		// 2. Upload an attachment
		const {response, json} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{
				content: 'Attachment with expiry initially enabled',
				attachments: [{id: 0, filename: 's1.png'}],
			},
			[{index: 0, filename: 's1.png', data: fileData}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		const attachmentId = json.attachments![0].id;

		// Verify it currently has expires_at in response
		let messageResponse = await createBuilder<{
			id: string;
			attachments: Array<{
				id: string;
				expires_at?: string | null;
			}>;
		}>(harness, account.token)
			.get(`/channels/${channelId}/messages/${json.id}`)
			.execute();
		expect(messageResponse.attachments![0].expires_at).toBeTruthy();

		// 3. Disable attachment expiry
		await getInstanceConfigRepository().setInstanceMediaConfig({
			attachment_decay: {
				enabled: false,
			},
		});

		// 4. Run the recalculation task
		await recalculateAttachmentDecay();

		// Verify the database record has null expires_at
		const repo = new AttachmentDecayRepository();
		const dbRecord = await repo.fetchById(createAttachmentID(BigInt(attachmentId)));
		expect(dbRecord?.expires_at).toBeNull();

		// Retrieve the message again. It should NOT have an expiry date applied.
		messageResponse = await createBuilder<{
			id: string;
			attachments: Array<{
				id: string;
				expires_at?: string | null;
			}>;
		}>(harness, account.token)
			.get(`/channels/${channelId}/messages/${json.id}`)
			.execute();

		expect(messageResponse.attachments![0].expires_at).toBeNull();
	});

	test('Scenario 2: If media expiry is disabled, then enabled, old attachments should retroactively have expiry applied', async () => {
		// 1. Ensure media expiry is disabled
		await getInstanceConfigRepository().setInstanceMediaConfig({
			attachment_decay: {
				enabled: false,
			},
		});

		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Scenario 2 Guild');
		const channel = await createChannel(harness, account.token, guild.id, 's2-channel');
		const channelId = guild.system_channel_id ?? channel.id;
		const fileData = loadFixture('yeah.png');

		// 2. Upload an attachment
		const {response, json} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{
				content: 'Attachment with expiry initially disabled',
				attachments: [{id: 0, filename: 's2.png'}],
			},
			[{index: 0, filename: 's2.png', data: fileData}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		const attachmentId = json.attachments![0].id;

		// Verify it currently does NOT have expires_at in response (since disabled)
		let messageResponse = await createBuilder<{
			id: string;
			attachments: Array<{
				id: string;
				expires_at?: string | null;
			}>;
		}>(harness, account.token)
			.get(`/channels/${channelId}/messages/${json.id}`)
			.execute();
		expect(messageResponse.attachments![0].expires_at).toBeFalsy();

		// 3. Enable attachment expiry with new rules
		await getInstanceConfigRepository().setInstanceMediaConfig({
			attachment_decay: {
				enabled: true,
				min_size_mb: 0,
				max_size_mb: 10,
				min_lifetime_days: 1,
				max_lifetime_days: 30,
			},
		});

		// 4. Run the recalculation task to apply new rules retroactively
		await recalculateAttachmentDecay();

		// Fetch the database record and check computed expiry (should be 30 days, not the default 3 years)
		const repo = new AttachmentDecayRepository();
		let dbRecord = await repo.fetchById(createAttachmentID(BigInt(attachmentId)));
		expect(dbRecord?.expires_at).not.toBeNull();

		const uploadedAt = new Date(dbRecord!.uploaded_at);
		const expiresAt = new Date(dbRecord!.expires_at);
		const diffDays = Math.round((expiresAt.getTime() - uploadedAt.getTime()) / (1000 * 60 * 60 * 24));
		expect(diffDays).toBe(30);

		// Retrieve the message again. It should have the new 30-day expiry date applied retroactively.
		messageResponse = await createBuilder<{
			id: string;
			attachments: Array<{
				id: string;
				expires_at?: string | null;
			}>;
		}>(harness, account.token)
			.get(`/channels/${channelId}/messages/${json.id}`)
			.execute();

		expect(messageResponse.attachments![0].expires_at).toBeTruthy();

		// Fetch the database record again to check that they match now (since GET request might have updated/renewed it)
		dbRecord = await repo.fetchById(createAttachmentID(BigInt(attachmentId)));
		expect(messageResponse.attachments![0].expires_at).toBe(dbRecord!.expires_at.toISOString());
	});
});
