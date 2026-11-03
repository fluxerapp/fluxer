// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createChannel,
	createGuild,
	loadFixture,
	sendMessageWithAttachments,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Embed Attachment URL Resolution', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	describe('Error Handling', () => {
		it('should reject embed referencing non-existent filename', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Missing Attachment Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Embed references missing file',
				attachments: [{id: 0, filename: 'yeah.png'}],
				embeds: [
					{
						title: 'Invalid Reference',
						description: 'This embed references a non-existent file',
						image: {url: 'attachment://nonexistent.png'},
					},
				],
			};
			const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'yeah.png', data: fileData},
			]);
			expect(response.status).toBe(400);
		});
	});
	describe('Validation Rules', () => {
		it('should reject non-image attachment in embed image field', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Non-Image Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const textFileData = Buffer.from('This is a text file, not an image.');
			const payload = {
				content: 'Test message with non-image in embed',
				attachments: [{id: 0, filename: 'document.txt'}],
				embeds: [
					{
						title: 'Invalid Embed',
						image: {url: 'attachment://document.txt'},
					},
				],
			};
			const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'document.txt', data: textFileData},
			]);
			expect(response.status).toBe(400);
		});
	});
	describe('Multiple Embeds and Files', () => {
		it('should preserve embed image metadata when attachment is excluded from standalone list', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Metadata Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Metadata preservation test',
				attachments: [{id: 0, filename: 'meta.png'}],
				embeds: [
					{
						title: 'Metadata Embed',
						image: {url: 'attachment://meta.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'meta.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.image?.url).toMatch(/^https?:\/\//);
			expect(embed.image?.url).not.toContain('attachment://');
			expect(embed.image?.content_type).toBe('image/png');
			expect(embed.image?.width).toBeGreaterThan(0);
			expect(embed.image?.height).toBeGreaterThan(0);
		});
	});
});
