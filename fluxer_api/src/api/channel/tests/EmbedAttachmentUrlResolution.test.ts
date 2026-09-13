// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createChannel,
	createGuild,
	loadFixture,
	sendMessageWithAttachments,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Embed Attachment URL Resolution', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	describe('Basic URL Resolution', () => {
		it('should resolve attachment:// URLs in embed image field', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Embed Attachment Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Test message with embed',
				attachments: [{id: 0, filename: 'yeah.png'}],
				embeds: [
					{
						title: 'Test Embed',
						description: 'This embed uses an attached image',
						image: {url: 'attachment://yeah.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'yeah.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.content).toBe('Test message with embed');
			expect(json.embeds).toBeDefined();
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.title).toBe('Test Embed');
			expect(embed.image?.url).toBeTruthy();
			expect(embed.image?.url).not.toContain('attachment://');
		});
		it('should resolve attachment:// URLs in embed thumbnail field', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Thumbnail Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Test with thumbnail',
				attachments: [{id: 0, filename: 'yeah.png'}],
				embeds: [
					{
						title: 'Thumbnail Test',
						description: 'This embed uses a thumbnail',
						thumbnail: {url: 'attachment://yeah.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'yeah.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toBeDefined();
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.title).toBe('Thumbnail Test');
			expect(embed.thumbnail?.url).toBeTruthy();
			expect(embed.thumbnail?.url).not.toContain('attachment://');
		});
	});
	describe('Image and Thumbnail Fields', () => {
		it('should handle single embed using attachment:// for both image and thumbnail', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Both Fields Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const file1Data = loadFixture('yeah.png');
			const file2Data = loadFixture('thisisfine.gif');
			const payload = {
				content: 'Both image and thumbnail',
				attachments: [
					{id: 0, filename: 'yeah.png'},
					{id: 1, filename: 'thisisfine.gif'},
				],
				embeds: [
					{
						title: 'Complete Embed',
						description: 'This embed uses both image and thumbnail',
						image: {url: 'attachment://thisisfine.gif'},
						thumbnail: {url: 'attachment://yeah.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'yeah.png', data: file1Data},
				{index: 1, filename: 'thisisfine.gif', data: file2Data},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toBeDefined();
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.image?.url).toBeTruthy();
			expect(embed.thumbnail?.url).toBeTruthy();
			expect(embed.image?.url).not.toContain('attachment://');
			expect(embed.thumbnail?.url).not.toContain('attachment://');
		});
		it('should handle image and thumbnail in same embed from different files', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Image And Thumbnail Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Image and thumbnail from different attachments',
				attachments: [
					{id: 0, filename: 'main-image.png'},
					{id: 1, filename: 'thumb-image.png'},
				],
				embeds: [
					{
						title: 'Dual Image Embed',
						image: {url: 'attachment://main-image.png'},
						thumbnail: {url: 'attachment://thumb-image.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'main-image.png', data: fileData},
				{index: 1, filename: 'thumb-image.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.image?.url).not.toContain('attachment://');
			expect(embed.thumbnail?.url).not.toContain('attachment://');
			expect(embed.image?.url).not.toBe(embed.thumbnail?.url);
		});
		it('should handle mixed attachment:// and https:// URLs in embeds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Mixed URLs Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Test message with mixed embed URLs',
				attachments: [{id: 0, filename: 'local-image.png'}],
				embeds: [
					{
						title: 'Mixed URL Embed',
						description: 'This embed uses both attachment and external URLs',
						image: {url: 'attachment://local-image.png'},
						thumbnail: {url: 'https://example.com/external-image.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'local-image.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toBeDefined();
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.image?.url).toBeTruthy();
			expect(embed.image?.url).not.toContain('attachment://');
			expect(embed.thumbnail?.url).toBe('https://example.com/external-image.png');
		});
		it('should resolve attachment:// URL while preserving external thumbnail URL', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Preserve External URL Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Test with external thumbnail',
				attachments: [{id: 0, filename: 'attached.png'}],
				embeds: [
					{
						title: 'External Thumbnail Test',
						image: {url: 'attachment://attached.png'},
						thumbnail: {url: 'https://cdn.example.com/thumb.jpg'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'attached.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.image?.url).not.toContain('attachment://');
			expect(embed.thumbnail?.url).toBe('https://cdn.example.com/thumb.jpg');
		});
	});
	describe('Filename Matching', () => {
		it('should require exact filename matching', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Filename Matching Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload1 = {
				content: 'Case-sensitive filename test',
				attachments: [{id: 0, filename: 'yeah.png'}],
				embeds: [
					{
						title: 'Exact Match Required',
						image: {url: 'attachment://yeah.png'},
					},
				],
			};
			const {response: response1} = await sendMessageWithAttachments(harness, account.token, channelId, payload1, [
				{index: 0, filename: 'yeah.png', data: fileData},
			]);
			expect(response1.status).toBe(200);
			const payload2 = {
				content: 'Case mismatch test',
				attachments: [{id: 0, filename: 'yeah.png'}],
				embeds: [
					{
						title: 'Wrong Case',
						image: {url: 'attachment://Yeah.png'},
					},
				],
			};
			const {response: response2} = await sendMessageWithAttachments(harness, account.token, channelId, payload2, [
				{index: 0, filename: 'yeah.png', data: fileData},
			]);
			expect(response2.status).toBe(400);
		});
		it('should match correct file by filename', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Filename Match Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Testing filename matching',
				attachments: [
					{id: 0, filename: 'alpha.png'},
					{id: 1, filename: 'beta.png'},
				],
				embeds: [
					{
						title: 'Beta Image',
						image: {url: 'attachment://beta.png'},
					},
					{
						title: 'Alpha Image',
						image: {url: 'attachment://alpha.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'alpha.png', data: fileData},
				{index: 1, filename: 'beta.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
			expect(json.embeds).toHaveLength(2);
			expect(json.embeds![0].title).toBe('Beta Image');
			expect(json.embeds![0].image?.url).not.toContain('attachment://');
			expect(json.embeds![1].title).toBe('Alpha Image');
			expect(json.embeds![1].image?.url).not.toContain('attachment://');
		});
		it('should resolve spoiler attachment in embed', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Spoiler Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Test message with spoiler attachment in embed',
				attachments: [{id: 0, filename: 'SPOILER_secret.png', flags: MessageAttachmentFlags.IS_SPOILER}],
				embeds: [
					{
						title: 'Spoiler Embed',
						description: 'This embed uses a spoiler attachment',
						image: {url: 'attachment://SPOILER_secret.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'SPOILER_secret.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
			expect(json.embeds).toBeDefined();
			expect(json.embeds).toHaveLength(1);
			const embed = json.embeds![0];
			expect(embed.image?.url).toBeTruthy();
			expect(embed.image?.url).not.toContain('attachment://');
		});
		it('should preserve spoiler flag on attachment when referenced by embed', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Spoiler Flag Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Spoiler flag preservation test',
				attachments: [{id: 0, filename: 'SPOILER_hidden.png', flags: MessageAttachmentFlags.IS_SPOILER}],
				embeds: [
					{
						title: 'Hidden Content',
						image: {url: 'attachment://SPOILER_hidden.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'SPOILER_hidden.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
		});
		it('should handle spoiler attachment alongside non-spoiler in embed', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Mixed Spoiler Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Mixed spoiler and non-spoiler attachments',
				attachments: [
					{id: 0, filename: 'SPOILER_hidden.png', flags: MessageAttachmentFlags.IS_SPOILER},
					{id: 1, filename: 'visible.png'},
				],
				embeds: [
					{
						title: 'Spoiler Image',
						image: {url: 'attachment://SPOILER_hidden.png'},
					},
					{
						title: 'Visible Image',
						thumbnail: {url: 'attachment://visible.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'SPOILER_hidden.png', data: fileData},
				{index: 1, filename: 'visible.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
			expect(json.embeds).toHaveLength(2);
			expect(json.embeds![0].image?.url).not.toContain('attachment://');
			expect(json.embeds![1].thumbnail?.url).not.toContain('attachment://');
		});
	});
	describe('Error Handling', () => {
		it('should reject embed with attachment:// URL when no files are uploaded', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'No Attachments Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const payload = {
				content: 'Embed references attachment but none provided',
				embeds: [
					{
						title: 'Invalid Reference',
						description: 'No attachment uploaded',
						image: {url: 'attachment://image.png'},
					},
				],
			};
			await createBuilder(harness, account.token)
				.post(`/channels/${channelId}/messages`)
				.body(payload)
				.expect(400)
				.execute();
		});
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
		it('should reject PDF attachment in embed image field', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'PDF Rejection Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const pdfHeader = Buffer.from('%PDF-1.4\n');
			const pdfData = Buffer.concat([pdfHeader, Buffer.alloc(100)]);
			const payload = {
				content: 'Test message with PDF in embed',
				attachments: [{id: 0, filename: 'document.pdf'}],
				embeds: [
					{
						title: 'PDF Embed Attempt',
						image: {url: 'attachment://document.pdf'},
					},
				],
			};
			const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'document.pdf', data: pdfData},
			]);
			expect(response.status).toBe(400);
		});
		it('should reject JSON attachment in embed thumbnail field', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'JSON Rejection Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const jsonData = Buffer.from(JSON.stringify({key: 'value'}));
			const payload = {
				content: 'Test message with JSON in embed thumbnail',
				attachments: [{id: 0, filename: 'data.json'}],
				embeds: [
					{
						title: 'JSON Embed Attempt',
						thumbnail: {url: 'attachment://data.json'},
					},
				],
			};
			const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'data.json', data: jsonData},
			]);
			expect(response.status).toBe(400);
		});
		it('should reject executable attachment in embed image field', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Executable Rejection Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const exeData = Buffer.from('MZ');
			const payload = {
				content: 'Test message with executable in embed',
				attachments: [{id: 0, filename: 'program.exe'}],
				embeds: [
					{
						title: 'Executable Embed Attempt',
						image: {url: 'attachment://program.exe'},
					},
				],
			};
			const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'program.exe', data: exeData},
			]);
			expect(response.status).toBe(400);
		});
		it('should accept file with image extension but corrupted content for embed reference', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Content Type Mismatch Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const textData = Buffer.from('Not an image, just text.');
			const payload = {
				content: 'Test with fake PNG extension',
				attachments: [{id: 0, filename: 'fake.png'}],
				embeds: [
					{
						title: 'Fake Image Embed',
						image: {url: 'attachment://fake.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'fake.png', data: textData},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toHaveLength(1);
			expect(json.embeds![0].image?.url).not.toContain('attachment://');
		});
	});
	describe('Multiple Embeds and Files', () => {
		it('should handle multiple embeds with different URL types', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Multiple Embeds Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Multiple embeds with different URL types',
				attachments: [{id: 0, filename: 'image1.png'}],
				embeds: [
					{
						title: 'First Embed - Attachment',
						image: {url: 'attachment://image1.png'},
					},
					{
						title: 'Second Embed - External',
						image: {url: 'https://example.com/external.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'image1.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toHaveLength(2);
			expect(json.embeds![0].image?.url).not.toContain('attachment://');
			expect(json.embeds![1].image?.url).toBe('https://example.com/external.png');
		});
		it('should handle multiple embeds each referencing different attachments', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Multiple Embeds Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const file1Data = loadFixture('yeah.png');
			const file2Data = loadFixture('thisisfine.gif');
			const payload = {
				content: 'Multiple embeds with different attachments',
				attachments: [
					{id: 0, filename: 'yeah.png'},
					{id: 1, filename: 'thisisfine.gif'},
				],
				embeds: [
					{
						title: 'First Embed',
						description: 'Uses PNG',
						image: {url: 'attachment://yeah.png'},
					},
					{
						title: 'Second Embed',
						description: 'Uses GIF',
						image: {url: 'attachment://thisisfine.gif'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'yeah.png', data: file1Data},
				{index: 1, filename: 'thisisfine.gif', data: file2Data},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toBeDefined();
			expect(json.embeds).toHaveLength(2);
			expect(json.embeds![0].image?.url).toContain('yeah.png');
			expect(json.embeds![1].image?.url).toContain('thisisfine.gif');
		});
		it('should resolve multiple files referenced by embeds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Multiple Files Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Test message with multiple attachments in embeds',
				attachments: [
					{id: 0, filename: 'image1.png'},
					{id: 1, filename: 'image2.png'},
					{id: 2, filename: 'image3.png'},
				],
				embeds: [
					{
						title: 'First Image',
						image: {url: 'attachment://image1.png'},
					},
					{
						title: 'Second Image',
						image: {url: 'attachment://image2.png'},
					},
					{
						title: 'Third Image',
						thumbnail: {url: 'attachment://image3.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'image1.png', data: fileData},
				{index: 1, filename: 'image2.png', data: fileData},
				{index: 2, filename: 'image3.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
			expect(json.embeds).toHaveLength(3);
			for (const embed of json.embeds!) {
				if (embed.image) {
					expect(embed.image.url).not.toContain('attachment://');
				}
				if (embed.thumbnail) {
					expect(embed.thumbnail.url).not.toContain('attachment://');
				}
			}
		});
		it('should allow same attachment to be used in multiple embeds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Reused Attachment Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Same attachment in multiple embeds',
				attachments: [{id: 0, filename: 'shared.png'}],
				embeds: [
					{
						title: 'First Use',
						image: {url: 'attachment://shared.png'},
					},
					{
						title: 'Second Use',
						thumbnail: {url: 'attachment://shared.png'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'shared.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments ?? []).toHaveLength(0);
			expect(json.embeds).toHaveLength(2);
			expect(json.embeds![0].image?.url).not.toContain('attachment://');
			expect(json.embeds![1].thumbnail?.url).not.toContain('attachment://');
		});
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
