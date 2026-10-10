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
	});
	describe('Image and Thumbnail Fields', () => {
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
		it('should accept image and video attachments beyond the legacy image extensions', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Media Type Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const payload = {
				content: 'Test with jxl and mp4 embed media',
				attachments: [
					{id: 0, filename: 'photo.jxl'},
					{id: 1, filename: 'clip.mp4'},
				],
				embeds: [
					{
						title: 'Media Embed',
						image: {url: 'attachment://clip.mp4'},
						thumbnail: {url: 'attachment://photo.jxl'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'photo.jxl', data: Buffer.from('jxl bytes')},
				{index: 1, filename: 'clip.mp4', data: Buffer.from('mp4 bytes')},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toHaveLength(1);
			expect(json.embeds![0].image?.url).not.toContain('attachment://');
			expect(json.embeds![0].thumbnail?.url).not.toContain('attachment://');
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
			const file2Data = loadFixture('animated.gif');
			const payload = {
				content: 'Multiple embeds with different attachments',
				attachments: [
					{id: 0, filename: 'yeah.png'},
					{id: 1, filename: 'animated.gif'},
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
						image: {url: 'attachment://animated.gif'},
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'yeah.png', data: file1Data},
				{index: 1, filename: 'animated.gif', data: file2Data},
			]);
			expect(response.status).toBe(200);
			expect(json.embeds).toBeDefined();
			expect(json.embeds).toHaveLength(2);
			expect(json.embeds![0].image?.url).toContain('yeah.png');
			expect(json.embeds![1].image?.url).toContain('animated.gif');
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
