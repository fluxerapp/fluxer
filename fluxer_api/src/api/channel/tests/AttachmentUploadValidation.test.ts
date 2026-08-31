// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Logger} from '@fluxer/logger/src/Logger';
import {beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import type {MediaProxyMetadataRequest} from '../../infrastructure/IMediaService';
import {setInjectedMediaService} from '../../middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {TestMediaService} from '../../test/TestMediaService';
import {createChannel, createGuild, loadFixture, sendMessageWithAttachments} from './AttachmentTestUtils';

class NullUploadMetadataService extends TestMediaService {
	override async getMetadata(request: MediaProxyMetadataRequest) {
		if (request.type === 'upload') {
			return null;
		}
		return super.getMetadata(request);
	}
}

describe('Attachment Upload Validation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		setInjectedMediaService(new TestMediaService(harness.storageService));
	});
	describe('File Validation', () => {
		describe('size limits', () => {
			it('should reject files exceeding size limits', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Too Large File Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const largeFileData = Buffer.alloc(26 * 1024 * 1024);
				const payload = {
					content: 'Large file test',
					attachments: [{id: 0, filename: 'large.bin'}],
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'large.bin', data: largeFileData},
				]);
				expect(response.status).toBe(400);
			});
			it('should store media files as plain attachments when metadata extraction fails', async () => {
				setInjectedMediaService(new NullUploadMetadataService(harness.storageService));
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Invalid Media Metadata Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const payload = {
					content: 'Invalid media metadata test',
					attachments: [{id: 0, filename: 'bad.png'}],
				};
				const {response, text} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'bad.png', data: loadFixture('yeah.png')},
				]);
				const body = JSON.parse(text) as {
					attachments?: Array<{filename?: string; width?: number | null; height?: number | null}>;
				};
				expect(response.status).toBe(200);
				expect(body.attachments?.[0]?.filename).toBe('bad.png');
				expect(body.attachments?.[0]?.width).toBeNull();
				expect(body.attachments?.[0]?.height).toBeNull();
			});
			it('should return service unavailable when fallback multipart storage upload fails', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Storage Unavailable Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
				harness.storageService.configure({shouldFailUpload: true});
				try {
					const payload = {
						content: 'storage unavailable test',
						attachments: [{id: 0, filename: 'diagnostics.txt'}],
					};
					const {response, text} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
						{index: 0, filename: 'diagnostics.txt', data: Buffer.from('diagnostics')},
					]);
					const body = JSON.parse(text) as {code?: string};
					expect(response.status).toBe(HTTP_STATUS.SERVICE_UNAVAILABLE);
					expect(body.code).toBe(APIErrorCodes.SERVICE_UNAVAILABLE);
					expect(errorLoggerSpy).toHaveBeenCalledTimes(1);
					expect(errorLoggerSpy).toHaveBeenCalledWith(
						{
							err: expect.objectContaining({
								cause: expect.objectContaining({message: 'Mock storage upload failure'}),
								code: APIErrorCodes.SERVICE_UNAVAILABLE,
								message: 'Attachment storage is temporarily unavailable',
								status: HTTP_STATUS.SERVICE_UNAVAILABLE,
							}),
							method: 'POST',
							path: `/channels/${channelId}/messages`,
							requestId: expect.any(String),
							status: HTTP_STATUS.SERVICE_UNAVAILABLE,
						},
						'Request failed',
					);
				} finally {
					harness.storageService.configure({shouldFailUpload: false});
					errorLoggerSpy.mockRestore();
				}
			});
		});
		describe('filename validation', () => {
			it('should reject empty filenames', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Empty Filename Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = Buffer.from('test content');
				const payload = {
					content: 'Empty filename test',
					attachments: [
						{
							id: 0,
							filename: '',
						},
					],
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'actualname.txt', data: fileData},
				]);
				expect(response.status).toBe(400);
			});
			it('should reject empty filename in metadata and upload', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Invalid Filename Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = Buffer.from('test content');
				const payload = {
					content: 'Invalid filename test',
					attachments: [
						{
							id: 0,
							filename: '',
						},
					],
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: '', data: fileData},
				]);
				expect(response.status).not.toBe(200);
			});
			it('should reject excessively long filename', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Invalid Filename Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = Buffer.from('test content');
				const longFilename = 'a'.repeat(300);
				const payload = {
					content: 'Invalid filename test',
					attachments: [
						{
							id: 0,
							filename: longFilename,
						},
					],
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: longFilename, data: fileData},
				]);
				expect(response.status).not.toBe(200);
			});
			it('should use metadata filename when different from upload filename', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Filename Mismatch Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = Buffer.from('test content');
				const payload = {
					content: 'Filename mismatch test',
					attachments: [{id: 0, filename: 'expected.txt'}],
				};
				const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'different.txt', data: fileData},
				]);
				expect(response.status).toBe(200);
				expect(json.attachments).toBeDefined();
				expect(json.attachments).not.toBeNull();
				expect(json.attachments!).toHaveLength(1);
				expect(json.attachments![0].filename).toBe('expected.txt');
			});
		});
		describe('special characters', () => {
			let channelId: string;
			let accountToken: string;
			beforeEach(async () => {
				const account = await createTestAccount(harness);
				accountToken = account.token;
				const guild = await createGuild(harness, account.token, 'Special Chars Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				channelId = guild.system_channel_id ?? channel.id;
			});
			const testCases = [
				{filename: 'file<script>.txt', shouldSanitize: true, description: 'HTML tag in filename'},
				{filename: 'file>output.txt', shouldSanitize: true, description: 'redirect operator'},
				{filename: 'file|pipe.txt', shouldSanitize: true, description: 'pipe character'},
				{filename: 'file:colon.txt', shouldSanitize: true, description: 'colon (Windows reserved)'},
				{filename: 'file*star.txt', shouldSanitize: true, description: 'asterisk (wildcard)'},
				{filename: 'file?question.txt', shouldSanitize: true, description: 'question mark'},
				{filename: 'file"quote.txt', shouldSanitize: true, description: 'double quote'},
				{filename: 'COM1.txt', shouldSanitize: true, description: 'Windows reserved name'},
				{filename: 'LPT1.txt', shouldSanitize: true, description: 'Windows reserved name'},
				{filename: 'file with spaces.txt', shouldSanitize: false, description: 'spaces should be OK'},
				{
					filename: 'file-dash_underscore.txt',
					shouldSanitize: false,
					description: 'dash and underscore OK',
				},
				{filename: 'file.multiple.dots.txt', shouldSanitize: false, description: 'multiple dots OK'},
				{filename: 'файл.txt', shouldSanitize: false, description: 'unicode characters OK'},
				{filename: '文件.txt', shouldSanitize: false, description: 'CJK characters OK'},
				{filename: '😀.txt', shouldSanitize: false, description: 'emoji OK'},
			];
			for (const tc of testCases) {
				it(`should handle ${tc.description}`, async () => {
					const fileData = Buffer.from('test content');
					const payload = {
						content: `Special char test: ${tc.description}`,
						attachments: [
							{
								id: 0,
								filename: tc.filename,
							},
						],
					};
					const {response, json} = await sendMessageWithAttachments(harness, accountToken, channelId, payload, [
						{index: 0, filename: 'test.txt', data: fileData},
					]);
					expect(response.status).toBe(200);
					expect(json.attachments).toBeDefined();
					expect(json.attachments).not.toBeNull();
					expect(json.attachments!).toHaveLength(1);
					const sanitized = json.attachments![0].filename;
					if (tc.shouldSanitize) {
						const dangerousChars = ['<', '>', ':', '"', '|', '?', '*'];
						for (const char of dangerousChars) {
							expect(sanitized).not.toContain(char);
						}
					}
				});
			}
		});
		describe('path traversal', () => {
			const testCases = [
				{input: '../../../etc/passwd', expectedSuffix: 'passwd'},
				{input: '..\\\\..\\\\..\\\\windows\\\\system32\\\\config\\\\sam', expectedSuffix: 'sam'},
				{input: '....//....//....//etc/passwd', expectedSuffix: 'passwd'},
				{input: '..\\\\..\\\\..\\\\', expectedSuffix: ''},
				{input: '../../sensitive.txt', expectedSuffix: 'sensitive.txt'},
				{input: './../../etc/hosts', expectedSuffix: 'hosts'},
			];
			for (const tc of testCases) {
				it(`should sanitize path traversal in filename: ${tc.input}`, async () => {
					const account = await createTestAccount(harness);
					const guild = await createGuild(harness, account.token, 'Path Traversal Test Guild');
					const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
					const channelId = guild.system_channel_id ?? channel.id;
					const fileData = Buffer.from('test content');
					const payload = {
						content: 'Path traversal test',
						attachments: [{id: 0, filename: tc.input}],
					};
					const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
						{index: 0, filename: tc.input, data: fileData},
					]);
					expect(response.status).toBe(200);
					expect(json.attachments).toBeDefined();
					expect(json.attachments).not.toBeNull();
					expect(json.attachments!).toHaveLength(1);
					const sanitized = json.attachments![0].filename;
					expect(sanitized).not.toContain('..');
					expect(sanitized).not.toContain('/');
					expect(sanitized).not.toContain('\\');
				});
			}
		});
	});
	describe('Metadata Validation', () => {
		describe('id matching', () => {
			it('should reject negative IDs in metadata', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Negative ID Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = Buffer.from('test content');
				const payload = {
					content: 'Negative ID test',
					attachments: [{id: -1, filename: 'test.txt'}],
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'test.txt', data: fileData},
				]);
				expect(response.status).toBe(400);
			});
			it('should reject mismatched IDs between metadata and files', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'ID Mismatch Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = Buffer.from('test content');
				const payload = {
					content: 'ID mismatch test',
					attachments: [{id: 5, filename: 'test.txt'}],
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'test.txt', data: fileData},
				]);
				expect(response.status).toBe(400);
			});
			it('should correctly match files with metadata when sent in natural order', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'ID Matching Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const file1Data = loadFixture('yeah.png');
				const file2Data = loadFixture('thisisfine.gif');
				const payload = {
					content: 'Ordered files test',
					attachments: [
						{id: 0, filename: 'yeah.png', description: 'First file', title: 'First'},
						{id: 1, filename: 'thisisfine.gif', description: 'Second file', title: 'Second'},
					],
				};
				const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'yeah.png', data: file1Data},
					{index: 1, filename: 'thisisfine.gif', data: file2Data},
				]);
				expect(response.status).toBe(200);
				expect(json.attachments).toBeDefined();
				expect(json.attachments).not.toBeNull();
				expect(json.attachments!).toHaveLength(2);
				expect(json.attachments![0].filename).toBe('yeah.png');
				expect(json.attachments![0].description).toBe('First file');
				expect(json.attachments![0].title).toBe('First');
				expect(json.attachments![1].filename).toBe('thisisfine.gif');
				expect(json.attachments![1].description).toBe('Second file');
				expect(json.attachments![1].title).toBe('Second');
			});
			it('should handle non-sequential IDs like 2, 5', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Sparse IDs Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const file1Data = loadFixture('yeah.png');
				const file2Data = loadFixture('thisisfine.gif');
				const payload = {
					content: 'Sparse IDs test',
					attachments: [
						{id: 2, filename: 'yeah.png', description: 'ID is 2', title: 'Two'},
						{id: 5, filename: 'thisisfine.gif', description: 'ID is 5', title: 'Five'},
					],
				};
				const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 2, filename: 'yeah.png', data: file1Data},
					{index: 5, filename: 'thisisfine.gif', data: file2Data},
				]);
				expect(response.status).toBe(200);
				expect(json.attachments).toBeDefined();
				expect(json.attachments).not.toBeNull();
				expect(json.attachments!).toHaveLength(2);
				expect(json.attachments![0].description).toBe('ID is 2');
				expect(json.attachments![0].title).toBe('Two');
				expect(json.attachments![1].description).toBe('ID is 5');
				expect(json.attachments![1].title).toBe('Five');
			});
		});
		describe('metadata requirements', () => {
			it('should allow file upload without attachment metadata', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Without Metadata Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = loadFixture('yeah.png');
				const payload = {
					content: 'File without metadata',
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'yeah.png', data: fileData},
				]);
				expect(response.status).toBe(200);
			});
			it('should reject attachment metadata without corresponding file', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Metadata Without File Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const payload = {
					content: 'Metadata without file',
					attachments: [{id: 0, filename: 'missing.png', description: 'This file does not exist'}],
				};
				const {response} = await sendMessageWithAttachments(harness, account.token, channelId, payload, []);
				expect(response.status).toBe(400);
			});
		});
		describe('title and description', () => {
			it('should preserve title and description fields through upload flow', async () => {
				const account = await createTestAccount(harness);
				const guild = await createGuild(harness, account.token, 'Title Description Test Guild');
				const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
				const channelId = guild.system_channel_id ?? channel.id;
				const fileData = loadFixture('yeah.png');
				const payload = {
					content: 'Testing title and description',
					attachments: [
						{
							id: 0,
							filename: 'yeah.png',
							title: 'My Awesome Title',
							description: 'This is a detailed description of the attachment with special chars: émoji 🎉',
						},
					],
				};
				const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
					{index: 0, filename: 'yeah.png', data: fileData},
				]);
				expect(response.status).toBe(200);
				expect(json.attachments).toBeDefined();
				expect(json.attachments).not.toBeNull();
				expect(json.attachments!).toHaveLength(1);
				const att = json.attachments![0];
				expect(att.title).toBe('My Awesome Title');
				const expectedDesc = 'This is a detailed description of the attachment with special chars: émoji 🎉';
				expect(att.description).toBe(expectedDesc);
			});
		});
	});
	describe('Flag Preservation', () => {
		it('should preserve attachment flags through upload flow', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Flags Preserved Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const fileData = loadFixture('yeah.png');
			const payload = {
				content: 'Flags preservation test',
				attachments: [
					{
						id: 0,
						filename: 'spoiler.png',
						flags: 8,
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'spoiler.png', data: fileData},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments).toBeDefined();
			expect(json.attachments).not.toBeNull();
			expect(json.attachments!).toHaveLength(1);
			expect(json.attachments![0].flags).toBe(8);
		});
	});
	describe('Multiple File Upload', () => {
		it('should handle multiple files with mixed metadata quality', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Mixed Metadata Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const channelId = guild.system_channel_id ?? channel.id;
			const file1Data = loadFixture('yeah.png');
			const file2Data = loadFixture('thisisfine.gif');
			const payload = {
				content: 'Mixed metadata test',
				attachments: [
					{
						id: 0,
						filename: 'yeah.png',
						title: 'Full Metadata',
						description: 'Complete description',
						flags: 0,
					},
					{
						id: 1,
						filename: 'thisisfine.gif',
					},
				],
			};
			const {response, json} = await sendMessageWithAttachments(harness, account.token, channelId, payload, [
				{index: 0, filename: 'yeah.png', data: file1Data},
				{index: 1, filename: 'thisisfine.gif', data: file2Data},
			]);
			expect(response.status).toBe(200);
			expect(json.attachments).toBeDefined();
			expect(json.attachments).not.toBeNull();
			expect(json.attachments!).toHaveLength(2);
			expect(json.attachments![0].title).toBe('Full Metadata');
			expect(json.attachments![0].description).toBe('Complete description');
			expect(json.attachments![1].title).toBeNull();
			expect(json.attachments![1].description).toBeNull();
		});
	});
});
