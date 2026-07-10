// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PresignedAttachmentUploadRequest,
	PresignedAttachmentUploadResponse,
} from '@fluxer/schema/src/domains/message/AttachmentUploadSchemas';
import {ClientUploadedAttachmentRequest} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import {MessageAttachmentResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it} from 'vitest';

describe('attachment byte size schemas', () => {
	it('accepts upload request file sizes above int32', () => {
		const result = PresignedAttachmentUploadRequest.safeParse({
			attachments: [
				{
					id: 0,
					filename: 'large.bin',
					file_size: 2147483648,
					content_type: 'application/octet-stream',
				},
			],
		});
		expect(result.success).toBe(true);
	});

	it('accepts upload response file and part sizes above int32', () => {
		const result = PresignedAttachmentUploadResponse.safeParse({
			attachments: [
				{
					id: 0,
					filename: 'large.bin',
					upload_filename: 'uploads/tmp-large',
					file_size: 2147483648,
					content_type: 'application/octet-stream',
					upload_mode: 'multipart',
					upload_id: 'multipart-id',
					part_size: 2147483648,
					parts: [{part_number: 1, upload_url: 'https://uploads.example.test/part'}],
				},
			],
		});
		expect(result.success).toBe(true);
	});

	it('accepts completed upload file sizes up to the JavaScript safe integer limit', () => {
		const result = ClientUploadedAttachmentRequest.safeParse({
			id: 0,
			filename: 'max.bin',
			upload_filename: 'uploads/tmp-max',
			file_size: Number.MAX_SAFE_INTEGER,
			content_type: 'application/octet-stream',
		});
		expect(result.success).toBe(true);
	});

	it('rejects unsafe upload file sizes', () => {
		const result = ClientUploadedAttachmentRequest.safeParse({
			id: 0,
			filename: 'unsafe.bin',
			upload_filename: 'uploads/tmp-unsafe',
			file_size: Number.MAX_SAFE_INTEGER + 1,
			content_type: 'application/octet-stream',
		});
		expect(result.success).toBe(false);
	});

	it('emits message attachment sizes as safe integer numbers', () => {
		const result = MessageAttachmentResponse.safeParse({
			id: '123456789012345678',
			filename: 'large.bin',
			title: null,
			description: null,
			content_type: 'application/octet-stream',
			content_hash: null,
			size: Number.MAX_SAFE_INTEGER,
			url: 'https://cdn.example.test/large.bin',
			proxy_url: 'https://cdn.example.test/large.bin',
			width: null,
			height: null,
			placeholder: null,
			flags: 0,
			nsfw: null,
			duration: null,
			waveform: null,
			expires_at: null,
			expired: null,
		});
		expect(result.success).toBe(true);
	});
});
