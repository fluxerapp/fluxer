// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AttachmentURLType, setIsDevelopment, URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {beforeEach, describe, expect, it} from 'vitest';

describe('URLType', () => {
	it('rejects URLs without protocol', () => {
		const result = URLType.safeParse('example.com');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.INVALID_URL_FORMAT);
		}
	});
	it('rejects FTP URLs', () => {
		const result = URLType.safeParse('ftp://example.com');
		expect(result.success).toBe(false);
	});
	it('rejects file URLs', () => {
		const result = URLType.safeParse('file:///path/to/file');
		expect(result.success).toBe(false);
	});
	it('rejects URLs exceeding max length', () => {
		const longPath = 'a'.repeat(2040);
		const result = URLType.safeParse(`https://example.com/${longPath}`);
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.URL_LENGTH_INVALID);
		}
	});
	it('rejects empty strings', () => {
		const result = URLType.safeParse('');
		expect(result.success).toBe(false);
	});
});

describe('AttachmentURLType', () => {
	it('rejects attachment URLs with empty filename', () => {
		const result = AttachmentURLType.safeParse('attachment://');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.INVALID_URL_OR_ATTACHMENT_FORMAT);
		}
	});
	it('rejects attachment URLs with invalid filename characters', () => {
		const result = AttachmentURLType.safeParse('attachment://file name.txt');
		expect(result.success).toBe(false);
	});
	it('rejects attachment URLs with path separators', () => {
		const result = AttachmentURLType.safeParse('attachment://path/file.txt');
		expect(result.success).toBe(false);
	});
	it('rejects URLs without valid protocol', () => {
		const result = AttachmentURLType.safeParse('ftp://example.com/file.txt');
		expect(result.success).toBe(false);
	});
	it('rejects URLs exceeding max length', () => {
		const longFilename = 'a'.repeat(2040);
		const result = AttachmentURLType.safeParse(`https://example.com/${longFilename}`);
		expect(result.success).toBe(false);
	});
});

describe('URL validators in production mode', () => {
	beforeEach(() => {
		setIsDevelopment(false);
	});
	it('rejects localhost URLs in production mode', () => {
		const result = URLType.safeParse('http://localhost:3000');
		expect(result.success).toBe(false);
	});
	it('rejects URLs without TLD in production mode', () => {
		const result = URLType.safeParse('http://myservice:8080');
		expect(result.success).toBe(false);
	});
});
