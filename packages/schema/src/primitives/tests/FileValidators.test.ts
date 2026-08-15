// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {createBase64StringType, FilenameType} from '@fluxer/schema/src/primitives/FileValidators';
import {describe, expect, it} from 'vitest';

describe('FilenameType', () => {
	it('accepts valid filenames', () => {
		const result = FilenameType.safeParse('document.pdf');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('document.pdf');
		}
	});
	it('accepts filenames with unicode characters', () => {
		const result = FilenameType.safeParse('archivo.txt');
		expect(result.success).toBe(true);
	});
	it('accepts filenames with numbers', () => {
		const result = FilenameType.safeParse('file123.txt');
		expect(result.success).toBe(true);
	});
	it('accepts filenames with underscores and hyphens', () => {
		const result = FilenameType.safeParse('my_file-name.txt');
		expect(result.success).toBe(true);
	});
	it('replaces path separators with underscores', () => {
		const result = FilenameType.safeParse('path/to/file.txt');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('path_to_file.txt');
		}
	});
	it('replaces backslashes with underscores', () => {
		const result = FilenameType.safeParse('path\\to\\file.txt');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('path_to_file.txt');
		}
	});
	it('collapses repeated dots into a single dot', () => {
		const result = FilenameType.safeParse('..file..txt');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('.file.txt');
		}
	});
	it('keeps the extension when the name contains repeated dots', () => {
		const result = FilenameType.safeParse('video..mp4');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('video.mp4');
		}
	});
	it('handles Windows reserved names by prefixing', () => {
		const result = FilenameType.safeParse('CON.txt');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('_CON.txt');
		}
	});
	it('replaces spaces with underscores', () => {
		const result = FilenameType.safeParse('my file.txt');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('my_file.txt');
		}
	});
	it('removes null bytes', () => {
		const result = FilenameType.safeParse('file\x00name.txt');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).not.toContain('\x00');
		}
	});
	it('falls back to "unnamed" for invalid filenames', () => {
		const result = FilenameType.safeParse('...');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('unnamed');
		}
	});
	it('rejects filenames exceeding max length', () => {
		const result = FilenameType.safeParse('a'.repeat(256));
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.FILENAME_LENGTH_INVALID);
		}
	});
	it('rejects empty filenames', () => {
		const result = FilenameType.safeParse('');
		expect(result.success).toBe(false);
	});
	it('trims whitespace', () => {
		const result = FilenameType.safeParse('  file.txt  ');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('file.txt');
		}
	});
});

describe('createBase64StringType', () => {
	it('accepts valid base64 strings', () => {
		const Base64Type = createBase64StringType(1, 100);
		const result = Base64Type.safeParse('SGVsbG8gV29ybGQ=');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('SGVsbG8gV29ybGQ=');
		}
	});
	it('accepts base64 with data URL prefix and extracts base64 part', () => {
		const Base64Type = createBase64StringType(1, 100);
		const result = Base64Type.safeParse('data:image/png;base64,SGVsbG8gV29ybGQ=');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('SGVsbG8gV29ybGQ=');
		}
	});
	it('accepts base64 without padding', () => {
		const Base64Type = createBase64StringType(1, 100);
		const result = Base64Type.safeParse('YQ==');
		expect(result.success).toBe(true);
	});
	it('rejects invalid base64 characters', () => {
		const Base64Type = createBase64StringType(1, 100);
		const result = Base64Type.safeParse('Invalid!Base64@');
		expect(result.success).toBe(false);
	});
	it('rejects base64 exceeding max length', () => {
		const Base64Type = createBase64StringType(1, 10);
		const result = Base64Type.safeParse('SGVsbG8gV29ybGQ=');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.BASE64_LENGTH_INVALID);
		}
	});
	it('rejects base64 shorter than min length', () => {
		const Base64Type = createBase64StringType(100, 200);
		const result = Base64Type.safeParse('YQ==');
		expect(result.success).toBe(false);
	});
	it('rejects base64 with incorrect padding', () => {
		const Base64Type = createBase64StringType(1, 100);
		const result = Base64Type.safeParse('YQ===');
		expect(result.success).toBe(false);
	});
	it('rejects empty base64 even when min is 0', () => {
		const Base64Type = createBase64StringType(0, 100);
		const result = Base64Type.safeParse('');
		expect(result.success).toBe(false);
	});
	it('handles base64 with plus and slash characters', () => {
		const Base64Type = createBase64StringType(1, 100);
		const result = Base64Type.safeParse('YWJj+/8=');
		expect(result.success).toBe(true);
	});
	it('trims whitespace before validation', () => {
		const Base64Type = createBase64StringType(1, 100);
		const result = Base64Type.safeParse('  SGVsbG8gV29ybGQ=  ');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('SGVsbG8gV29ybGQ=');
		}
	});
});
