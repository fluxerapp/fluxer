// SPDX-License-Identifier: AGPL-3.0-or-later

import {AVATAR_MAX_SIZE, EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	base64LengthForBytes,
	createBase64StringType,
	FilenameType,
	isValidBase64,
} from '@fluxer/schema/src/primitives/FileValidators';
import {describe, expect, it} from 'vitest';

describe('FilenameType', () => {
	it.each([
		['document.pdf', 'document.pdf'],
		['résumé-東京.txt', 'résumé-東京.txt'],
		['file123.txt', 'file123.txt'],
		['my_file-name.txt', 'my_file-name.txt'],
		['path/to/file.txt', 'path_to_file.txt'],
		['path\\to\\file.txt', 'path_to_file.txt'],
		['..file..txt', '.file.txt'],
		['video..mp4', 'video.mp4'],
		['CON.txt', '_CON.txt'],
		['lpt9', '_lpt9'],
		['console.txt', 'console.txt'],
		['my file.txt', 'my_file.txt'],
		['file\x00name.txt', 'filename.txt'],
		['file.<.txt', 'file_txt'],
		['<>:"|?*', 'unnamed'],
		['...', 'unnamed'],
		['  file.txt  ', 'file.txt'],
		['   ', 'unnamed'],
	])('normalizes %j to %j', (input, expected) => {
		expect(FilenameType.parse(input)).toBe(expected);
	});

	it.each(['', 'a'.repeat(256)])('rejects empty or oversized input %#', (input) => {
		expect(FilenameType.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.FILENAME_LENGTH_INVALID}]},
		});
	});

	it('bounds input length before normalization', () => {
		const filename = 'a'.repeat(255);
		expect(FilenameType.parse(filename)).toBe(filename);
		expect(FilenameType.safeParse(`${' '.repeat(255)}a`).success).toBe(false);
	});
});

describe('isValidBase64', () => {
	it.each(['', 'YQ==', 'YWI=', 'YWJj', 'SGVsbG8gV29ybGQ=', 'YWJj+/8='])('accepts canonical encoding %j', (input) => {
		expect(isValidBase64(input)).toBe(true);
	});

	it.each([
		'YQ',
		'YQ=',
		'YQ===',
		'====',
		'Y=Q=',
		'YR==',
		'YWJ=',
		'YWJ_',
		'YWJ-',
		'YW J',
		'éAAA',
	])('rejects malformed or noncanonical encoding %j', (input) => {
		expect(isValidBase64(input)).toBe(false);
	});

	it.each([1, 2, 3, 255, 256])('accepts a byte round trip of length %i', (length) => {
		const bytes = Buffer.from(Array.from({length}, (_, index) => index % 256));
		const encoded = bytes.toString('base64');
		expect(isValidBase64(encoded)).toBe(true);
		expect(Buffer.from(encoded, 'base64')).toEqual(bytes);
	});
});

describe('createBase64StringType', () => {
	const schema = createBase64StringType(1, 100);

	it.each([
		['SGVsbG8gV29ybGQ=', 'SGVsbG8gV29ybGQ='],
		['data:image/png;base64,SGVsbG8gV29ybGQ=', 'SGVsbG8gV29ybGQ='],
		['  SGVsbG8gV29ybGQ=  ', 'SGVsbG8gV29ybGQ='],
		['YWJj', 'YWJj'],
		['YQ==', 'YQ=='],
		['YWJj+/8=', 'YWJj+/8='],
	])('normalizes %j', (input, expected) => {
		expect(schema.parse(input)).toBe(expected);
	});

	it.each(['Invalid!Base64@', 'YQ===', 'YR==', 'YWJ='])('rejects invalid payload %j', (input) => {
		expect(schema.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.INVALID_BASE64_FORMAT}]},
		});
	});

	it.each([
		{min: 1, max: 10, input: 'SGVsbG8gV29ybGQ='},
		{min: 100, max: 200, input: 'YQ=='},
	])('enforces encoded-length bounds $min..$max', ({min, max, input}) => {
		expect(createBase64StringType(min, max).safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.BASE64_LENGTH_INVALID}]},
		});
	});

	it('checks the extracted payload length, not the data URL prefix', () => {
		expect(createBase64StringType(4, 4).parse('data:image/png;base64,YQ==')).toBe('YQ==');
	});

	it('rejects an empty payload even when the configured minimum is zero', () => {
		expect(createBase64StringType(0, 100).safeParse('')).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.INVALID_BASE64_FORMAT}]},
		});
	});
});

describe('base64LengthForBytes', () => {
	it.each([
		[0, 0],
		[1, 4],
		[2, 4],
		[3, 4],
		[4, 8],
		[EMOJI_MAX_SIZE, 699052],
		[AVATAR_MAX_SIZE, 13981016],
	])('encodes a ceiling of %i bytes in %i characters', (bytes, expected) => {
		expect(base64LengthForBytes(bytes)).toBe(expected);
	});

	it.each([255, 256, STICKER_MAX_SIZE])('rounds %i bytes up to complete base64 blocks', (bytes) => {
		expect(base64LengthForBytes(bytes) % 4).toBe(0);
	});

	it.each([
		EMOJI_MAX_SIZE,
		EMOJI_MAX_SIZE + 1,
	])('accepts %i encoded bytes; decoded-byte limits remain authoritative', (bytes) => {
		const schema = createBase64StringType(1, base64LengthForBytes(EMOJI_MAX_SIZE));
		const encoded = Buffer.alloc(bytes, 1).toString('base64');
		expect(encoded.length).toBe(base64LengthForBytes(EMOJI_MAX_SIZE));
		expect(schema.parse(encoded)).toBe(encoded);
	});
});
