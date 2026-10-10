// SPDX-License-Identifier: AGPL-3.0-or-later

import {ffmpegMetadataStripArgs} from '@app/api/infrastructure/StorageObjectHelpers';
import {describe, expect, it} from 'vitest';

const INPUT_PATH = '/tmp/0123456789abcdef';
const OUTPUT_PATH = '/tmp/fedcba9876543210';

function outputFormat(contentType: string): string | undefined {
	const args = ffmpegMetadataStripArgs(INPUT_PATH, OUTPUT_PATH, contentType);
	const flag = args.indexOf('-f');
	return flag === -1 ? undefined : args[flag + 1];
}

describe('ffmpegMetadataStripArgs', () => {
	it.each([
		['video/mp4', 'mp4'],
		['video/quicktime', 'mov'],
		['video/3gpp', '3gp'],
		['video/webm', 'webm'],
		['video/x-matroska', 'matroska'],
		['video/x-msvideo', 'avi'],
		['video/x-flv', 'flv'],
		['video/mp2t', 'mpegts'],
		['video/mpeg', 'mpeg'],
		['video/x-ms-wmv', 'asf'],
		['video/ogg', 'ogg'],
		['audio/mpeg', 'mp3'],
		['audio/mp4', 'mp4'],
		['audio/ogg', 'ogg'],
		['audio/webm', 'webm'],
		['audio/wav', 'wav'],
		['audio/flac', 'flac'],
		['audio/aac', 'adts'],
		['audio/aiff', 'aiff'],
	])('names the %s output format because the output path has no extension', (contentType, muxer) => {
		expect(outputFormat(contentType)).toBe(muxer);
	});

	it('writes to the output path last and reads the input path', () => {
		const args = ffmpegMetadataStripArgs(INPUT_PATH, OUTPUT_PATH, 'video/mp4');
		expect(args[args.indexOf('-i') + 1]).toBe(INPUT_PATH);
		expect(args.at(-1)).toBe(OUTPUT_PATH);
	});

	it.each([
		['video/mp4', true],
		['video/quicktime', true],
		['video/webm', false],
		['audio/mpeg', false],
	])('moves the %s index to the front of the file: %s', (contentType, fastStart) => {
		const args = ffmpegMetadataStripArgs(INPUT_PATH, OUTPUT_PATH, contentType);
		expect(args.includes('+faststart')).toBe(fastStart);
	});

	it('refuses a container it has no output format for', () => {
		expect(() => ffmpegMetadataStripArgs(INPUT_PATH, OUTPUT_PATH, 'video/x-unknown-container')).toThrow(
			/output format/,
		);
	});
});
