// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFileSync, spawnSync} from 'node:child_process';
import fs from 'node:fs';
import {
	ffmpegContainerMatchesContentType,
	ffmpegMetadataStripArgs,
	processMediaFile,
} from '@app/api/infrastructure/StorageObjectHelpers';
import {temporaryFile} from 'tempy';
import {afterEach, describe, expect, it} from 'vitest';

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

	it('keeps video, audio and subtitle streams and leaves data streams behind', () => {
		const args = ffmpegMetadataStripArgs(INPUT_PATH, OUTPUT_PATH, 'video/quicktime');
		const maps = args.flatMap((arg, index) => (arg === '-map' ? [args[index + 1]] : []));
		expect(maps).toEqual(['0:v?', '0:a?', '0:s?']);
	});
});

describe('ffmpegContainerMatchesContentType', () => {
	it.each([
		['mov,mp4,m4a,3gp,3g2,mj2\n', 'video/mp4'],
		['mov,mp4,m4a,3gp,3g2,mj2\n', 'video/quicktime'],
		['mov,mp4,m4a,3gp,3g2,mj2\n', 'video/3gpp'],
		['mov,mp4,m4a,3gp,3g2,mj2\n', 'audio/mp4'],
		['matroska,webm\n', 'video/webm'],
		['matroska,webm\n', 'video/x-matroska'],
		['matroska,webm\n', 'audio/webm'],
		['avi\n', 'video/x-msvideo'],
		['flv\n', 'video/x-flv'],
		['mpegts\n', 'video/mp2t'],
		['mpeg\n', 'video/mpeg'],
		['asf\n', 'video/x-ms-wmv'],
		['ogg\n', 'video/ogg'],
		['mp3\n', 'audio/mpeg'],
		['ogg\n', 'audio/ogg'],
		['wav\n', 'audio/wav'],
		['flac\n', 'audio/flac'],
		['aac\n', 'audio/aac'],
		['aiff\n', 'audio/aiff'],
	])('accepts the %s container for %s', (formatNames, contentType) => {
		expect(ffmpegContainerMatchesContentType(formatNames, contentType)).toBe(true);
	});

	it.each([
		['mov,mp4,m4a,3gp,3g2,mj2\n', 'video/x-matroska'],
		['matroska,webm\n', 'video/mp4'],
		['mp3\n', 'video/mpeg'],
		['mpeg\n', 'audio/mpeg'],
		['mpegts\n', 'video/mpeg'],
		['wav\n', 'audio/flac'],
	])('refuses the %s container for %s', (formatNames, contentType) => {
		expect(ffmpegContainerMatchesContentType(formatNames, contentType)).toBe(false);
	});
});

const hasFfmpeg = ['ffmpeg', 'ffprobe'].every((tool) => spawnSync(tool, ['-version']).status === 0);

describe.skipIf(!hasFfmpeg)('processMediaFile with ffmpeg', () => {
	const paths: Array<string> = [];

	function tempPath(): string {
		const path = temporaryFile();
		paths.push(path);
		return path;
	}

	function generate(args: ReadonlyArray<string>): string {
		const path = tempPath();
		execFileSync('ffmpeg', [
			'-loglevel',
			'error',
			'-f',
			'lavfi',
			'-i',
			'testsrc=s=64x48:r=5',
			'-f',
			'lavfi',
			'-i',
			'sine=f=440',
			'-t',
			'1',
			'-c:v',
			'mpeg4',
			'-c:a',
			'aac',
			'-metadata',
			'title=private-title',
			...args,
			'-y',
			path,
		]);
		return path;
	}

	function probe(path: string): {
		format: {format_name: string; tags?: Record<string, string>};
		streams: Array<{codec_type: string}>;
	} {
		return JSON.parse(
			execFileSync('ffprobe', ['-v', 'error', '-show_format', '-show_streams', '-of', 'json', path]).toString(),
		);
	}

	afterEach(() => {
		for (const path of paths.splice(0)) {
			fs.rmSync(path, {force: true});
		}
	});

	it('rewrites a QuickTime file without its tags and data stream at paths that have no extension', async () => {
		const input = generate(['-timecode', '01:00:00:00', '-f', 'mov']);
		expect(probe(input).format.tags?.title).toBe('private-title');
		expect(probe(input).streams.map((stream) => stream.codec_type)).toContain('data');
		const output = tempPath();

		const processed = await processMediaFile(input, output, 'video/quicktime');

		const stripped = probe(output);
		expect(processed?.contentType).toBe('video/quicktime');
		expect(processed?.contentLength).toBe(fs.statSync(output).size);
		expect(stripped.format.format_name).toContain('mov');
		expect(stripped.format.tags?.title).toBeUndefined();
		expect(stripped.streams.map((stream) => stream.codec_type).sort()).toEqual(['audio', 'video']);
	});

	it('refuses a file whose container is not the one its content type names', async () => {
		const input = generate(['-f', 'matroska']);

		await expect(processMediaFile(input, tempPath(), 'video/mp4')).rejects.toThrow(/does not match content type/);
	});
});
