// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {describe, expect, test} from 'vitest';
import {MusicError} from '../Errors';
import {YouTubeResolver} from './YouTube';
import {createYtDlpRunner, type YtDlpRunner} from './YtDlp';

const fixture = (name: string) => fs.readFileSync(new URL(`./fixtures/${name}`, import.meta.url), 'utf8');

function fakeRunner(output: string) {
	const calls: Array<Array<string>> = [];
	const run: YtDlpRunner = async (args) => {
		calls.push(args);
		return output;
	};
	return {run, calls};
}

async function code(p: Promise<unknown>): Promise<string | undefined> {
	try {
		await p;
	} catch (e) {
		return e instanceof MusicError ? e.code : 'other';
	}
	return undefined;
}

describe('YouTubeResolver', () => {
	test('video maps fields', async () => {
		const {run, calls} = fakeRunner(fixture('video.json'));
		const track = await new YouTubeResolver(run).video('https://youtu.be/dQw4w9WgXcQ');
		expect(track).toEqual({
			title: 'Never Gonna Give You Up',
			author: 'Rick Astley',
			durationMs: 213_000,
			thumbnailUrl: 'https://i.ytimg.com/vi/dQw4w9WgXcQ/maxresdefault.jpg',
			url: 'https://www.youtube.com/watch?v=dQw4w9WgXcQ',
			source: 'youtube',
		});
		expect(calls[0]).toEqual(['-J', '--', 'https://youtu.be/dQw4w9WgXcQ']);
	});

	test('video falls back to uploader, last thumbnail and constructed url', async () => {
		const {run} = fakeRunner(
			JSON.stringify({id: 'x1', title: 't', uploader: 'U', duration: 5, thumbnails: [{url: 'a'}, {url: 'b'}]}),
		);
		const track = await new YouTubeResolver(run).video('u');
		expect(track).toMatchObject({author: 'U', thumbnailUrl: 'b', url: 'https://www.youtube.com/watch?v=x1'});
	});

	test('video refuses live', async () => {
		const {run} = fakeRunner(fixture('live.json'));
		expect(await code(new YouTubeResolver(run).video('u'))).toBe('LIVE_NOT_SUPPORTED');
	});

	test('video refuses over 3h', async () => {
		const {run} = fakeRunner(fixture('long.json'));
		expect(await code(new YouTubeResolver(run).video('u'))).toBe('TOO_LONG');
	});

	test('playlist caps at 100 and drops live entries', async () => {
		const {run, calls} = fakeRunner(fixture('playlist.json'));
		const tracks = await new YouTubeResolver(run).playlist('https://youtube.com/playlist?list=PL1');
		expect(tracks).toHaveLength(100);
		expect(tracks.some((t) => t.title === 'Song 3')).toBe(false);
		expect(tracks.some((t) => t.title === 'Song 4')).toBe(false);
		expect(calls[0]).toEqual([
			'-J',
			'--flat-playlist',
			'--playlist-end',
			'100',
			'--',
			'https://youtube.com/playlist?list=PL1',
		]);
	});

	test('search returns at most limit and puts the query after --', async () => {
		const {run, calls} = fakeRunner(fixture('search.jsonl'));
		const tracks = await new YouTubeResolver(run).search('-rf');
		expect(tracks).toHaveLength(10);
		expect(calls[0]?.at(-1)).toBe('ytsearch10:-rf');
		expect(calls[0]?.at(-2)).toBe('--');
	});

	test('empty search → NO_RESULTS', async () => {
		const {run} = fakeRunner('');
		expect(await code(new YouTubeResolver(run).search('zzz'))).toBe('NO_RESULTS');
	});

	test('streamUrl returns the first line', async () => {
		const {run, calls} = fakeRunner('https://cdn/a\nhttps://cdn/b\n');
		expect(await new YouTubeResolver(run).streamUrl('u')).toBe('https://cdn/a');
		expect(calls[0]).toEqual(['-f', 'bestaudio', '--get-url', '--', 'u']);
	});
});

describe('createYtDlpRunner', () => {
	function setup(script: string) {
		const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'ytdlp-test-'));
		const bin = path.join(dir, 'fake.sh');
		fs.writeFileSync(bin, `#!/bin/sh\n${script}\n`, {mode: 0o755});
		return {dir, bin};
	}

	test('runner adds fixed flags and cookies only when the file exists', async () => {
		const {dir, bin} = setup('echo "$@"');
		const cookies = path.join(dir, 'cookies.txt');
		const run = createYtDlpRunner({bin, cookiesFile: cookies});
		expect((await run(['-J', '--', 'u'])).trim()).toBe('--no-warnings --js-runtimes node -J -- u');
		fs.writeFileSync(cookies, '');
		expect((await run(['-J', '--', 'u'])).trim()).toBe(`--no-warnings --js-runtimes node --cookies ${cookies} -J -- u`);
	});

	test('runner non-zero exit → SOURCE_FAILED', async () => {
		const {bin} = setup('echo boom >&2; exit 3');
		const run = createYtDlpRunner({bin, cookiesFile: null});
		expect(await code(run(['x']))).toBe('SOURCE_FAILED');
	});

	test('runner times out → SOURCE_FAILED', async () => {
		const {bin} = setup('sleep 5');
		const run = createYtDlpRunner({bin, cookiesFile: null});
		const started = Date.now();
		expect(await code(run(['x'], {timeoutMs: 200}))).toBe('SOURCE_FAILED');
		expect(Date.now() - started).toBeLessThan(3000);
	});
});
