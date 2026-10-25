// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {MusicError} from '../Errors';
import {SpotifyClient} from './Spotify';

const TOKEN_URL = 'https://accounts.spotify.com/api/token';
const API = 'https://api.spotify.com/v1';

type Reply = {status?: number; body?: unknown};

function fakeFetch(routes: Record<string, Array<Reply> | Reply>) {
	const calls: Array<{url: string; init?: RequestInit}> = [];
	const queues = new Map<string, Array<Reply>>();
	for (const [url, r] of Object.entries(routes)) queues.set(url, Array.isArray(r) ? [...r] : [r]);
	const f = (async (input: string | URL | Request, init?: RequestInit) => {
		const url = String(input);
		calls.push({url, init});
		const q = queues.get(url);
		if (!q) throw new Error(`unexpected fetch ${url}`);
		const reply = q.length > 1 ? (q.shift() as Reply) : (q[0] as Reply);
		return new Response(JSON.stringify(reply.body ?? {}), {status: reply.status ?? 200});
	}) as typeof fetch;
	return {f, calls};
}

const auth = (call?: {init?: RequestInit}) =>
	(call?.init?.headers as Record<string, string> | undefined)?.Authorization;
const token = (n = 'tok', expires = 3600): Reply => ({body: {access_token: n, expires_in: expires}});
const artists = (...names: Array<string>) => names.map((name) => ({name}));
const trackObj = (id: string, extra: Record<string, unknown> = {}) => ({
	id,
	name: `Song ${id}`,
	duration_ms: 1000,
	artists: artists('A', 'B'),
	is_local: false,
	...extra,
});

function client(f: typeof fetch, now?: () => number) {
	return new SpotifyClient({clientId: 'id', clientSecret: 'secret', fetch: f, now});
}

async function failure(p: Promise<unknown>): Promise<MusicError> {
	try {
		await p;
	} catch (e) {
		if (e instanceof MusicError) return e;
	}
	throw new Error('expected MusicError');
}

describe('SpotifyClient', () => {
	test('fetches a token once and reuses it until near expiry', async () => {
		let t = 0;
		const {f, calls} = fakeFetch({
			[TOKEN_URL]: token('tok', 3600),
			[`${API}/tracks/t1`]: {body: trackObj('t1', {album: {images: [{url: 'img'}]}})},
		});
		const c = client(f, () => t);
		const [track] = await c.tracks('track', 't1');
		await c.tracks('track', 't1');
		expect(calls.filter((x) => x.url === TOKEN_URL)).toHaveLength(1);
		const tokenCall = calls[0];
		expect(tokenCall?.init?.method).toBe('POST');
		expect(auth(tokenCall)).toBe(`Basic ${Buffer.from('id:secret').toString('base64')}`);
		expect(String(tokenCall?.init?.body)).toContain('grant_type=client_credentials');
		expect(track).toEqual({
			title: 'Song t1',
			author: 'A, B',
			durationMs: 1000,
			thumbnailUrl: 'img',
			url: 'https://open.spotify.com/track/t1',
			source: 'spotify',
		});
		t = 3600_000 - 59_000;
		await c.tracks('track', 't1');
		expect(calls.filter((x) => x.url === TOKEN_URL)).toHaveLength(2);
	});

	test('album maps tracks with album image', async () => {
		const {f} = fakeFetch({
			[TOKEN_URL]: token(),
			[`${API}/albums/al`]: {
				body: {images: [{url: 'cover'}], tracks: {items: [trackObj('a1'), trackObj('a2')], next: null}},
			},
		});
		const tracks = await client(f).tracks('album', 'al');
		expect(tracks.map((x) => x.url)).toEqual([
			'https://open.spotify.com/track/a1',
			'https://open.spotify.com/track/a2',
		]);
		expect(tracks.every((x) => x.thumbnailUrl === 'cover' && x.source === 'spotify')).toBe(true);
	});

	test('playlist follows next pages and stops at 100', async () => {
		const page = (start: number, n: number) => Array.from({length: n}, (_, i) => ({track: trackObj(`p${start + i}`)}));
		const next = `${API}/playlists/pl/tracks?offset=60`;
		const next2 = `${API}/playlists/pl/tracks?offset=120`;
		const {f, calls} = fakeFetch({
			[TOKEN_URL]: token(),
			[`${API}/playlists/pl`]: {body: {tracks: {items: page(0, 60), next}}},
			[next]: {body: {items: page(60, 60), next: next2}},
		});
		const tracks = await client(f).tracks('playlist', 'pl');
		expect(tracks).toHaveLength(100);
		expect(calls.some((x) => x.url === next2)).toBe(false);
	});

	test('playlist skips null and local items', async () => {
		const {f} = fakeFetch({
			[TOKEN_URL]: token(),
			[`${API}/playlists/pl`]: {
				body: {
					tracks: {
						items: [
							{track: null},
							{track: trackObj('loc', {is_local: true})},
							{track: trackObj('ok', {album: {images: []}})},
						],
						next: null,
					},
				},
			},
		});
		const tracks = await client(f).tracks('playlist', 'pl');
		expect(tracks).toHaveLength(1);
		expect(tracks[0]?.thumbnailUrl).toBeNull();
	});

	test('playlist 404 → NOT_FOUND with Spotify message', async () => {
		const {f} = fakeFetch({[TOKEN_URL]: token(), [`${API}/playlists/pl`]: {status: 404}});
		const err = await failure(client(f).tracks('playlist', 'pl'));
		expect(err.code).toBe('NOT_FOUND');
		expect(err.message).toContain('Spotify-made');
	});

	test('401 refreshes the token once then succeeds', async () => {
		const {f, calls} = fakeFetch({
			[TOKEN_URL]: [token('old'), token('new')],
			[`${API}/tracks/t1`]: [{status: 401}, {body: trackObj('t1')}],
		});
		const tracks = await client(f).tracks('track', 't1');
		expect(tracks).toHaveLength(1);
		expect(calls.filter((x) => x.url === TOKEN_URL)).toHaveLength(2);
		expect(auth(calls.at(-1))).toBe('Bearer new');
	});

	test('other failures → SOURCE_FAILED', async () => {
		const {f} = fakeFetch({[TOKEN_URL]: token(), [`${API}/tracks/t1`]: {status: 500}});
		expect((await failure(client(f).tracks('track', 't1'))).code).toBe('SOURCE_FAILED');
	});
});
