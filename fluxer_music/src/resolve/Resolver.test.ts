// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {MusicError} from '../Errors';
import type {ResolvedTrack} from '../Types';
import {Resolver} from './Resolver';
import type {SpotifyClient} from './Spotify';
import type {YouTubeResolver} from './YouTube';

const track = (n: string, source: ResolvedTrack['source'] = 'youtube', durationMs = 200_000): ResolvedTrack => ({
	title: n,
	author: 'Artist',
	durationMs,
	thumbnailUrl: null,
	url: `https://example.com/${n}`,
	source,
});

function fakeYoutube() {
	const searches: Array<[string, number | undefined]> = [];
	const yt = {
		video: async (url: string) => track(`video:${url}`),
		playlist: async (url: string) => [track(`pl:${url}`)],
		search: async (query: string, limit?: number) => {
			searches.push([query, limit]);
			return [track('first', 'youtube', 201_000), track('second')];
		},
		streamUrl: async (url: string) => `stream:${url}`,
	};
	return {yt: yt as unknown as YouTubeResolver, searches};
}

describe('Resolver', () => {
	test('search kind resolves to the first result', async () => {
		const {yt} = fakeYoutube();
		const out = await new Resolver(yt, null).resolve('some song');
		expect(out.map((t) => t.title)).toEqual(['first']);
	});

	test('youtube and spotify kinds delegate', async () => {
		const {yt} = fakeYoutube();
		const sp = {
			tracks: async (type: string, id: string) => [track(`${type}:${id}`, 'spotify')],
		} as unknown as SpotifyClient;
		const r = new Resolver(yt, sp);
		expect((await r.resolve('https://youtu.be/abc'))[0]?.title).toBe('video:https://youtu.be/abc');
		expect((await r.resolve('https://www.youtube.com/playlist?list=PL1'))[0]?.title).toContain('pl:');
		expect((await r.resolve('https://open.spotify.com/album/xyz'))[0]?.title).toBe('album:xyz');
	});

	test('spotify without credentials → SOURCE_FAILED', async () => {
		const {yt} = fakeYoutube();
		const err = await new Resolver(yt, null).resolve('https://open.spotify.com/track/abc').catch((e) => e);
		expect(err).toBeInstanceOf(MusicError);
		expect(err.code).toBe('SOURCE_FAILED');
		expect(err.message).toContain("Spotify links aren't set up");
	});

	test('matchSpotify searches "artist - title" with limit 5', async () => {
		const {yt, searches} = fakeYoutube();
		const match = await new Resolver(yt, null).matchSpotify(track('Song', 'spotify', 200_000));
		expect(searches).toEqual([['Artist - Song', 5]]);
		expect(match?.title).toBe('second');
	});

	test('streamUrl rejects non-youtube tracks', async () => {
		const {yt} = fakeYoutube();
		const r = new Resolver(yt, null);
		expect(await r.streamUrl(track('x'))).toBe('stream:https://example.com/x');
		await expect(r.streamUrl(track('x', 'spotify'))).rejects.toMatchObject({code: 'BAD_REQUEST'});
	});
});
