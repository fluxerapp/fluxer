// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {loadConfig} from './Config';

const base = {FLUXER_MUSIC_BOT_TOKEN: 'secret-token', FLUXER_BASE_URL: 'https://fluxer.example'};

describe('loadConfig', () => {
	it('requires the token and names it without its value', () => {
		expect(() => loadConfig({FLUXER_BASE_URL: 'https://fluxer.example'})).toThrow('FLUXER_MUSIC_BOT_TOKEN');
		expect(() => loadConfig({FLUXER_MUSIC_BOT_TOKEN: 'secret-token'})).toThrow('FLUXER_BASE_URL');
		try {
			loadConfig({FLUXER_MUSIC_BOT_TOKEN: 'secret-token'});
		} catch (err) {
			expect(String(err)).not.toContain('secret-token');
		}
	});

	it('spotify null unless both keys set', () => {
		expect(loadConfig(base).spotify).toBeNull();
		expect(loadConfig({...base, SPOTIFY_CLIENT_ID: 'id'}).spotify).toBeNull();
		expect(loadConfig({...base, SPOTIFY_CLIENT_SECRET: 'sec'}).spotify).toBeNull();
		expect(loadConfig({...base, SPOTIFY_CLIENT_ID: 'id', SPOTIFY_CLIENT_SECRET: 'sec'}).spotify).toEqual({
			clientId: 'id',
			clientSecret: 'sec',
		});
	});

	it('defaults', () => {
		expect(loadConfig(base)).toEqual({
			token: 'secret-token',
			baseUrl: 'https://fluxer.example',
			spotify: null,
			ytDlpBin: 'yt-dlp',
			cookiesFile: '/run/music-secrets/yt-cookies.txt',
			healthPort: 8099,
		});
	});
});
