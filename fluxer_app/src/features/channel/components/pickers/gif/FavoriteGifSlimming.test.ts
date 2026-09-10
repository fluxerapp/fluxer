// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {type FavoriteGifEntry, slimFavoriteGifEntry} from './FavoriteGifTypes';

function format(src: string, width: number) {
	return {src, proxy_src: `https://media.test/external/sig/${src}`, width, height: Math.round(width * 0.84)};
}

const FAT: FavoriteGifEntry = {
	url: 'https://klipy.com/gifs/reaction',
	proxy_url: 'https://media.test/external/sig/tiny.webm',
	width: 220,
	height: 185,
	media: {
		nanowebm: format('nano.webm', 90),
		tinywebm: format('tiny.webm', 220),
		mediumwebm: format('medium.webm', 320),
		webm: format('full.webm', 498),
		gif: format('full.gif', 498),
	},
	content_type: 'video/webm',
	placeholder: 'placeholder',
};

describe('slimFavoriteGifEntry', () => {
	it('drops the media map so a favorite is cheap to sync', () => {
		expect(slimFavoriteGifEntry(FAT).media).toEqual({});
	});

	it('promotes a preview that covers a 200px tile at 2x into the top-level fields', () => {
		const slim = slimFavoriteGifEntry(FAT);
		expect(slim.proxy_url).toBe(FAT.media.webm.proxy_src);
		expect(slim.width).toBe(498);
		expect(slim.content_type).toBe('video/webm');
	});

	it('keeps url and placeholder untouched', () => {
		const slim = slimFavoriteGifEntry(FAT);
		expect(slim.url).toBe(FAT.url);
		expect(slim.placeholder).toBe(FAT.placeholder);
	});

	it('is idempotent so the synced roundtrip stays stable', () => {
		const once = slimFavoriteGifEntry(FAT);
		expect(slimFavoriteGifEntry(once)).toEqual(once);
	});

	it('leaves an entry that already carries no media alone', () => {
		const urlOnly: FavoriteGifEntry = {...FAT, media: {}};
		expect(slimFavoriteGifEntry(urlOnly)).toBe(urlOnly);
	});

	it('does not invent a preview when every format is unusable', () => {
		const broken: FavoriteGifEntry = {...FAT, media: {webm: {src: '', proxy_src: '', width: 0, height: 0}}};
		const slim = slimFavoriteGifEntry(broken);
		expect(slim.media).toEqual({});
		expect(slim.proxy_url).toBe(FAT.proxy_url);
	});
});
