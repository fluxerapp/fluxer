// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {buildKlipyShareUrl, extractKlipySlugFromUrl} from './NatsGifProvider';

describe('NatsGifProvider KLIPY URL helpers', () => {
	it('extracts GIF and clip slugs from KLIPY share URLs', () => {
		expect(extractKlipySlugFromUrl('https://klipy.com/gifs/goatplaybanjo-chat-4')).toBe('goatplaybanjo-chat-4');
		expect(extractKlipySlugFromUrl('https://www.klipy.com/gif/funny-123')).toBe('funny-123');
		expect(extractKlipySlugFromUrl('https://klipy.com/clips/clip-123')).toBe('clip-123');
	});

	it('rejects non-KLIPY and unsupported URLs', () => {
		expect(extractKlipySlugFromUrl('https://notklipy.com/gifs/funny-123')).toBeNull();
		expect(extractKlipySlugFromUrl('https://klipy.com/search/funny-123')).toBeNull();
		expect(extractKlipySlugFromUrl('not a url')).toBeNull();
	});

	it('builds canonical GIF share URLs locally', () => {
		expect(buildKlipyShareUrl('goatplaybanjo-chat-4')).toBe('https://klipy.com/gifs/goatplaybanjo-chat-4');
		expect(buildKlipyShareUrl('  ')).toBe('https://klipy.com/gifs');
		expect(buildKlipyShareUrl('a slug')).toBe('https://klipy.com/gifs/a%20slug');
	});
});
