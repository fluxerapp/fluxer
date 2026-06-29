// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import {beforeEach, describe, expect, it, vi} from 'vitest';
import {resolveGifRequestCountry} from './GifRequestCountry';

const {lookupGeoipMock} = vi.hoisted(() => ({
	lookupGeoipMock: vi.fn(),
}));

vi.mock('../utils/IpUtils', () => ({
	lookupGeoip: lookupGeoipMock,
}));

function geoip(countryCode: string | null): GeoipResult {
	return {
		countryCode,
		normalizedIp: '203.0.113.10',
		city: null,
		region: null,
		countryName: null,
	};
}

describe('resolveGifRequestCountry', () => {
	const req = new Request('https://fluxer.test/gifs/search');

	beforeEach(() => {
		lookupGeoipMock.mockReset();
	});

	it('uses the GeoIP country code for GIF provider requests', async () => {
		lookupGeoipMock.mockResolvedValue(geoip('se'));

		await expect(resolveGifRequestCountry(req)).resolves.toBe('SE');
	});

	it('falls back to US when GeoIP has no country', async () => {
		lookupGeoipMock.mockResolvedValue(geoip(null));

		await expect(resolveGifRequestCountry(req)).resolves.toBe('US');
	});

	it('falls back to US when GeoIP lookup fails', async () => {
		lookupGeoipMock.mockRejectedValue(new Error('geoip unavailable'));

		await expect(resolveGifRequestCountry(req)).resolves.toBe('US');
	});
});
