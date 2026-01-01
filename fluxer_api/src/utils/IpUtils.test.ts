/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {__clearIpCache, DEFAULT_CC, FETCH_TIMEOUT_MS, formatGeoipLocation, getCountryCode} from './IpUtils';

vi.mock('~/Config', () => ({Config: {geoip: {provider: 'ipinfo', host: 'geoip:8080'}}}));

describe('getCountryCode(ip)', () => {
	const realFetch = globalThis.fetch;

	beforeEach(() => {
		__clearIpCache();
		vi.useRealTimers();
		vi.restoreAllMocks();
	});

	afterEach(() => {
		globalThis.fetch = realFetch;
	});

	it('returns US if GEOIP_HOST is not set', async () => {
		vi.doMock('~/Config', () => ({Config: {geoip: {provider: 'ipinfo', host: ''}}}));
		const {getCountryCode: modFn, __clearIpCache: reset} = await import('./IpUtils');
		reset();
		const cc = await modFn('8.8.8.8');
		expect(cc).toBe(DEFAULT_CC);
	});

	it('returns US for invalid IP', async () => {
		const cc = await getCountryCode('not_an_ip');
		expect(cc).toBe(DEFAULT_CC);
	});

	it('accepts bracketed IPv6', async () => {
		globalThis.fetch = vi.fn().mockResolvedValue({ok: true, text: () => Promise.resolve('se')});
		const cc = await getCountryCode('[2001:db8::1]');
		expect(cc).toBe('SE');
	});

	it('returns uppercase alpha-2 on success', async () => {
		globalThis.fetch = vi.fn().mockResolvedValue({ok: true, text: () => Promise.resolve('se')});
		const cc = await getCountryCode('8.8.8.8');
		expect(cc).toBe('SE');
		expect(globalThis.fetch).toHaveBeenCalledTimes(1);
	});

	it('falls back on non-2xx', async () => {
		globalThis.fetch = vi.fn().mockResolvedValue({ok: false, text: () => Promise.resolve('se')});
		const cc = await getCountryCode('8.8.8.8');
		expect(cc).toBe(DEFAULT_CC);
	});

	it('falls back on invalid body', async () => {
		globalThis.fetch = vi.fn().mockResolvedValue({ok: true, text: () => Promise.resolve('USA')});
		const cc = await getCountryCode('8.8.8.8');
		expect(cc).toBe(DEFAULT_CC);
	});

	it('falls back on network error', async () => {
		globalThis.fetch = vi.fn().mockRejectedValue(new Error('fail'));
		const cc = await getCountryCode('8.8.8.8');
		expect(cc).toBe(DEFAULT_CC);
	});

	it('uses cache for repeated lookups within TTL', async () => {
		const spyNow = vi.spyOn(Date, 'now');
		spyNow.mockReturnValueOnce(1_000);
		globalThis.fetch = vi.fn().mockResolvedValue({ok: true, text: () => Promise.resolve('gb')});
		const r1 = await getCountryCode('1.1.1.1');
		expect(r1).toBe('GB');
		spyNow.mockReturnValueOnce(2_000);
		const r2 = await getCountryCode('1.1.1.1');
		expect(r2).toBe('GB');
		expect(globalThis.fetch).toHaveBeenCalledTimes(1);
	});

	it('aborts after timeout', async () => {
		vi.useFakeTimers();
		const abortingFetch = vi.fn<typeof fetch>(
			(_url, opts) =>
				new Promise((_res, rej) => {
					opts?.signal?.addEventListener('abort', () =>
						rej(Object.assign(new Error('AbortError'), {name: 'AbortError'})),
					);
				}),
		);
		globalThis.fetch = abortingFetch;
		const p = getCountryCode('8.8.8.8');
		vi.advanceTimersByTime(FETCH_TIMEOUT_MS + 5);
		await expect(p).resolves.toBe(DEFAULT_CC);
		expect(abortingFetch).toHaveBeenCalledTimes(1);
	});
});

describe('formatGeoipLocation(result)', () => {
	it('returns Unknown Location when no parts are available', () => {
		const label = formatGeoipLocation({
			countryCode: DEFAULT_CC,
			normalizedIp: '1.1.1.1',
			reason: null,
			city: null,
			region: null,
			countryName: null,
		});
		expect(label).toBe('Unknown Location');
	});

	it('concatenates available city, region, and country', () => {
		const label = formatGeoipLocation({
			countryCode: 'US',
			normalizedIp: '1.1.1.1',
			reason: null,
			city: 'San Francisco',
			region: 'CA',
			countryName: 'United States',
		});
		expect(label).toBe('San Francisco, CA, United States');
	});
});
