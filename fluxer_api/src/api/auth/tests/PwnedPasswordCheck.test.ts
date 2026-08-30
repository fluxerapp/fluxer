// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {delay, HttpResponse, http} from 'msw';
import {beforeEach, describe, expect, test} from 'vitest';
import type {ApiContext} from '../../ApiContext';
import {server} from '../../test/msw/server';
import {isPasswordPwned, resetPwnedPasswordCacheForTesting} from '../AuthPassword';

const PWNED_PASSWORD = 'fluxer-prefix-592';
const SAFE_PASSWORD_SAME_PREFIX = 'fluxer-prefix-837';
const HANGING_RESPONSE_MS = 8000;

const ctx = {} as unknown as ApiContext;

function sha1(password: string): string {
	return crypto.createHash('sha1').update(password).digest('hex').toUpperCase();
}

function prefixOf(password: string): string {
	return sha1(password).slice(0, 5);
}

function suffixOf(password: string): string {
	return sha1(password).slice(5);
}

function rangeBody(pwnedSuffixes: Array<string>): string {
	const padded = ['0'.repeat(35), '1'.repeat(35)].map((suffix) => `${suffix}:0`);
	return [...pwnedSuffixes.map((suffix) => `${suffix}:42`), ...padded].join('\r\n');
}

function rangeHandler(requestedPrefixes: Array<string>, pwnedSuffixes: Array<string>) {
	return http.get('https://api.pwnedpasswords.com/range/:prefix', ({params}) => {
		requestedPrefixes.push(String(params.prefix));
		return HttpResponse.text(rangeBody(pwnedSuffixes), {
			status: 200,
			headers: {'content-type': 'text/plain; charset=utf-8'},
		});
	});
}

function hangingRangeHandler() {
	return http.get('https://api.pwnedpasswords.com/range/:prefix', async () => {
		await delay(HANGING_RESPONSE_MS);
		return HttpResponse.text('');
	});
}

describe('isPasswordPwned', () => {
	beforeEach(() => {
		resetPwnedPasswordCacheForTesting();
	});
	test('the fixture passwords are distinct but share a range prefix', () => {
		expect(PWNED_PASSWORD).not.toBe(SAFE_PASSWORD_SAME_PREFIX);
		expect(prefixOf(PWNED_PASSWORD)).toBe(prefixOf(SAFE_PASSWORD_SAME_PREFIX));
	});
	test('reports a breached password from the range response', async () => {
		const requestedPrefixes: Array<string> = [];
		server.use(rangeHandler(requestedPrefixes, [suffixOf(PWNED_PASSWORD)]));
		await expect(isPasswordPwned(ctx, PWNED_PASSWORD)).resolves.toBe(true);
		expect(requestedPrefixes).toEqual([prefixOf(PWNED_PASSWORD)]);
	});
	test('two passwords sharing a prefix trigger a single upstream call', async () => {
		const requestedPrefixes: Array<string> = [];
		server.use(rangeHandler(requestedPrefixes, [suffixOf(PWNED_PASSWORD)]));
		await expect(isPasswordPwned(ctx, PWNED_PASSWORD)).resolves.toBe(true);
		await expect(isPasswordPwned(ctx, SAFE_PASSWORD_SAME_PREFIX)).resolves.toBe(false);
		expect(requestedPrefixes).toHaveLength(1);
	});
	test('fails open on a non-OK response', async () => {
		server.use(http.get('https://api.pwnedpasswords.com/range/:prefix', () => HttpResponse.text('', {status: 503})));
		await expect(isPasswordPwned(ctx, PWNED_PASSWORD)).resolves.toBe(false);
	});
	test('fails open the same way when the lookup times out, without caching the prefix', async () => {
		server.use(hangingRangeHandler());
		await expect(isPasswordPwned(ctx, PWNED_PASSWORD)).resolves.toBe(false);
		const requestedPrefixes: Array<string> = [];
		server.use(rangeHandler(requestedPrefixes, [suffixOf(PWNED_PASSWORD)]));
		await expect(isPasswordPwned(ctx, PWNED_PASSWORD)).resolves.toBe(true);
		expect(requestedPrefixes).toHaveLength(1);
	});
});
