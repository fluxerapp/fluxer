// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import {isPasswordPwned, resetPwnedPasswordCacheForTesting} from '@app/api/auth/AuthPassword';
import {getConfig} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const PWNED_PASSWORD = 'fluxer-prefix-592';
const SAFE_PASSWORD_SAME_PREFIX = 'fluxer-prefix-837';

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

describe('isPasswordPwned', () => {
	let harness: ApiTestHarness;
	let ctx: ApiContext;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		ctx = createApiContext();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(() => {
		resetPwnedPasswordCacheForTesting();
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
	test('makes no upstream call when the check is switched off', async () => {
		const config = getConfig();
		const originalEnabled = config.breachedPasswordCheck.enabled;
		const requestedPrefixes: Array<string> = [];
		server.use(rangeHandler(requestedPrefixes, [suffixOf(PWNED_PASSWORD)]));
		try {
			config.breachedPasswordCheck.enabled = false;
			await expect(isPasswordPwned(ctx, PWNED_PASSWORD)).resolves.toBe(false);
			expect(requestedPrefixes).toHaveLength(0);
		} finally {
			config.breachedPasswordCheck.enabled = originalEnabled;
		}
	});
	test('fails open on a non-OK response', async () => {
		server.use(http.get('https://api.pwnedpasswords.com/range/:prefix', () => HttpResponse.text('', {status: 503})));
		await expect(isPasswordPwned(ctx, PWNED_PASSWORD)).resolves.toBe(false);
	});
});
