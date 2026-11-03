// SPDX-License-Identifier: AGPL-3.0-or-later

import {extractStringValues, shouldSkipContentFilterPath} from '@app/api/middleware/ContentFilterMiddleware';
import {describe, expect, test} from 'vitest';

describe('extractStringValues', () => {
	test('omits fields with identifier and secret suffixes', () => {
		const body = {
			label: 'visible label',
			actor_user_id: '111111111111111111',
			channel_ids: ['222222222222222222'],
			content_hash: 'f'.repeat(64),
			encryption_key: 'p256dh-value',
			original_proof: 'email-proof-token',
			state_token: 'state-token',
		};
		const result = extractStringValues(body);
		expect(result).toEqual(['visible label']);
	});
});

describe('shouldSkipContentFilterPath', () => {
	test('skips credential-only request bodies', () => {
		const paths = [
			'/users/@me/email-change/verify-original',
			'/users/@me/mfa/totp/enable',
			'/users/@me/password-change/complete',
			'/reports/dsa/email/verify',
		];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([true, true, true, true]);
	});
	test('does not skip in-app report submissions', () => {
		const paths = ['/reports/message', '/reports/user', '/reports/flows/message/submissions'];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([false, false, false]);
	});
	test('does not skip public content update request bodies', () => {
		const paths = ['/guilds/123/vanity-url', '/channels/123/messages', '/users/@me'];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([false, false, false]);
	});
});
