// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {UNREAD_BADGE_DOT, describeUnreadBadge, parseUnreadBadge} = await import('./UnreadBadge.ts');

describe('parseUnreadBadge', () => {
	test('accepts the dot, zero and positive counts', () => {
		assert.equal(parseUnreadBadge(UNREAD_BADGE_DOT), -1);
		assert.equal(parseUnreadBadge(0), 0);
		assert.equal(parseUnreadBadge(7), 7);
		assert.equal(parseUnreadBadge(Number.MAX_SAFE_INTEGER), Number.MAX_SAFE_INTEGER);
	});

	test('rejects anything that is not a safe integer of at least -1', () => {
		for (const value of [
			-2,
			1.5,
			Number.NaN,
			Number.POSITIVE_INFINITY,
			2 ** 53,
			'3',
			null,
			undefined,
			{count: 1},
			true,
		]) {
			assert.equal(parseUnreadBadge(value), null, String(value));
		}
	});
});

describe('describeUnreadBadge', () => {
	test('describes the dot and counts and nothing for zero', () => {
		assert.equal(describeUnreadBadge(UNREAD_BADGE_DOT), 'Unread messages');
		assert.equal(describeUnreadBadge(1234), 'Unread messages: 1,234');
		assert.equal(describeUnreadBadge(0), null);
	});
});
