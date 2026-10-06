// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readPasskeyLoginRoute, writePasskeyLoginRoute} from '@app/features/auth/passkey_migration/PasskeyLoginRoute';
import {beforeEach, describe, expect, it} from 'vitest';

describe('PasskeyLoginRoute', () => {
	beforeEach(() => {
		window.localStorage.clear();
	});

	it('defaults to the page passkey when nothing is stored', () => {
		expect(readPasskeyLoginRoute()).toBe('native');
	});

	it('remembers the previous address after it is chosen and forgets it again', () => {
		writePasskeyLoginRoute('legacy');
		expect(readPasskeyLoginRoute()).toBe('legacy');
		expect(window.localStorage.getItem('fluxer:passkey-login-route')).toBe('legacy');
		writePasskeyLoginRoute('native');
		expect(readPasskeyLoginRoute()).toBe('native');
		expect(window.localStorage.getItem('fluxer:passkey-login-route')).toBeNull();
	});

	it('ignores unknown stored values', () => {
		window.localStorage.setItem('fluxer:passkey-login-route', 'something');
		expect(readPasskeyLoginRoute()).toBe('native');
	});
});
