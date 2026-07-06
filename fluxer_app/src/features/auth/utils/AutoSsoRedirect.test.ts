// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	LOCAL_SSO_LOGIN_PATH,
	isLocalSsoRedirectBypass,
	shouldAutoStartSso,
	shouldPreferSsoStep,
} from './AutoSsoRedirect';

describe('isLocalSsoRedirectBypass', () => {
	it('allows local login bypass through query params', () => {
		expect(isLocalSsoRedirectBypass('?local=1')).toBe(true);
		expect(isLocalSsoRedirectBypass('?redirect_to=%2F&local=true')).toBe(true);
	});

	it('marks explicit logout login path as a local bypass', () => {
		expect(isLocalSsoRedirectBypass(new URL(LOCAL_SSO_LOGIN_PATH, 'https://app.example').search)).toBe(true);
	});

	it('does not bypass auto redirect by default', () => {
		expect(isLocalSsoRedirectBypass('')).toBe(false);
		expect(isLocalSsoRedirectBypass('?local=0')).toBe(false);
	});
});

describe('shouldPreferSsoStep', () => {
	it('uses the SSO panel while automatic redirect is pending', () => {
		expect(
			shouldPreferSsoStep({
				enabled: true,
				autoRedirect: true,
				localBypass: false,
				desktopHandoff: false,
			}),
		).toBe(true);
	});

	it('does not prefer the SSO panel when local login was requested', () => {
		expect(
			shouldPreferSsoStep({
				enabled: true,
				autoRedirect: true,
				localBypass: true,
				desktopHandoff: false,
			}),
		).toBe(false);
	});
});

describe('shouldAutoStartSso', () => {
	it('starts SSO when enabled and auto redirect is configured', () => {
		expect(
			shouldAutoStartSso({
				enabled: true,
				autoRedirect: true,
				localBypass: false,
				desktopHandoff: false,
				alreadyAttempted: false,
			}),
		).toBe(true);
	});

	it.each([
		{enabled: false, autoRedirect: true, localBypass: false, desktopHandoff: false, alreadyAttempted: false},
		{enabled: true, autoRedirect: false, localBypass: false, desktopHandoff: false, alreadyAttempted: false},
		{enabled: true, autoRedirect: true, localBypass: true, desktopHandoff: false, alreadyAttempted: false},
		{enabled: true, autoRedirect: true, localBypass: false, desktopHandoff: true, alreadyAttempted: false},
		{enabled: true, autoRedirect: true, localBypass: false, desktopHandoff: false, alreadyAttempted: true},
	])('does not start SSO for %o', (options) => {
		expect(shouldAutoStartSso(options)).toBe(false);
	});
});