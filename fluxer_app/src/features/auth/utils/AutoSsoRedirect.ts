// SPDX-License-Identifier: AGPL-3.0-or-later

export interface AutoSsoRedirectOptions {
	enabled: boolean;
	autoRedirect: boolean;
	localBypass: boolean;
	desktopHandoff: boolean;
	alreadyAttempted: boolean;
}

export const LOCAL_SSO_LOGIN_PATH = '/login?local=1';

export function isLocalSsoRedirectBypass(search: string): boolean {
	const params = new URLSearchParams(search);
	const local = params.get('local')?.toLowerCase();
	return local === '1' || local === 'true';
}

export function shouldPreferSsoStep({
	enabled,
	autoRedirect,
	localBypass,
	desktopHandoff,
}: Omit<AutoSsoRedirectOptions, 'alreadyAttempted'>): boolean {
	return enabled && autoRedirect && !localBypass && !desktopHandoff;
}

export function shouldAutoStartSso({
	enabled,
	autoRedirect,
	localBypass,
	desktopHandoff,
	alreadyAttempted,
}: AutoSsoRedirectOptions): boolean {
	return shouldPreferSsoStep({enabled, autoRedirect, localBypass, desktopHandoff}) && !alreadyAttempted;
}
