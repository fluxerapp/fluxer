// SPDX-License-Identifier: AGPL-3.0-or-later

const APP_NAVIGATION_EXACT_PATHS = new Set([
	'/app',
	'/login',
	'/register',
	'/forgot',
	'/reset',
	'/verify',
	'/authorize-ip',
	'/wasntme',
	'/pending',
	'/oauth2/authorize',
	'/auth/sso/callback',
	'/bookmarks',
	'/mentions',
	'/notifications',
	'/you',
	'/report',
	'/premium-callback',
	'/age-verification-callback',
	'/connection-callback',
	'/theme-studio',
	'/__notfound',
	'/invite',
	'/gift',
	'/theme',
]);

const APP_NAVIGATION_PREFIXES = ['/channels/', '/users/', '/invite/', '/gift/', '/theme/', '/migrate/'];

export function isAppNavigationPath(pathname: string): boolean {
	const normalizedPathname = pathname.length > 1 ? pathname.replace(/\/+$/, '') : pathname;
	if (APP_NAVIGATION_EXACT_PATHS.has(normalizedPathname)) {
		return true;
	}
	return APP_NAVIGATION_PREFIXES.some((prefix) => normalizedPathname.startsWith(prefix));
}
