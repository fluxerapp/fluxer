// SPDX-License-Identifier: AGPL-3.0-or-later

const APP_NAVIGATION_EXACT_PATHS = new Set([
	'/app',
	'/login',
	'/register',
	'/forgot',
	'/reset',
	'/recover',
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
	'/connection-callback',
	'/__notfound',
	'/invite',
	'/gift',
	'/theme',
]);

const APP_NAVIGATION_PREFIXES = ['/channels/', '/users/', '/invite/', '/gift/', '/theme/', '/migrate/'];

function normalizePathname(pathname: string): string {
	return pathname.length > 1 ? pathname.replace(/\/+$/, '') : pathname;
}

export function isAppNavigationPath(pathname: string): boolean {
	const normalizedPathname = normalizePathname(pathname);
	if (APP_NAVIGATION_EXACT_PATHS.has(normalizedPathname)) {
		return true;
	}
	return APP_NAVIGATION_PREFIXES.some((prefix) => normalizedPathname.startsWith(prefix));
}

const SIGNED_IN_SHELL_EXACT_PATHS = new Set(['/app', '/bookmarks', '/mentions', '/notifications', '/you']);

export function servesSignedInShell(pathname: string): boolean {
	const normalizedPathname = normalizePathname(pathname);
	return SIGNED_IN_SHELL_EXACT_PATHS.has(normalizedPathname) || normalizedPathname.startsWith('/channels/');
}
