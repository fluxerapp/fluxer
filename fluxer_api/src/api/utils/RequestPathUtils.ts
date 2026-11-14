// SPDX-License-Identifier: AGPL-3.0-or-later

const VERSION_PATTERN = /^\/v\d+(?=\/|$)/;

export function stripApiPrefix(path: string): string {
	if (path === '/api') {
		return '/';
	}
	const unprefixedPath = path.startsWith('/api/') ? path.slice(4) : path;
	const versionMatch = unprefixedPath.match(VERSION_PATTERN);
	return versionMatch ? unprefixedPath.slice(versionMatch[0].length) || '/' : unprefixedPath;
}

export function normalizeRequestPath(path: string): string {
	let normalized = stripApiPrefix(path);
	if (normalized.length > 1 && normalized.endsWith('/')) {
		normalized = normalized.slice(0, -1);
	}
	return normalized;
}
