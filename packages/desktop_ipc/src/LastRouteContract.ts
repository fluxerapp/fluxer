// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_LAST_ROUTE_CHANNEL = 'desktop-last-route:report';

const MAX_LAST_ROUTE_LENGTH = 512;

function containsControlCharacter(value: string): boolean {
	for (let index = 0; index < value.length; index += 1) {
		const code = value.charCodeAt(index);
		if (code <= 0x1f || code === 0x7f) {
			return true;
		}
	}
	return false;
}

export function isRestorableDesktopRoutePath(value: unknown): value is string {
	if (typeof value !== 'string') return false;
	if (value.length === 0 || value.length > MAX_LAST_ROUTE_LENGTH) return false;
	if (!value.startsWith('/')) return false;
	if (value.startsWith('//')) return false;
	if (value.includes('\\')) return false;
	if (value.includes('://')) return false;
	if (containsControlCharacter(value)) return false;
	for (const segment of value.split('/')) {
		if (segment === '..') return false;
	}
	return true;
}
