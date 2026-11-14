// SPDX-License-Identifier: AGPL-3.0-or-later

const HTTP_PREFIX = 'http://';
const HTTPS_PREFIX = 'https://';
const APP_PROTOCOL_SCHEME = 'fluxer:';
const APP_PROTOCOL_PREFIX = 'fluxer://';

export function startsWithUrl(text: string): boolean {
	if (text.length < HTTPS_PREFIX.length) return false;
	if (text.startsWith(HTTP_PREFIX)) {
		const prefixEnd = 7;
		return !text.substring(0, prefixEnd).includes('"') && !text.substring(0, prefixEnd).includes("'");
	}
	if (text.startsWith(HTTPS_PREFIX)) {
		const prefixEnd = 8;
		return !text.substring(0, prefixEnd).includes('"') && !text.substring(0, prefixEnd).includes("'");
	}
	if (text.startsWith(APP_PROTOCOL_PREFIX)) {
		const prefixEnd = APP_PROTOCOL_PREFIX.length;
		return !text.substring(0, prefixEnd).includes('"') && !text.substring(0, prefixEnd).includes("'");
	}
	if (text.startsWith(APP_PROTOCOL_SCHEME)) {
		const nextChar = text[APP_PROTOCOL_SCHEME.length] ?? '';
		return nextChar === '/' || /[A-Za-z0-9_-]/.test(nextChar);
	}
	return false;
}
