// SPDX-License-Identifier: AGPL-3.0-or-later

import * as idna from 'idna-uts46-hx';

const HTTP_PROTOCOL = 'http:';
const HTTPS_PROTOCOL = 'https:';
const MAILTO_PROTOCOL = 'mailto:';
const TEL_PROTOCOL = 'tel:';
const SMS_PROTOCOL = 'sms:';
const APP_PROTOCOL = 'fluxer:';
const SPECIAL_PROTOCOLS_REGEX = /^(mailto:|tel:|sms:)/;
const PROTOCOL_REGEX = /:\/\//;
const TRAILING_SLASH_REGEX = /\/+$/;
const NORMALIZE_PHONE_REGEX = /[\s\-()]/g;

function createUrlObject(url: string): URL | null {
	if (typeof url !== 'string') return null;
	try {
		if (!PROTOCOL_REGEX.test(url)) {
			return null;
		}
		return new URL(url);
	} catch {
		return null;
	}
}

export function normalizePhoneNumber(phoneNumber: string): string {
	return phoneNumber.replace(NORMALIZE_PHONE_REGEX, '');
}

export function normalizeUrl(url: string): string {
	if (typeof url !== 'string') return url;
	if (SPECIAL_PROTOCOLS_REGEX.test(url)) {
		return url.replace(TRAILING_SLASH_REGEX, '');
	}
	const urlObj = createUrlObject(url);
	return urlObj ? urlObj.toString() : url;
}

function idnaEncodeURL(url: string): string {
	const urlObj = createUrlObject(url);
	if (!urlObj) return url;
	try {
		urlObj.hostname = idna.toAscii(urlObj.hostname).toLowerCase();
		urlObj.username = '';
		urlObj.password = '';
		return urlObj.toString();
	} catch {
		return url;
	}
}

export function convertToAsciiUrl(url: string): string {
	if (SPECIAL_PROTOCOLS_REGEX.test(url)) return url;
	if (url.startsWith(`${APP_PROTOCOL}//`)) return url;
	const urlObj = createUrlObject(url);
	return urlObj ? idnaEncodeURL(url) : url;
}

export function isValidUrl(urlStr: string): boolean {
	if (typeof urlStr !== 'string') return false;
	if (SPECIAL_PROTOCOLS_REGEX.test(urlStr)) return true;
	const urlObj = createUrlObject(urlStr);
	if (!urlObj) return false;
	const {protocol} = urlObj;
	return (
		protocol === HTTP_PROTOCOL ||
		protocol === HTTPS_PROTOCOL ||
		protocol === MAILTO_PROTOCOL ||
		protocol === TEL_PROTOCOL ||
		protocol === SMS_PROTOCOL ||
		protocol === APP_PROTOCOL
	);
}
