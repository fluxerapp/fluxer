// SPDX-License-Identifier: AGPL-3.0-or-later

import {HTTPHeader} from './HTTPConstants';

const PERMISSIONS_POLICY =
	'accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()';
const REFERRER_POLICY = 'strict-origin-when-cross-origin';
const CONTENT_TYPE_OPTIONS = 'nosniff';
const FRAME_OPTIONS = 'DENY';
const STRICT_TRANSPORT_SECURITY = 'max-age=31536000; includeSubDomains; preload';

interface SiteSecurityHeader {
	readonly name: string;
	readonly value: string;
}

const INSECURE_ORIGIN_HEADERS: ReadonlyArray<SiteSecurityHeader> = Object.freeze([
	Object.freeze({name: HTTPHeader.PERMISSIONS_POLICY, value: PERMISSIONS_POLICY}),
	Object.freeze({name: HTTPHeader.REFERRER_POLICY, value: REFERRER_POLICY}),
	Object.freeze({name: HTTPHeader.X_CONTENT_TYPE_OPTIONS, value: CONTENT_TYPE_OPTIONS}),
	Object.freeze({name: HTTPHeader.X_FRAME_OPTIONS, value: FRAME_OPTIONS}),
]);

const SECURE_ORIGIN_HEADERS: ReadonlyArray<SiteSecurityHeader> = Object.freeze([
	...INSECURE_ORIGIN_HEADERS,
	Object.freeze({name: HTTPHeader.STRICT_TRANSPORT_SECURITY, value: STRICT_TRANSPORT_SECURITY}),
]);

export function siteSecurityHeaders(secureOrigin: boolean): ReadonlyArray<SiteSecurityHeader> {
	if (secureOrigin) {
		return SECURE_ORIGIN_HEADERS;
	}
	return INSECURE_ORIGIN_HEADERS;
}
