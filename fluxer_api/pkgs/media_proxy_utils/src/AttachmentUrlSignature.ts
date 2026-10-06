// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';

export const ATTACHMENT_URL_TTL_SECS = 86_400;
export const ATTACHMENT_URL_BUCKET_SECS = 43_200;

export const ORDINARY_USAGE = '';
export const DATA_PACKAGE_USAGE = 'dp';

export type AttachmentUrlUsage = typeof ORDINARY_USAGE | typeof DATA_PACKAGE_USAGE;
export type SignatureParameterName = 'ex' | 'is' | 'hm' | 'uc';

const SIGNATURE_DOMAIN = 'fluxer-attachment-url-v1';
const ATTACHMENT_PATH_PREFIX = '/attachments/';
const SIGNATURE_PARAMETER_NAMES: ReadonlyArray<SignatureParameterName> = ['ex', 'is', 'hm', 'uc'];
const DATA_PACKAGE_EXPIRES = '0';
const WINDOW_HEX_LENGTH = 8;
const MAX_WINDOW_SECS = 0xff_ff_ff_ff;
const LEADING_SLASHES_REGEX = /^\/+/u;
const TRAILING_SLASHES_REGEX = /\/+$/u;

const textEncoder = new TextEncoder();
const strictTextDecoder = new TextDecoder('utf-8', {fatal: true, ignoreBOM: true});

export interface AttachmentUrlWindow {
	issued: number;
	expires: number;
}

export interface SignAttachmentUrlOptions {
	mediaEndpoint: string;
	secret: Uint8Array;
	nowSecs: number;
	anchorSecs: number;
}

function hexNibble(byte: number): number {
	if (byte >= 0x30 && byte <= 0x39) return byte - 0x30;
	if (byte >= 0x41 && byte <= 0x46) return byte - 0x41 + 10;
	if (byte >= 0x61 && byte <= 0x66) return byte - 0x61 + 10;
	return -1;
}

function percentDecodeBytes(value: string, plusAsSpace: boolean): Uint8Array {
	const bytes = textEncoder.encode(value);
	const decoded = new Uint8Array(bytes.length);
	let length = 0;
	let index = 0;
	while (index < bytes.length) {
		const byte = bytes[index] as number;
		if (byte === 0x25 && index + 2 < bytes.length) {
			const high = hexNibble(bytes[index + 1] as number);
			const low = hexNibble(bytes[index + 2] as number);
			if (high >= 0 && low >= 0) {
				decoded[length] = (high << 4) | low;
				length += 1;
				index += 3;
				continue;
			}
		}
		decoded[length] = plusAsSpace && byte === 0x2b ? 0x20 : byte;
		length += 1;
		index += 1;
	}
	return decoded.subarray(0, length);
}

export function percentDecodeStorageKey(path: string): string | null {
	try {
		return strictTextDecoder.decode(percentDecodeBytes(path.replace(LEADING_SLASHES_REGEX, ''), false));
	} catch {
		return null;
	}
}

export function signatureParameterName(name: string): SignatureParameterName | null {
	const decoded = percentDecodeBytes(name, true);
	if (decoded.length !== 2) return null;
	const candidate = String.fromCharCode(decoded[0] as number, decoded[1] as number);
	return SIGNATURE_PARAMETER_NAMES.find((entry) => entry === candidate) ?? null;
}

export function isSignatureParameterName(name: string): boolean {
	return signatureParameterName(name) !== null;
}

function isSafeStorageKey(key: string): boolean {
	if (key.length === 0 || key.startsWith('/')) return false;
	return key
		.split('/')
		.every((component) => component.length > 0 && component !== '.' && component !== '..' && !component.includes('\0'));
}

function firstIndexOf(value: string, characters: ReadonlyArray<string>): number {
	let found = -1;
	for (const character of characters) {
		const index = value.indexOf(character);
		if (index >= 0 && (found < 0 || index < found)) {
			found = index;
		}
	}
	return found;
}

function rawPathFromUrl(url: string): string | null {
	const schemeIndex = url.indexOf('://');
	if (schemeIndex < 0) return null;
	const afterAuthority = url.slice(schemeIndex + 3);
	const boundary = firstIndexOf(afterAuthority, ['/', '?', '#']);
	if (boundary < 0 || afterAuthority[boundary] !== '/') return '';
	const path = afterAuthority.slice(boundary);
	const queryIndex = firstIndexOf(path, ['?', '#']);
	return queryIndex < 0 ? path : path.slice(0, queryIndex);
}

function parseWebUrl(value: string): URL | null {
	try {
		const parsed = new URL(value);
		return parsed.protocol === 'http:' || parsed.protocol === 'https:' ? parsed : null;
	} catch {
		return null;
	}
}

export function attachmentStorageKeyFromUrl(url: string, mediaEndpoint: string): string | null {
	const target = parseWebUrl(url);
	const endpoint = parseWebUrl(mediaEndpoint);
	if (!target || !endpoint || target.origin !== endpoint.origin) return null;
	const path = rawPathFromUrl(url);
	if (path === null) return null;
	const endpointPath = (rawPathFromUrl(mediaEndpoint) ?? '').replace(TRAILING_SLASHES_REGEX, '');
	if (!path.startsWith(`${endpointPath}${ATTACHMENT_PATH_PREFIX}`)) return null;
	const storageKey = percentDecodeStorageKey(path.slice(endpointPath.length));
	if (storageKey === null || !isSafeStorageKey(storageKey)) return null;
	return storageKey;
}

interface SplitUrl {
	base: string;
	query: string;
	fragment: string;
}

function splitUrl(url: string): SplitUrl {
	const fragmentIndex = url.indexOf('#');
	const head = fragmentIndex < 0 ? url : url.slice(0, fragmentIndex);
	const fragment = fragmentIndex < 0 ? '' : url.slice(fragmentIndex);
	const queryIndex = head.indexOf('?');
	if (queryIndex < 0) return {base: head, query: '', fragment};
	return {base: head.slice(0, queryIndex), query: head.slice(queryIndex + 1), fragment};
}

function preservedFields(query: string): Array<string> {
	if (query.length === 0) return [];
	return query.split('&').filter((field) => {
		if (field.length === 0 || field === '=') return false;
		const separator = field.indexOf('=');
		return !isSignatureParameterName(separator < 0 ? field : field.slice(0, separator));
	});
}

export function stripAttachmentSignature(url: string): string {
	const {base, query, fragment} = splitUrl(url);
	const preserved = preservedFields(query);
	if (preserved.length === 0) return `${base}${fragment}`;
	return `${base}?${preserved.join('&')}${fragment}`;
}

export function issueWindow(anchorSecs: number, nowSecs: number): AttachmentUrlWindow {
	const elapsed = Math.max(0, nowSecs - anchorSecs);
	const issued = anchorSecs + Math.floor(elapsed / ATTACHMENT_URL_BUCKET_SECS) * ATTACHMENT_URL_BUCKET_SECS;
	return {issued, expires: issued + ATTACHMENT_URL_TTL_SECS};
}

export function canonicalInput(storageKey: string, exHex: string, isHex: string, usage: AttachmentUrlUsage): string {
	return `${SIGNATURE_DOMAIN}\n${exHex}\n${isHex}\n${usage}\n${storageKey}`;
}

function windowHex(value: number): string {
	return value.toString(16).padStart(WINDOW_HEX_LENGTH, '0');
}

function signUsage(url: string, options: SignAttachmentUrlOptions, usage: AttachmentUrlUsage): string {
	const storageKey = attachmentStorageKeyFromUrl(url, options.mediaEndpoint);
	if (storageKey === null) return url;
	const {issued, expires} = issueWindow(options.anchorSecs, options.nowSecs);
	if (!Number.isSafeInteger(issued) || issued < 0 || expires > MAX_WINDOW_SECS) return url;
	const isDataPackage = usage === DATA_PACKAGE_USAGE;
	const exHex = windowHex(isDataPackage ? 0 : expires);
	const isHex = windowHex(issued);
	const signature = crypto
		.createHmac('sha256', options.secret)
		.update(canonicalInput(storageKey, exHex, isHex, usage))
		.digest('hex');
	const signatureFields = isDataPackage
		? `ex=${DATA_PACKAGE_EXPIRES}&is=${isHex}&hm=${signature}&uc=${DATA_PACKAGE_USAGE}`
		: `ex=${exHex}&is=${isHex}&hm=${signature}`;
	const {base, query, fragment} = splitUrl(url);
	const preserved = preservedFields(query);
	const fields = preserved.length === 0 ? signatureFields : `${signatureFields}&${preserved.join('&')}`;
	return `${base}?${fields}${fragment}`;
}

export function signAttachmentUrl(url: string, options: SignAttachmentUrlOptions): string {
	return signUsage(url, options, ORDINARY_USAGE);
}

export function signDataPackageAttachmentUrl(url: string, options: SignAttachmentUrlOptions): string {
	return signUsage(url, options, DATA_PACKAGE_USAGE);
}
