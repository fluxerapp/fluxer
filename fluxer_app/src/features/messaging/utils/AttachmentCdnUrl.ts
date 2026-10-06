// SPDX-License-Identifier: AGPL-3.0-or-later

export const ATTACHMENT_URL_REFRESH_MARGIN_MS = 3_600_000;

export interface EndpointInfo {
	basePath: string;
	origin: string;
}

export interface AttachmentUrlSignature {
	expiresAtMs: number;
}

const ATTACHMENT_PATH_SEGMENT = '/attachments/';
const WINDOW_HEX_PATTERN = /^[0-9a-f]{8}$/u;
const MAC_HEX_PATTERN = /^[0-9a-f]{64}$/u;
const DIGITS_PATTERN = /^[0-9]+$/u;
const PLUS_PATTERN = /\+/gu;
const DATA_PACKAGE_USE_CASE = 'dp';
const DATA_PACKAGE_EXPIRY = '0';

export function parseEndpoint(endpoint: string): EndpointInfo | null {
	if (!endpoint) return null;
	try {
		const parsedEndpoint = new URL(endpoint);
		const basePath =
			parsedEndpoint.pathname.length > 1 && parsedEndpoint.pathname.endsWith('/')
				? parsedEndpoint.pathname.slice(0, -1)
				: parsedEndpoint.pathname || '/';
		return {
			basePath,
			origin: parsedEndpoint.origin,
		};
	} catch {
		return null;
	}
}

export function isUrlOnEndpoint(targetUrl: URL, endpoint: EndpointInfo): boolean {
	if (targetUrl.origin !== endpoint.origin) return false;
	if (endpoint.basePath === '/') return true;
	return targetUrl.pathname === endpoint.basePath || targetUrl.pathname.startsWith(`${endpoint.basePath}/`);
}

export function isAttachmentCdnUrl(url: string, mediaEndpoint: string): boolean {
	if (!url.includes(ATTACHMENT_PATH_SEGMENT)) return false;
	const endpoint = parseEndpoint(mediaEndpoint);
	if (endpoint === null) return false;
	let parsed: URL;
	try {
		parsed = new URL(url);
	} catch {
		return false;
	}
	if (parsed.username !== '' || parsed.password !== '') return false;
	if (parsed.origin !== endpoint.origin) return false;
	const prefix = endpoint.basePath === '/' ? ATTACHMENT_PATH_SEGMENT : `${endpoint.basePath}${ATTACHMENT_PATH_SEGMENT}`;
	if (!parsed.pathname.startsWith(prefix)) return false;
	const segments = parsed.pathname.slice(prefix.length).split('/');
	if (segments.length < 3) return false;
	const [channelId, attachmentId] = segments;
	return (
		DIGITS_PATTERN.test(channelId) &&
		DIGITS_PATTERN.test(attachmentId) &&
		segments.slice(2).every((segment) => segment.length > 0)
	);
}

export function attachmentCacheKey(url: string): string | null {
	try {
		const parsed = new URL(url);
		return `${parsed.origin}${parsed.pathname}`;
	} catch {
		return null;
	}
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

function formDecodeName(name: string): string {
	try {
		return decodeURIComponent(name.replace(PLUS_PATTERN, ' '));
	} catch {
		return name;
	}
}

function fieldName(field: string): string {
	const separator = field.indexOf('=');
	return formDecodeName(separator < 0 ? field : field.slice(0, separator));
}

function fieldValue(field: string): string {
	const separator = field.indexOf('=');
	return separator < 0 ? '' : field.slice(separator + 1);
}

type SignatureFieldName = 'ex' | 'is' | 'hm' | 'uc';

function isSignatureFieldName(name: string): name is SignatureFieldName {
	return name === 'ex' || name === 'is' || name === 'hm' || name === 'uc';
}

function preservedFields(query: string): Array<string> {
	if (query.length === 0) return [];
	return query.split('&').filter((field) => {
		if (field.length === 0 || field === '=') return false;
		return !isSignatureFieldName(fieldName(field));
	});
}

interface ScannedSignature {
	ex: Array<string>;
	is: Array<string>;
	hm: Array<string>;
	uc: Array<string>;
}

function scanSignatureFields(url: string): ScannedSignature {
	const found: ScannedSignature = {ex: [], is: [], hm: [], uc: []};
	const {query} = splitUrl(url);
	if (query.length === 0) return found;
	for (const field of query.split('&')) {
		if (field.length === 0) continue;
		const name = fieldName(field);
		if (!isSignatureFieldName(name)) continue;
		found[name].push(fieldValue(field));
	}
	return found;
}

interface AttachmentSignatureFields {
	ex: string;
	is: string;
	hm: string;
	uc: string;
	expiresAtMs: number;
}

function parseAttachmentSignature(url: string): AttachmentSignatureFields | null {
	const found = scanSignatureFields(url);
	if (found.ex.length !== 1 || found.is.length !== 1 || found.hm.length !== 1 || found.uc.length > 1) return null;
	const ex = found.ex[0];
	const is = found.is[0];
	const hm = found.hm[0];
	if (!WINDOW_HEX_PATTERN.test(is) || !MAC_HEX_PATTERN.test(hm)) return null;
	if (found.uc.length === 1) {
		if (found.uc[0] !== DATA_PACKAGE_USE_CASE || ex !== DATA_PACKAGE_EXPIRY) return null;
		return {ex, is, hm, uc: DATA_PACKAGE_USE_CASE, expiresAtMs: Number.POSITIVE_INFINITY};
	}
	if (!WINDOW_HEX_PATTERN.test(ex)) return null;
	return {ex, is, hm, uc: '', expiresAtMs: Number.parseInt(ex, 16) * 1000};
}

export function readAttachmentUrlSignature(url: string): AttachmentUrlSignature | null {
	const signature = parseAttachmentSignature(url);
	return signature === null ? null : {expiresAtMs: signature.expiresAtMs};
}

export function readAttachmentSignatureFields(url: string): string | null {
	const signature = parseAttachmentSignature(url);
	if (signature === null) return null;
	const fields = `ex=${signature.ex}&is=${signature.is}&hm=${signature.hm}`;
	return signature.uc === '' ? fields : `${fields}&uc=${signature.uc}`;
}

export function stripAttachmentSignature(url: string): string {
	const {base, query, fragment} = splitUrl(url);
	const preserved = preservedFields(query);
	if (preserved.length === 0) return `${base}${fragment}`;
	return `${base}?${preserved.join('&')}${fragment}`;
}

export function applyAttachmentSignature(url: string, signatureFields: string): string {
	const {base, query, fragment} = splitUrl(url);
	const preserved = preservedFields(query);
	if (preserved.length === 0) return `${base}?${signatureFields}${fragment}`;
	return `${base}?${signatureFields}&${preserved.join('&')}${fragment}`;
}

export function attachmentUrlNeedsRefresh(url: string, nowMs: number): boolean {
	const signature = readAttachmentUrlSignature(url);
	if (signature === null) return true;
	return signature.expiresAtMs <= nowMs + ATTACHMENT_URL_REFRESH_MARGIN_MS;
}
