// SPDX-License-Identifier: AGPL-3.0-or-later

import {domainToASCII} from 'node:url';
import {compareText} from '@app/api/utils/StringComparison';

const TRACKING_PARAMS = new Set([
	'gclid',
	'fbclid',
	'mc_cid',
	'mc_eid',
	'msclkid',
	'_ga',
	'_gl',
	'igshid',
	'ref_src',
	'ref_url',
]);
const FIRST_PRINTABLE_ASCII_CODE = 0x20;
const WHITESPACE_RE = /\s/;
const TRAILING_DOTS_RE = /\.+$/;
export const MAX_HOSTNAME_LENGTH = 253;

export function canonicalizeUrl(raw: string): string | null {
	const trimmed = raw.trim();
	if (!trimmed) return null;
	const checked = `${trimmed}`;
	for (let index = 0; index < checked.length; index++) {
		if (checked.charCodeAt(index) < FIRST_PRINTABLE_ASCII_CODE) {
			return null;
		}
	}
	if (WHITESPACE_RE.test(checked)) return null;
	let parsed: URL;
	try {
		parsed = new URL(trimmed);
	} catch {
		return null;
	}
	if (parsed.protocol !== 'http:' && parsed.protocol !== 'https:') {
		return null;
	}
	const asciiHost = domainToASCII(parsed.hostname);
	if (!asciiHost) return null;
	const host = asciiHost.toLowerCase().replace(TRAILING_DOTS_RE, '');
	if (!host || host.length > MAX_HOSTNAME_LENGTH) return null;
	parsed.hostname = host;
	parsed.username = '';
	parsed.password = '';
	if (
		(parsed.protocol === 'http:' && parsed.port === '80') ||
		(parsed.protocol === 'https:' && parsed.port === '443')
	) {
		parsed.port = '';
	}
	parsed.hash = '';
	const params = parsed.searchParams;
	const kept: Array<[string, string]> = [];
	for (const [k, v] of params.entries()) {
		const kl = k.toLowerCase();
		if (kl.startsWith('utm_') || TRACKING_PARAMS.has(kl)) {
			continue;
		}
		kept.push([k, v]);
	}
	kept.sort(([a], [b]) => compareText(a, b));
	parsed.search = '';
	for (const [k, v] of kept) {
		parsed.searchParams.append(k, v);
	}
	if (parsed.pathname === '') parsed.pathname = '/';
	return parsed.toString().toLowerCase();
}

export function normalizeHostname(raw: string): string | null {
	const trimmed = raw.trim();
	if (!trimmed) return null;
	const ascii = domainToASCII(trimmed);
	if (!ascii) return null;
	const host = ascii.toLowerCase().replace(TRAILING_DOTS_RE, '');
	if (!host || host.length > MAX_HOSTNAME_LENGTH) return null;
	return host;
}

const URL_CANDIDATE_RE =
	/(?<![\p{L}\p{M}\p{N}-]|[\p{L}\p{N}._+-]@)((?:https?:\/\/)?[\p{L}\p{M}\p{N}-]+(?:[.\u3002\uFF0E\uFF61][\p{L}\p{M}\p{N}-]+)+\.?(?:[/:?#][^\s<>\u201D']*)?)/giu;
const SCHEME_URL_RE = /https?:\/\/[^\s<>\u201D']+/giu;
const TRAILING_PUNCT_RE = /(?<![.,;:!?)\]}\x22'\u00bb\u201C\u201D])[.,;:!?)\]}\x22'\u00bb\u201C\u201D]+$/;
const SCHEME_RE = /^https?:\/\//i;
const HOST_END_RE = /[/:?#]/;
const LABEL_SEPARATOR_RE = /[.\u3002\uFF0E\uFF61]/;
const NUMERIC_LABEL_RE = /^(?:0x[0-9a-f]*|\d+)$/i;
const IPV4_OCTET_RE = /^(?:0|[1-9]\d{0,2})$/;

function isPlainHostCandidate(candidate: string): boolean {
	const hostEnd = candidate.search(HOST_END_RE);
	const labels = (hostEnd === -1 ? candidate : candidate.slice(0, hostEnd)).split(LABEL_SEPARATOR_RE);
	if (!NUMERIC_LABEL_RE.test(labels[labels.length - 1] ?? '')) return true;
	return labels.length === 4 && labels.every((label) => IPV4_OCTET_RE.test(label) && Number(label) <= 255);
}

export function extractUrlCandidates(text: string | null | undefined): Array<string> {
	if (!text) return [];
	const out = new Set<string>();
	for (const match of text.matchAll(URL_CANDIDATE_RE)) {
		const candidate = match[1]?.replace(TRAILING_PUNCT_RE, '');
		if (!candidate) continue;
		if (SCHEME_RE.test(candidate)) {
			out.add(candidate);
		} else if (isPlainHostCandidate(candidate)) {
			out.add(`http://${candidate}`);
		}
	}
	for (const match of text.matchAll(SCHEME_URL_RE)) {
		const candidate = match[0].replace(TRAILING_PUNCT_RE, '');
		if (candidate) out.add(candidate);
	}
	return [...out];
}

const LINK_AUTHORITY_RE = /https?:\/\/([^\s/\\?#<>"'`()[\]{}|^]+)/giu;

function hostFromAuthority(authority: string): string | null {
	const cleaned = authority.replace(TRAILING_PUNCT_RE, '');
	if (!cleaned) return null;
	let parsed: URL;
	try {
		parsed = new URL(`http://${cleaned}/`);
	} catch {
		return null;
	}
	return normalizeHostname(parsed.hostname);
}

export function extractLinkHosts(text: string | null | undefined): Array<string> {
	if (!text) return [];
	const hosts = new Set<string>();
	for (const match of text.matchAll(LINK_AUTHORITY_RE)) {
		const authority = match[1];
		if (!authority) continue;
		const host = hostFromAuthority(authority);
		if (host) hosts.add(host);
	}
	return [...hosts];
}
