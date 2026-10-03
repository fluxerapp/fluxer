// SPDX-License-Identifier: AGPL-3.0-or-later

import {domainToASCII} from 'node:url';

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
// biome-ignore lint/complexity/useRegexLiterals: The literal form trips noControlCharactersInRegex for C0 controls.
const CONTROL_OR_WS_RE = new RegExp('[\\s\\x00-\\x1f]');

export function canonicalizeUrl(raw: string): string | null {
	const trimmed = raw.trim();
	if (!trimmed) return null;
	if (CONTROL_OR_WS_RE.test(trimmed)) return null;
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
	parsed.hostname = asciiHost.toLowerCase();
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
	kept.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
	parsed.search = '';
	for (const [k, v] of kept) {
		parsed.searchParams.append(k, v);
	}
	if (parsed.pathname === '') parsed.pathname = '/';
	return parsed.toString().toLowerCase();
}

const TRAILING_DOTS_RE = /\.+$/;

export function normalizeHostname(raw: string): string | null {
	const trimmed = raw.trim();
	if (!trimmed) return null;
	const ascii = domainToASCII(trimmed);
	if (!ascii) return null;
	const host = ascii.toLowerCase().replace(TRAILING_DOTS_RE, '');
	return host || null;
}

const URL_CANDIDATE_RE =
	/(?<![a-z0-9._+-]@)((?:https?:\/\/)?(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z]{2,}(?:[/:?#][^\s<>\u201D']*)?)/gi;
const TRAILING_PUNCT_RE = /[.,;:!?)\]}\x22'\u00bb\u201C\u201D]+$/;

export function extractUrlCandidates(text: string | null | undefined): Array<string> {
	if (!text) return [];
	const out: Array<string> = [];
	URL_CANDIDATE_RE.lastIndex = 0;
	let m: RegExpExecArray | null;
	while ((m = URL_CANDIDATE_RE.exec(text)) !== null) {
		const candidate = m[1];
		if (!candidate) continue;
		let cleaned = candidate.replace(TRAILING_PUNCT_RE, '');
		if (!/^https?:\/\//i.test(cleaned)) {
			cleaned = `http://${cleaned}`;
		}
		out.push(cleaned);
	}
	return out;
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
