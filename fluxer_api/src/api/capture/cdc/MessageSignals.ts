// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';

export interface ContentSignals {
	length: number;
	hash: bigint;
	simhash: bigint;
	linkDomains: Array<string>;
	linkUrls: Array<string>;
	inviteCodes: Array<string>;
}

const MAX_LINKS = 16;
const MAX_URL_LENGTH = 512;
const MAX_TOKENS = 512;
const INVISIBLE = /[\u00AD\u180E\u200B-\u200F\u202A-\u202E\u2060-\u2064\uFEFF]/g;
const WHITESPACE = /\s+/g;
const TOKEN = /[\p{L}\p{N}]+/gu;
const URL_CANDIDATE = /https?:\/\/[^\s<>"'`|\\^{}]+/gi;
const BARE_INVITE = /(?<![\w.-])(fluxer\.gg|discord\.gg)\/([A-Za-z0-9-]{2,32})(?![A-Za-z0-9-])/gi;
const INVITE_CODE = /^[A-Za-z0-9-]{2,32}$/;
const SHORT_INVITE_HOSTS = new Set(['fluxer.gg', 'discord.gg']);
const INVITE_PATH_HOSTS = ['fluxer.app', 'fluxer.com', 'discord.com', 'discordapp.com'];
const TRAILING = new Set(['.', ',', ';', ':', '!', '?', ')', ']', '>', "'", '"', '*', '_', '~']);

export function normaliseContent(content: string): string {
	return content.normalize('NFKC').replace(INVISIBLE, '').toLowerCase().replace(WHITESPACE, ' ').trim();
}

function hash32(text: string, seed: number): number {
	let hash = seed;
	for (let index = 0; index < text.length; index++) {
		hash = Math.imul(hash ^ text.charCodeAt(index), 0x01000193);
	}
	hash ^= hash >>> 16;
	hash = Math.imul(hash, 0x85ebca6b);
	hash ^= hash >>> 13;
	hash = Math.imul(hash, 0xc2b2ae35);
	return (hash ^ (hash >>> 16)) >>> 0;
}

export function simhash(normalised: string): bigint {
	const tokens = (normalised.match(TOKEN) ?? []).slice(0, MAX_TOKENS);
	if (tokens.length === 0) return 0n;
	const weights = new Int32Array(64);
	const add = (feature: string) => {
		const high = hash32(feature, 0x811c9dc5);
		const low = hash32(feature, 0x9747b28c);
		for (let bit = 0; bit < 32; bit++) {
			weights[bit] += (low >>> bit) & 1 ? 1 : -1;
			weights[bit + 32] += (high >>> bit) & 1 ? 1 : -1;
		}
	};
	for (let index = 0; index < tokens.length; index++) {
		add(tokens[index]);
		if (index > 0) add(`${tokens[index - 1]} ${tokens[index]}`);
	}
	let low = 0;
	let high = 0;
	for (let bit = 0; bit < 32; bit++) {
		if (weights[bit] > 0) low |= 1 << bit;
		if (weights[bit + 32] > 0) high |= 1 << bit;
	}
	return (BigInt(high >>> 0) << 32n) | BigInt(low >>> 0);
}

function contentHash(normalised: string): bigint {
	if (normalised.length === 0) return 0n;
	return createHash('sha256').update(normalised).digest().readBigUInt64BE(0);
}

function trimTrailing(candidate: string): string {
	let end = candidate.length;
	while (end > 0 && TRAILING.has(candidate[end - 1])) end--;
	return candidate.slice(0, end);
}

function inviteOf(url: URL, host: string): string | null {
	const segments = url.pathname.split('/').filter((segment) => segment.length > 0);
	if (SHORT_INVITE_HOSTS.has(host)) {
		return segments.length > 0 && INVITE_CODE.test(segments[0]) ? `${host}/${segments[0]}` : null;
	}
	const known = INVITE_PATH_HOSTS.some((base) => host === base || host.endsWith(`.${base}`));
	if (!known || segments[0] !== 'invite' || segments.length < 2 || !INVITE_CODE.test(segments[1])) return null;
	return `${host}/${segments[1]}`;
}

function links(content: string): {domains: Array<string>; urls: Array<string>; invites: Array<string>} {
	const domains = new Set<string>();
	const urls = new Set<string>();
	const invites = new Set<string>();
	for (const match of content.matchAll(URL_CANDIDATE)) {
		if (urls.size >= MAX_LINKS) break;
		let url: URL;
		try {
			url = new URL(trimTrailing(match[0]));
		} catch {
			continue;
		}
		const host = url.hostname.replace(/^www\./, '');
		if (host.length === 0) continue;
		domains.add(host);
		urls.add(url.href.slice(0, MAX_URL_LENGTH));
		const invite = inviteOf(url, host);
		if (invite !== null) invites.add(invite);
	}
	for (const match of content.matchAll(BARE_INVITE)) {
		if (invites.size >= MAX_LINKS) break;
		const host = match[1].toLowerCase();
		domains.add(host);
		invites.add(`${host}/${match[2]}`);
	}
	return {domains: [...domains], urls: [...urls], invites: [...invites]};
}

export function contentSignals(content: string | null | undefined): ContentSignals {
	if (content === null || content === undefined || content.length === 0) {
		return {length: 0, hash: 0n, simhash: 0n, linkDomains: [], linkUrls: [], inviteCodes: []};
	}
	const normalised = normaliseContent(content);
	const found = links(content);
	return {
		length: content.length,
		hash: contentHash(normalised),
		simhash: simhash(normalised),
		linkDomains: found.domains,
		linkUrls: found.urls,
		inviteCodes: found.invites,
	};
}
