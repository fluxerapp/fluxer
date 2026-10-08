// SPDX-License-Identifier: AGPL-3.0-or-later

import {isLinkWrappedInAngleBrackets} from '@app/features/messaging/utils/LinkSuppressionUtils';
import * as RegexUtils from '@app/features/messaging/utils/RegexUtils';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';

export interface CodeLinkConfig {
	path: string;
	urlBases: ReadonlyArray<string | null | undefined>;
}

export interface CodeLinkMatch {
	code: string;
	matchedText: string;
	index: number;
}

const patternCache = new Map<string, RegExp>();
const CODE_REGEX = '([a-zA-Z0-9\\-]{2,32})(?![a-zA-Z0-9\\-])';
const SPOILER_REGEX = /\|\|([\s\S]*?)\|\|/g;

function normalizeUrlBase(urlBase: string | null | undefined): string | null {
	if (!urlBase) return null;
	const trimmed = urlBase.trim().replace(/\/+$/, '');
	if (!trimmed) return null;
	try {
		const parsed = new URL(trimmed);
		const normalizedPath = parsed.pathname === '/' ? '' : parsed.pathname.replace(/\/+$/, '');
		return `${parsed.host}${normalizedPath}`;
	} catch {
		return trimmed.replace(/^https?:\/\//i, '');
	}
}

function createPattern(config: CodeLinkConfig): RegExp {
	const normalizedBases = new Set<string>();
	for (const urlBase of config.urlBases) {
		const normalized = normalizeUrlBase(urlBase);
		if (normalized) {
			normalizedBases.add(normalized);
		}
	}
	if (typeof location !== 'undefined' && location.host && !isDesktopLocalAppDocument()) {
		normalizedBases.add(`${location.host}/${config.path}`);
	}
	const cacheKey = Array.from(normalizedBases).sort().join('|');
	let pattern = patternCache.get(cacheKey);
	if (pattern) {
		return pattern;
	}
	const branches: Array<string> = [];
	const orderedBases = Array.from(normalizedBases).sort((a, b) => b.length - a.length || a.localeCompare(b));
	for (const base of orderedBases) {
		const slashIndex = base.indexOf('/');
		const host = slashIndex === -1 ? base : base.slice(0, slashIndex);
		const path = slashIndex === -1 ? '' : base.slice(slashIndex);
		branches.push(`${RegexUtils.escapeRegex(host)}(?:\\/#)?${RegexUtils.escapeRegex(path)}`);
	}
	pattern = new RegExp(['(?:https?:\\/\\/)?', '(?:', branches.join('|'), ')\\/', CODE_REGEX].join(''), 'gi');
	patternCache.set(cacheKey, pattern);
	return pattern;
}

function findCodeMatchesInternal(
	content: string | null,
	config: CodeLinkConfig,
	options?: {
		dedupeCodes?: boolean;
		limit?: number;
	},
): Array<CodeLinkMatch> {
	if (!content) return [];
	const matches: Array<CodeLinkMatch> = [];
	const seenCodes = new Set<string>();
	const dedupeCodes = options?.dedupeCodes ?? true;
	const limit = options?.limit ?? 10;
	const pattern = createPattern(config);
	pattern.lastIndex = 0;
	let match: RegExpExecArray | null;
	while ((match = pattern.exec(content)) !== null && matches.length < limit) {
		const matchedText = match[0];
		if (isLinkWrappedInAngleBrackets(content, match.index ?? 0, matchedText.length)) {
			continue;
		}
		const code = match[1];
		if (code && (!dedupeCodes || !seenCodes.has(code))) {
			if (dedupeCodes) {
				seenCodes.add(code);
			}
			matches.push({code, matchedText, index: match.index});
		}
	}
	return matches;
}

export function findCodes(content: string | null, config: CodeLinkConfig): Array<string> {
	return findCodeMatches(content, config).map((match) => match.code);
}

export function findCodeMatches(content: string | null, config: CodeLinkConfig): Array<CodeLinkMatch> {
	return findCodeMatchesInternal(content, config);
}

export function findCode(content: string | null, config: CodeLinkConfig): string | null {
	return findCodeMatches(content, config)[0]?.code ?? null;
}

export function findSpoileredCodeMatches(content: string | null, config: CodeLinkConfig): Array<CodeLinkMatch> {
	if (!content) return [];
	const matches: Array<CodeLinkMatch> = [];
	for (const spoilerMatch of content.matchAll(SPOILER_REGEX)) {
		const spoilerBody = spoilerMatch[1];
		if (!spoilerBody) continue;
		for (const codeMatch of findCodeMatchesInternal(spoilerBody, config, {
			dedupeCodes: false,
			limit: Number.POSITIVE_INFINITY,
		})) {
			matches.push({
				...codeMatch,
				index: (spoilerMatch.index ?? 0) + 2 + codeMatch.index,
			});
		}
	}
	return matches;
}
