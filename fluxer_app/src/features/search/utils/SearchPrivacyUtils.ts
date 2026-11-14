// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchHistoryEntry} from '@app/features/search/state/SearchHistory';
import {
	isQuoteChar,
	type SearchHints,
	splitCSV,
	tokenize,
	tryResolveUser,
} from '@app/features/search/utils/SearchQueryParser';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {isProbablyAValidSnowflake} from '@fluxer/snowflake/src/SnowflakeUtils';

const USER_FILTER_KEYS = new Set(['from', 'mentions']);

function unquoteTokenValue(value: string): string {
	const trimmed = value.trim();
	if (trimmed.length < 2) return trimmed;
	const first = trimmed[0];
	const last = trimmed[trimmed.length - 1];
	if ((first === '"' && last === '"') || (first === "'" && last === "'")) {
		return trimmed.slice(1, -1);
	}
	return trimmed;
}

const UNREDACTED_USER_VALUES = new Set(['@me', 'everyone', 'here', '@everyone', '@here']);

function shouldRedactUserValue(value: string): boolean {
	return !isProbablyAValidSnowflake(value) && !UNREDACTED_USER_VALUES.has(value.toLowerCase());
}

function quoteFilterValuePart(part: string): string {
	if (!/[\s,\\]/.test(part) && !Array.from(part).some(isQuoteChar)) return part;
	const escaped = Array.from(part)
		.map((ch) => (ch === '\\' || isQuoteChar(ch) ? `\\${ch}` : ch))
		.join('');
	return `"${escaped}"`;
}

export function formatSearchHistoryEntryForStreamerMode(entry: SearchHistoryEntry): SearchHistoryEntry {
	const {tokens} = tokenize(entry.query);
	const nextUsersByTag: Record<string, string> = {...entry.hints?.usersByTag};
	let query = entry.query;
	for (let index = tokens.length - 1; index >= 0; index--) {
		const token = tokens[index];
		const normalizedFilterKey = token.key.startsWith('-') ? token.key.slice(1) : token.key;
		if (!USER_FILTER_KEYS.has(normalizedFilterKey)) continue;
		let changed = false;
		const parts = splitCSV(token.value).map((part) => {
			if (!shouldRedactUserValue(part)) return part;
			const formatted = NicknameUtils.formatTagForStreamerMode(part);
			if (formatted === part) return part;
			changed = true;
			const userId = tryResolveUser(part, entry.hints);
			if (userId) nextUsersByTag[formatted] = userId;
			return formatted;
		});
		if (!changed) continue;
		const valueStart = token.end - token.value.length;
		query = query.slice(0, valueStart) + parts.map(quoteFilterValuePart).join(',') + query.slice(token.end);
	}
	if (query === entry.query) {
		return entry;
	}
	const hints: SearchHints = {
		...entry.hints,
		usersByTag: nextUsersByTag,
	};
	return {...entry, query, hints};
}

export function buildSearchSegmentsFromHints(query: string, hints?: SearchHints): Array<SearchSegment> {
	const segments: Array<SearchSegment> = [];
	if (!hints) {
		return segments;
	}
	const {tokens} = tokenize(query);
	for (const token of tokens) {
		const filterKey = token.exclude ? `-${token.key}` : token.key;
		const normalizedFilterKey = token.key.startsWith('-') ? token.key.slice(1) : token.key;
		const value = unquoteTokenValue(token.value);
		if (USER_FILTER_KEYS.has(normalizedFilterKey)) {
			const userId = hints.usersByTag?.[value];
			if (userId) {
				segments.push({
					type: 'user',
					filterKey,
					id: userId,
					displayText: token.raw,
					start: token.start,
					end: token.end,
				});
			}
			continue;
		}
		if (normalizedFilterKey === 'in') {
			const channelId = hints.channelsByName?.[value];
			if (channelId) {
				segments.push({
					type: 'channel',
					filterKey,
					id: channelId,
					displayText: token.raw,
					start: token.start,
					end: token.end,
				});
			}
		}
	}
	return segments;
}
