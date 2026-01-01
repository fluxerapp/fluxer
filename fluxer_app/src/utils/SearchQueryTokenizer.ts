/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

const KNOWN_FILTER_PREFIXES = new Set([
	'from:',
	'-from:',
	'mentions:',
	'-mentions:',
	'in:',
	'-in:',
	'before:',
	'during:',
	'on:',
	'after:',
	'has:',
	'-has:',
	'pinned:',
	'author-type:',
	'sort:',
	'order:',
	'nsfw:',
	'embed-type:',
	'-embed-type:',
	'embed-provider:',
	'-embed-provider:',
	'link:',
	'-link:',
	'filename:',
	'-filename:',
	'ext:',
	'-ext:',
	'last:',
	'beforeid:',
	'afterid:',
	'any:',
	'scope:',
]);

const isFilterPrefix = (text: string): boolean => {
	const lower = text.toLowerCase();
	for (const prefix of KNOWN_FILTER_PREFIXES) {
		if (lower.startsWith(prefix)) {
			return true;
		}
	}
	return false;
};

const skipFilterValue = (query: string, startIndex: number): number => {
	let i = startIndex;
	const n = query.length;
	let inQuotes = false;
	let escaped = false;

	while (i < n) {
		const ch = query[i];
		if (escaped) {
			escaped = false;
			i++;
			continue;
		}
		if (ch === '\\') {
			escaped = true;
			i++;
			continue;
		}
		if (ch === '"') {
			inQuotes = !inQuotes;
			i++;
			continue;
		}
		if (!inQuotes && ch === ' ') {
			break;
		}
		i++;
	}
	return i;
};

export const tokenizeSearchQuery = (query: string): Array<string> => {
	const tokens: Array<string> = [];
	const n = query.length;
	let i = 0;

	while (i < n) {
		while (i < n && query[i] === ' ') {
			i++;
		}
		if (i >= n) {
			break;
		}

		const remaining = query.slice(i);
		if (isFilterPrefix(remaining)) {
			const colonIndex = remaining.indexOf(':');
			if (colonIndex !== -1) {
				i = skipFilterValue(query, i + colonIndex + 1);
				continue;
			}
		}

		if (query[i] === '"') {
			i++;
			let token = '';
			let escaped = false;
			while (i < n) {
				const ch = query[i];
				if (escaped) {
					token += ch;
					escaped = false;
					i++;
					continue;
				}
				if (ch === '\\') {
					escaped = true;
					i++;
					continue;
				}
				if (ch === '"') {
					i++;
					break;
				}
				token += ch;
				i++;
			}
			const trimmed = token.trim();
			if (trimmed) {
				tokens.push(trimmed);
			}
			continue;
		}

		let token = '';
		while (i < n && query[i] !== ' ' && query[i] !== '"') {
			token += query[i];
			i++;
		}
		const trimmed = token.trim();
		if (trimmed) {
			tokens.push(trimmed);
		}
	}

	return tokens;
};
