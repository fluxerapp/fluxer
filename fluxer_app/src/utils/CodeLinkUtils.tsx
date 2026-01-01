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

import * as RegexUtils from '~/utils/RegexUtils';

export interface CodeLinkConfig {
	shortHost: string;
	path: string;
}

const patternCache = new Map<string, RegExp>();

function createPattern(config: CodeLinkConfig): RegExp {
	const cacheKey = `${config.shortHost}:${config.path}`;

	let pattern = patternCache.get(cacheKey);
	if (pattern) {
		return pattern;
	}

	pattern = new RegExp(
		[
			'(?:https?:\\/\\/)?',
			'(?:',
			`${RegexUtils.escapeRegex(config.shortHost)}(?:\\/#)?\\/(?!${config.path}\\/)([a-zA-Z0-9\\-]{2,32})(?![a-zA-Z0-9\\-])`,
			'|',
			`${RegexUtils.escapeRegex(location.host)}(?:\\/#)?\\/${config.path}\\/([a-zA-Z0-9\\-]{2,32})(?![a-zA-Z0-9\\-])`,
			')',
		].join(''),
		'gi',
	);

	patternCache.set(cacheKey, pattern);
	return pattern;
}

export function findCodes(content: string | null, config: CodeLinkConfig): Array<string> {
	if (!content) return [];

	const codes: Array<string> = [];
	const seenCodes = new Set<string>();
	const pattern = createPattern(config);

	pattern.lastIndex = 0;

	let match: RegExpExecArray | null;
	while ((match = pattern.exec(content)) !== null && codes.length < 10) {
		const code = match[1] || match[2];
		if (code && !seenCodes.has(code)) {
			seenCodes.add(code);
			codes.push(code);
		}
	}

	return codes;
}

export function findCode(content: string | null, config: CodeLinkConfig): string | null {
	if (!content) return null;

	const pattern = createPattern(config);
	pattern.lastIndex = 0;
	const match = pattern.exec(content);

	if (match) {
		return match[1] || match[2];
	}

	return null;
}
