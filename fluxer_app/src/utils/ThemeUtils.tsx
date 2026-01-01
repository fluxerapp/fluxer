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

import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import * as RegexUtils from '~/utils/RegexUtils';

const THEME_ID_REGEX = '[a-zA-Z0-9-]{2,32}';

const normalizeEndpoint = (endpoint: string | null | undefined): string | null => {
	if (!endpoint) return null;
	const trimmed = endpoint.replace(/\/$/, '');
	return trimmed || null;
};

const buildThemePrefixes = (): Array<string> => {
	const prefixes = new Set<string>();

	const webApp = normalizeEndpoint(RuntimeConfigStore.webAppBaseUrl);
	if (webApp) {
		prefixes.add(`${webApp}/theme/`);
	}

	const marketingEndpoint = normalizeEndpoint(RuntimeConfigStore.marketingEndpoint);
	if (marketingEndpoint) {
		prefixes.add(`${marketingEndpoint}/theme/`);
	}

	return Array.from(prefixes);
};

const createThemeRegex = (): RegExp | null => {
	const prefixes = buildThemePrefixes();
	if (prefixes.length === 0) return null;

	const escapedPrefix = prefixes.map((prefix) => RegexUtils.escapeRegex(prefix)).join('|');
	return new RegExp(`(?:${escapedPrefix})(${THEME_ID_REGEX})(?![a-zA-Z0-9-])`, 'gi');
};

const matchThemes = (content: string | null, maxMatches = 1): Array<string> => {
	const regex = createThemeRegex();
	if (!regex || !content) return [];

	const codes: Array<string> = [];
	const seen = new Set<string>();
	regex.lastIndex = 0;

	let match: RegExpExecArray | null;
	while ((match = regex.exec(content)) !== null && codes.length < maxMatches) {
		const code = match[1];
		if (code && !seen.has(code)) {
			seen.add(code);
			codes.push(code);
		}
	}

	return codes;
};

export function findThemes(content: string | null): Array<string> {
	return matchThemes(content, 10);
}

export function findTheme(content: string | null): string | null {
	const matches = matchThemes(content, 1);
	return matches[0] ?? null;
}
