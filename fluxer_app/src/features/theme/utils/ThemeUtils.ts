// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as CodeLinkUtils from '@app/features/messaging/utils/CodeLinkUtils';
import {buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';

const OFFICIAL_THEME_URL_BASES = Object.freeze([
	'https://fluxer.app/theme',
	'https://canary.fluxer.app/theme',
	'https://web.fluxer.app/theme',
	'https://web.canary.fluxer.app/theme',
	'https://fluxer.com/theme',
	'https://canary.fluxer.com/theme',
]);
const appendThemePath = (endpoint: string | null | undefined): string | null => {
	if (!endpoint) return null;
	const trimmed = endpoint.replace(/\/$/, '');
	return trimmed ? `${trimmed}/theme` : null;
};
const THEME_CONFIG: CodeLinkUtils.CodeLinkConfig = {
	path: 'theme',
	get urlBases() {
		return [
			appendThemePath(RuntimeConfig.webAppBaseUrl),
			appendThemePath(RuntimeConfig.marketingEndpoint),
			...OFFICIAL_THEME_URL_BASES,
		];
	},
};

export function findThemes(content: string | null): Array<string> {
	return CodeLinkUtils.findCodes(content, THEME_CONFIG);
}

export function findTheme(content: string | null): string | null {
	return CodeLinkUtils.findCode(content, THEME_CONFIG);
}

export function findSpoileredThemes(content: string | null): Array<CodeLinkUtils.CodeLinkMatch> {
	return CodeLinkUtils.findSpoileredCodeMatches(content, THEME_CONFIG);
}

function buildThemeCssUrl(endpoint: string | null | undefined, themeId: string): string | null {
	if (!endpoint) return null;
	const base = endpoint.replace(/\/$/, '');
	return `${base}/themes/${themeId}.css`;
}

export function buildThemeCssProxyUrl(endpoint: string | null | undefined, themeId: string): string | null {
	const rawUrl = buildThemeCssUrl(endpoint, themeId);
	if (!rawUrl) return null;
	return buildMediaProxyURL(rawUrl);
}
