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

import {selectOne} from 'css-select';
import type {Element} from 'domhandler';
import {parseDocument} from 'htmlparser2';
import {Logger} from '~/Logger';

export function extractPostId(url: URL): string | null {
	const mastodonMatch = url.pathname.match(/\/@([^/]+)\/(\w+)/);
	if (mastodonMatch) return mastodonMatch[2];
	const altMastodonMatch = url.pathname.match(/\/users\/[^/]+\/status(?:es)?\/(\w+)/);
	if (altMastodonMatch) return altMastodonMatch[1];
	const genericMatch = url.pathname.match(/\/[^/]+\/status(?:es)?\/(\w+)/);
	if (genericMatch) return genericMatch[1];
	const pleromaMatch = url.pathname.match(/\/notice\/([a-zA-Z0-9]+)/);
	if (pleromaMatch) return pleromaMatch[1];
	const misskeyMatch = url.pathname.match(/\/notes\/([a-zA-Z0-9]+)/);
	if (misskeyMatch) return misskeyMatch[1];
	Logger.debug({url: url.toString()}, 'Could not extract post ID from URL');
	return null;
}

export function extractAppleTouchIcon(html: string, url: URL): string | undefined {
	Logger.debug({url: url.toString()}, 'Attempting to extract apple touch icon');
	try {
		const document = parseDocument(html);
		const appleTouchIcon180 = selectOne('link[rel="apple-touch-icon"][sizes="180x180"]', document) as Element | null;
		if (appleTouchIcon180?.attribs.href) {
			const iconPath = appleTouchIcon180.attribs.href;
			const fullPath = iconPath.startsWith('http') ? iconPath : new URL(iconPath, url.origin).toString();
			Logger.debug({iconPath, fullPath}, 'Found 180x180 apple touch icon');
			return fullPath;
		}
		const anyAppleTouchIcon = selectOne('link[rel="apple-touch-icon"]', document) as Element | null;
		if (anyAppleTouchIcon?.attribs.href) {
			const iconPath = anyAppleTouchIcon.attribs.href;
			const fullPath = iconPath.startsWith('http') ? iconPath : new URL(iconPath, url.origin).toString();
			Logger.debug({iconPath, fullPath}, 'Found fallback apple touch icon');
			return fullPath;
		}
		Logger.debug('No apple touch icon found');
		return;
	} catch (error) {
		Logger.error({error}, 'Error parsing HTML for apple touch icon');
		return;
	}
}

export function escapeMarkdownChars(text: string): string {
	return text
		.replace(/\\\[/g, '\\[')
		.replace(/\\\]/g, '\\]')
		.replace(/\\\(/g, '\\(')
		.replace(/\\\)/g, '\\)')
		.replace(/\\-/g, '\\-');
}
