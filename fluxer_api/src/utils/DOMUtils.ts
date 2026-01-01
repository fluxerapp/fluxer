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

import {decode} from 'html-entities';
export function decodeHTMLEntities(html?: string | null): string {
	if (!html) return '';
	return decode(html);
}
export function stripHtmlTags(html?: string | null): string {
	if (!html) return '';
	return html.replace(/<[^>]*>/g, '');
}
export function htmlToMarkdown(html?: string | null): string {
	if (!html) return '';
	let md = html
		.replace(/<p>/gi, '\n\n')
		.replace(/<\/p>/gi, '')
		.replace(/<br\s*\/?>/gi, '\n')
		.replace(/<h[1-6]>/gi, '\n\n**')
		.replace(/<\/h[1-6]>/gi, '**\n\n')
		.replace(/<li>/gi, '• ')
		.replace(/<\/li>/gi, '\n')
		.replace(/<ul>|<ol>/gi, '\n')
		.replace(/<\/ul>|<\/ol>/gi, '\n')
		.replace(/<pre><code>([\s\S]*?)<\/code><\/pre>/gi, (_, code) => `\`\`\`\n${code}\n\`\`\``)
		.replace(/<code>([\s\S]*?)<\/code>/gi, '`$1`')
		.replace(/<strong>([\s\S]*?)<\/strong>/gi, '**$1**')
		.replace(/<b>([\s\S]*?)<\/b>/gi, '**$1**')
		.replace(/<em>([\s\S]*?)<\/em>/gi, '_$1_')
		.replace(/<i>([\s\S]*?)<\/i>/gi, '_$1_')
		.replace(/<a\s+(?:[^>]*?\s+)?href="([^"]*)"[^>]*>([\s\S]*?)<\/a>/gi, '[$2]($1)');
	md = stripHtmlTags(md);
	md = decodeHTMLEntities(md);
	return md
		.replace(/\n{3,}/g, '\n\n')
		.replace(/\s+$/, '')
		.trim();
}
