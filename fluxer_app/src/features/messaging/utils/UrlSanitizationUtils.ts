// SPDX-License-Identifier: AGPL-3.0-or-later

import {sanitizeTrackingURL} from '@app/features/messaging/utils/TrackingUrlSanitizer';
import UserSettings from '@app/features/user/state/UserSettings';

const CONTENT_SEGMENT_REGEX = /```[\s\S]*?```|``[^`]*``|`[^`\n]*`|\bhttps?:\/\/[^\s<>'"`)\]]+/gi;
const TRAILING_PUNCT_REGEX = /[.,;:!?]+$/;

function sanitizeURLsInContent(content: string): string {
	if (content.length === 0) return content;
	return content.replace(CONTENT_SEGMENT_REGEX, (match) => {
		if (match.startsWith('`')) return match;
		const trailing = TRAILING_PUNCT_REGEX.exec(match);
		let suffix: string;
		if (trailing != null) {
			suffix = trailing[0];
		} else {
			suffix = '';
		}
		let trimmed: string;
		if (suffix.length > 0) {
			trimmed = match.slice(0, -suffix.length);
		} else {
			trimmed = match;
		}
		return sanitizeTrackingURL(trimmed) + suffix;
	});
}

export function maybeSanitizeOutgoingMessage(content: string | null | undefined): string {
	if (content == null) return '';
	if (content.length === 0) return content;
	if (!UserSettings.getSanitizeUrls()) return content;
	return sanitizeURLsInContent(content);
}
