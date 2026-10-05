// SPDX-License-Identifier: AGPL-3.0-or-later

import {UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {transliterate as tr} from 'transliteration';

const MAX_USERNAME_LENGTH = 32;

export function normaliseUsernameCandidate(value: string): string {
	const trimmed = value.trim();
	if (!trimmed) return '';
	let sanitized = tr(trimmed);
	sanitized = sanitized.replace(/[\s\-.]+/g, '_');
	sanitized = sanitized.replace(/[^a-zA-Z0-9_]/g, '');
	if (sanitized.length > MAX_USERNAME_LENGTH) {
		sanitized = sanitized.substring(0, MAX_USERNAME_LENGTH);
	}
	return sanitized;
}

function sanitizeDisplayName(globalName: string): string | null {
	const sanitized = normaliseUsernameCandidate(globalName);
	if (!sanitized) return null;
	const validation = UsernameType.safeParse(sanitized);
	if (!validation.success) {
		return null;
	}
	return sanitized;
}

export function deriveUsernameFromDisplayName(globalName: string): string | null {
	return sanitizeDisplayName(globalName);
}

export function generateUsernameSuggestions(globalName: string): Array<string> {
	const candidate = deriveUsernameFromDisplayName(globalName);
	return candidate ? [candidate] : [];
}
