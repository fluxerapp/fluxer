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

import {UsernameType} from '~/Schema';
import {transliterate as tr} from 'transliteration';

const MAX_USERNAME_LENGTH = 32;

function sanitizeDisplayName(globalName: string): string | null {
	const trimmed = globalName.trim();
	if (!trimmed) return null;

	let sanitized = tr(trimmed);
	sanitized = sanitized.replace(/[\s\-.]+/g, '_');
	sanitized = sanitized.replace(/[^a-zA-Z0-9_]/g, '');
	if (!sanitized) return null;
	if (sanitized.length > MAX_USERNAME_LENGTH) {
		sanitized = sanitized.substring(0, MAX_USERNAME_LENGTH);
	}

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
