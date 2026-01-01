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

export function isTextInputKeyEvent(event: KeyboardEvent): boolean {
	const {key, ctrlKey, metaKey} = event;

	if (!key || key === 'Unidentified') {
		return false;
	}

	if (ctrlKey || metaKey) {
		return false;
	}

	if (key === 'Dead') {
		return true;
	}

	if (key.length > 1 && NAMED_KEY_PATTERN.test(key)) {
		return false;
	}

	const firstCodePoint = key.codePointAt(0)!;
	if (firstCodePoint <= 0x1f || (firstCodePoint >= 0x7f && firstCodePoint <= 0x9f)) {
		return false;
	}

	return true;
}

const NAMED_KEY_PATTERN = /^[A-Z][A-Za-z0-9]*$/;
