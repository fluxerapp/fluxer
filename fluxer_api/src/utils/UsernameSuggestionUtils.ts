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

import {transliterate as tr} from 'transliteration';
import {generateRandomUsername} from '~/utils/UsernameGenerator';

function sanitizeForFluxerTag(input: string): string {
	let result = tr(input.trim());

	result = result.replace(/[\s\-.]+/g, '_');

	result = result.replace(/[^a-zA-Z0-9_]/g, '');

	if (!result) {
		result = 'user';
	}

	if (result.length > 32) {
		result = result.substring(0, 32);
	}

	return result.toLowerCase();
}

export function generateUsernameSuggestions(globalName: string, count: number = 5): Array<string> {
	const suggestions: Array<string> = [];

	const transliterated = tr(globalName.trim());
	const hasMeaningfulContent = /[a-zA-Z]/.test(transliterated);

	if (!hasMeaningfulContent) {
		for (let i = 0; i < count; i++) {
			const randomUsername = generateRandomUsername();
			const sanitizedRandom = sanitizeForFluxerTag(randomUsername);
			if (sanitizedRandom && sanitizedRandom.length <= 32) {
				suggestions.push(sanitizedRandom.toLowerCase());
			}
		}
		return Array.from(new Set(suggestions)).slice(0, count);
	}

	const baseUsername = sanitizeForFluxerTag(globalName);
	suggestions.push(baseUsername);

	const suffixes = ['_', '__', '___', '123', '_1', '_official', '_real'];
	for (const suffix of suffixes) {
		if (suggestions.length >= count) break;
		const suggestion = baseUsername + suffix;
		if (suggestion.length <= 32) {
			suggestions.push(suggestion);
		}
	}

	let counter = 2;
	while (suggestions.length < count) {
		const suggestion = `${baseUsername}${counter}`;
		if (suggestion.length <= 32) {
			suggestions.push(suggestion);
		}
		counter++;
	}

	return Array.from(new Set(suggestions)).slice(0, count);
}
