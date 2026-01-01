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

import {adjectives, animals, colors, uniqueNamesGenerator} from 'unique-names-generator';

export function generateRandomUsername(): string {
	const MAX_LENGTH = 32;
	const MAX_ATTEMPTS = 100;

	for (let i = 0; i < MAX_ATTEMPTS; i++) {
		const username = uniqueNamesGenerator({
			dictionaries: [adjectives, colors, animals],
			separator: '',
			style: 'capital',
			length: 3,
		});

		if (username.length <= MAX_LENGTH) {
			return username;
		}
	}

	for (let i = 0; i < MAX_ATTEMPTS; i++) {
		const username = uniqueNamesGenerator({
			dictionaries: [adjectives, animals],
			separator: '',
			style: 'capital',
			length: 2,
		});

		if (username.length <= MAX_LENGTH) {
			return username;
		}
	}

	for (let i = 0; i < MAX_ATTEMPTS; i++) {
		const username = uniqueNamesGenerator({
			dictionaries: [animals],
			separator: '',
			style: 'capital',
			length: 1,
		});

		if (username.length <= MAX_LENGTH) {
			return username;
		}
	}

	return uniqueNamesGenerator({
		dictionaries: [animals],
		separator: '',
		style: 'capital',
		length: 1,
	});
}
