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

import {z} from '~/Schema';

const BasicAuthScheme = z
	.string()
	.regex(/^Basic\s+/i)
	.transform((val) => val.replace(/^Basic\s+/i, ''));

interface ParsedClientCredentials {
	clientId: string;
	clientSecret?: string;
}

export function parseClientCredentials(
	authorizationHeader: string | undefined,
	bodyClientId?: bigint,
	bodyClientSecret?: string,
): ParsedClientCredentials {
	const bodyClientIdStr = bodyClientId?.toString() ?? '';

	if (authorizationHeader) {
		const parseResult = BasicAuthScheme.safeParse(authorizationHeader);
		if (parseResult.success) {
			try {
				const decoded = Buffer.from(parseResult.data, 'base64').toString('utf8');
				const colonIndex = decoded.indexOf(':');

				if (colonIndex >= 0) {
					const id = decoded.slice(0, colonIndex);
					const secret = decoded.slice(colonIndex + 1);

					return {
						clientId: id || bodyClientIdStr,
						clientSecret: secret || bodyClientSecret,
					};
				}
			} catch {}
		}
	}

	return {
		clientId: bodyClientIdStr,
		clientSecret: bodyClientSecret,
	};
}
