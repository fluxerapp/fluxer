// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {InvalidRequestError} from '@fluxer/errors/src/domains/core/InvalidRequestError';
import {z} from 'zod';

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
	const hasBodyCredentials = !!bodyClientIdStr || !!bodyClientSecret;
	if (authorizationHeader) {
		const parseResult = BasicAuthScheme.safeParse(authorizationHeader);
		if (parseResult.success) {
			if (hasBodyCredentials) {
				throw new InvalidRequestError();
			}
			try {
				const decoded = Buffer.from(parseResult.data, 'base64').toString('utf8');
				const colonIndex = decoded.indexOf(':');
				if (colonIndex >= 0) {
					const id = decoded.slice(0, colonIndex);
					const secret = decoded.slice(colonIndex + 1);
					return {
						clientId: id,
						clientSecret: secret || undefined,
					};
				}
			} catch (error) {
				Logger.debug({error}, 'Failed to decode basic auth credentials, falling back to body credentials');
			}
		}
	}
	return {
		clientId: bodyClientIdStr,
		clientSecret: bodyClientSecret,
	};
}
