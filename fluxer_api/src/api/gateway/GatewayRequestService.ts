// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {BotAuthService} from '@app/api/oauth/BotAuthService';
import {InvalidGatewayAuthTokenError} from '@fluxer/errors/src/domains/auth/InvalidGatewayAuthTokenError';
import {MissingGatewayAuthorizationError} from '@fluxer/errors/src/domains/auth/MissingGatewayAuthorizationError';
import type {GatewayBotResponse as GatewayBotResponseType} from '@fluxer/schema/src/domains/gateway/GatewaySchemas';

type TokenType = 'user' | 'bot' | 'unknown';

function parseTokenType(raw: string): TokenType {
	if (raw.startsWith('flx_')) return 'user';
	const dotIndex = raw.indexOf('.');
	if (dotIndex > 0 && dotIndex < raw.length - 1) {
		const beforeDot = raw.slice(0, dotIndex);
		if (/^\d+$/.test(beforeDot)) return 'bot';
	}
	return 'unknown';
}

function extractToken(authHeader: string | null): string {
	if (!authHeader) return '';
	const lower = authHeader.toLowerCase();
	if (lower.startsWith('bot ')) return authHeader.slice(4).trim();
	if (lower.startsWith('bearer ')) return authHeader.slice(7).trim();
	return authHeader.trim();
}

export class GatewayRequestService {
	constructor(private readonly botAuthService: BotAuthService) {}

	async getBotGatewayInfo(authHeader: string | null): Promise<GatewayBotResponseType> {
		const token = extractToken(authHeader);
		if (!token) {
			throw new MissingGatewayAuthorizationError();
		}
		const tokenType = parseTokenType(token);
		if (tokenType !== 'bot') {
			throw new InvalidGatewayAuthTokenError();
		}
		await this.botAuthService.validateBotToken(token);
		return {
			url: Config.endpoints.gateway,
			shards: 1,
			session_start_limit: {
				total: 1000,
				remaining: 999,
				reset_after: 14400000,
				max_concurrency: 1,
			},
		};
	}
}
