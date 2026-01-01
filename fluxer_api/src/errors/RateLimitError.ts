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

import {APIErrorCodes} from '~/Constants';
import {FluxerAPIError} from './FluxerAPIError';

export class RateLimitError extends FluxerAPIError {
	constructor({
		message = 'You are being rate limited.',
		global = false,
		retryAfter,
		retryAfterDecimal,
		limit,
		resetTime,
	}: {
		message?: string;
		global?: boolean;
		retryAfter: number;
		retryAfterDecimal?: number;
		limit: number;
		resetTime: Date;
	}) {
		super({
			code: APIErrorCodes.RATE_LIMITED,
			message,
			status: 429,
			data: {
				global,
				retry_after: retryAfterDecimal ?? retryAfter,
			},
			headers: {
				'X-RateLimit-Global': global ? 'true' : 'false',
				'X-RateLimit-Limit': limit.toString(),
				'X-RateLimit-Remaining': '0',
				'X-RateLimit-Reset': Math.floor(resetTime.getTime() / 1000).toString(),
				'Retry-After': retryAfter.toString(),
			},
		});
	}
}
