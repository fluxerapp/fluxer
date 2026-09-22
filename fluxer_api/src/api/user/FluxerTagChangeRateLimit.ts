// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {getRetryAfterSeconds} from '@app/api/utils/RateLimitUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import {ms} from 'itty-time';

const FLUXER_TAG_CHANGE_MAX_ATTEMPTS = 5;
const FLUXER_TAG_CHANGE_WINDOW_MS = ms('3 hours');

export async function enforceFluxerTagChangeRateLimit(params: {
	rateLimitService: IRateLimitService;
	userId: UserID;
	errorPath: 'username' | 'discriminator';
}): Promise<void> {
	const rateLimit = await params.rateLimitService.checkLimit({
		identifier: `username_change:${params.userId}`,
		maxAttempts: FLUXER_TAG_CHANGE_MAX_ATTEMPTS,
		windowMs: FLUXER_TAG_CHANGE_WINDOW_MS,
	});
	if (!rateLimit.allowed) {
		throw InputValidationError.fromCode(params.errorPath, ValidationErrorCodes.USERNAME_CHANGED_TOO_MANY_TIMES, {
			minutes: Math.max(1, Math.ceil(getRetryAfterSeconds(rateLimit) / 60)),
		});
	}
}
