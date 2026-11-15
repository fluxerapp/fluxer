// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {sanitizeRetryAfterSeconds} from '@fluxer/errors/src/domains/core/RetryAfterSeconds';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';

export class HandoffAttemptsExceededError extends ThrottledError {
	constructor(retryAfterSeconds: number) {
		const retryAfter = sanitizeRetryAfterSeconds(retryAfterSeconds);
		super({
			code: APIErrorCodes.HANDOFF_ATTEMPTS_EXCEEDED,
			retryAfterSeconds: retryAfter,
			data: {retry_after: retryAfter},
			messageVariables: {minutes: Math.max(1, Math.ceil(retryAfter / 60))},
		});
	}
}
