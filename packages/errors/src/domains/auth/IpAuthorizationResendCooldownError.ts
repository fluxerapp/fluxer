// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {sanitizeRetryAfterSeconds} from '@fluxer/errors/src/domains/core/RetryAfterSeconds';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';

export class IpAuthorizationResendCooldownError extends ThrottledError {
	constructor(resendAvailableIn: number) {
		const retryAfter = sanitizeRetryAfterSeconds(resendAvailableIn);
		super({
			code: APIErrorCodes.IP_AUTHORIZATION_RESEND_COOLDOWN,
			retryAfterSeconds: retryAfter,
			data: {resend_available_in: resendAvailableIn, retry_after: retryAfter},
		});
	}
}
