// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class MaxPollVotesPerAnswerError extends BadRequestError {
	constructor(limit: number) {
		super({
			code: APIErrorCodes.MAX_POLL_VOTES,
			messageVariables: {count: limit},
		});
	}
}
