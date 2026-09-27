// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class CannotVoteOnNonPollError extends BadRequestError {
    constructor() {
        super({
            code: APIErrorCodes.CANNOT_VOTE_ON_NON_POLL_ERROR,
        });
    }
}
