// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class CannotVoteOnFinalizedPollError extends BadRequestError {
    constructor() {
        super({
            code: APIErrorCodes.CANNOT_VOTE_ON_FINALIZED_POLL_ERROR,
        });
    }
}
