// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class CannotSendPollInPersonalNotesError extends BadRequestError {
    constructor() {
        super({code: APIErrorCodes.CANNOT_SEND_POLL_IN_PERSONAL_NOTES});
    }
}
