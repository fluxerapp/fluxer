// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

/**
 * Raised when a user attempts to select more than one answer on a poll
 * that has allow_multiselect=false (a simple, single-choice poll).
 */
export class PollMultiselectNotAllowedError extends BadRequestError {
	constructor() {
		super({code: APIErrorCodes.POLL_MULTISELECT_NOT_ALLOWED});
	}
}
