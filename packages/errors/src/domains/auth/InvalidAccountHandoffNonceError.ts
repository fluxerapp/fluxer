// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class InvalidAccountHandoffNonceError extends BadRequestError {
	constructor() {
		super({code: APIErrorCodes.INVALID_ACCOUNT_HANDOFF_NONCE});
	}
}
