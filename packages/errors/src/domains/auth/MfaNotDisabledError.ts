// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class MfaNotDisabledError extends BadRequestError {
	constructor() {
		super({code: APIErrorCodes.TWO_FA_ALREADY_ENABLED});
	}
}
