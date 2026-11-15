// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class OperatorPassLastPassConfirmationRequiredError extends BadRequestError {
	constructor() {
		super({code: APIErrorCodes.OPERATOR_PASS_LAST_PASS_CONFIRMATION_REQUIRED});
	}
}
