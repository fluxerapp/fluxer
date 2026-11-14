// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class OperatorPassWallNameRequiredError extends BadRequestError {
	constructor() {
		super({code: APIErrorCodes.OPERATOR_PASS_WALL_NAME_REQUIRED});
	}
}
