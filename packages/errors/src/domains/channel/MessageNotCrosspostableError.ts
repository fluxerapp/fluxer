// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class MessageNotCrosspostableError extends BadRequestError {
	constructor() {
		super({
			code: APIErrorCodes.MESSAGE_NOT_CROSSPOSTABLE,
		});
	}
}
