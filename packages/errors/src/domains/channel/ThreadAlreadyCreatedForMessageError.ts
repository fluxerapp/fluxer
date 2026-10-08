// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class ThreadAlreadyCreatedForMessageError extends BadRequestError {
	constructor() {
		super({
			code: APIErrorCodes.THREAD_ALREADY_CREATED_FOR_MESSAGE,
		});
	}
}
