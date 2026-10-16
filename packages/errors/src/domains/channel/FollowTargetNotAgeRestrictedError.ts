// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class FollowTargetNotAgeRestrictedError extends BadRequestError {
	constructor() {
		super({
			code: APIErrorCodes.FOLLOW_TARGET_NOT_AGE_RESTRICTED,
		});
	}
}
