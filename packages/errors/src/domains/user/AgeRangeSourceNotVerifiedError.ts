// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export class AgeRangeSourceNotVerifiedError extends ForbiddenError {
	constructor() {
		super({code: APIErrorCodes.AGE_RANGE_SOURCE_NOT_VERIFIED});
	}
}
