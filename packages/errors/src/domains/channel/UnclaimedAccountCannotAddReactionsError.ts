// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export class UnclaimedAccountCannotAddReactionsError extends ForbiddenError {
	constructor() {
		super({code: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS});
	}
}
