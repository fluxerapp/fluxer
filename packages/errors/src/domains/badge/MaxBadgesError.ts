// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class MaxBadgesError extends BadRequestError {
	constructor({maxBadges}: {maxBadges: number}) {
		super({code: APIErrorCodes.MAX_BADGES, data: {max_badges: maxBadges}});
	}
}
