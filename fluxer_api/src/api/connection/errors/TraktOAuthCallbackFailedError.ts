// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class TraktOAuthCallbackFailedError extends BadRequestError {
	constructor() {
		super({code: APIErrorCodes.TRAKT_OAUTH_CALLBACK_FAILED});
	}
}
