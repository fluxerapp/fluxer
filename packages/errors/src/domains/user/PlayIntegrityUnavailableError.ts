// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';

export class PlayIntegrityUnavailableError extends ServiceUnavailableError {
	constructor() {
		super({code: APIErrorCodes.PLAY_INTEGRITY_UNAVAILABLE});
	}
}
