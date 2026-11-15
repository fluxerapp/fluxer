// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';

export class VoiceJoinTokenBusyError extends ConflictError {
	constructor() {
		super({code: APIErrorCodes.VOICE_JOIN_TOKEN_BUSY});
	}
}
