// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {statusError} from '@fluxer/errors/src/FluxerError';

export class RequestBodyTooLargeError extends statusError(413) {
	constructor(maxSize: number) {
		super({code: APIErrorCodes.FILE_SIZE_TOO_LARGE, messageVariables: {maxSize}});
	}
}
