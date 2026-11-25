// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {statusError} from '@fluxer/errors/src/FluxerError';

export class ResourceLockedError extends statusError(423, false) {
	constructor(detail?: string) {
		super({
			code: APIErrorCodes.GENERAL_ERROR,
			headers: {'Retry-After': '2'},
			messageVariables: detail ? {detail} : undefined,
		});
	}
}
