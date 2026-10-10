// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {FluxerError, type FluxerErrorData} from '@fluxer/errors/src/FluxerError';

interface HttpErrorOptions {
	code?: string;
	message?: string;
	data?: FluxerErrorData;
	headers?: Record<string, string>;
	cause?: Error;
}

export class ServiceUnavailableError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.SERVICE_UNAVAILABLE,
			message: options.message ?? 'Service Unavailable',
			status: HttpStatus.SERVICE_UNAVAILABLE,
		});
		this.name = 'ServiceUnavailableError';
	}
}
