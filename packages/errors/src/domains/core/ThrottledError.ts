// SPDX-License-Identifier: AGPL-3.0-or-later

import {sanitizeRetryAfterSeconds} from '@fluxer/errors/src/domains/core/RetryAfterSeconds';
import {FluxerError, type FluxerErrorData} from '@fluxer/errors/src/FluxerError';

export class ThrottledError extends FluxerError {
	constructor({
		code,
		message,
		retryAfterSeconds,
		data,
		headers,
		messageVariables,
	}: {
		code: string;
		message?: string;
		retryAfterSeconds: number;
		data?: FluxerErrorData;
		headers?: Record<string, string>;
		messageVariables?: Record<string, unknown>;
	}) {
		super({
			code,
			message,
			status: 429,
			data,
			headers: {...headers, 'Retry-After': sanitizeRetryAfterSeconds(retryAfterSeconds).toString()},
			messageVariables,
		});
	}
}
