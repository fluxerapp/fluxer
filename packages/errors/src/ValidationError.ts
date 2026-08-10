// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {FluxerError} from '@fluxer/errors/src/FluxerError';

interface PathError {
	path: string;
	code: string;
	message: string;
}

interface ValidationErrorOptions {
	code?: string;
	message?: string;
	errors: Array<PathError>;
}

export class ValidationError extends FluxerError {
	readonly errors: Array<PathError>;

	constructor(options: ValidationErrorOptions) {
		super({
			code: options.code ?? APIErrorCodes.VALIDATION_ERROR,
			message: options.message ?? 'Validation failed',
			status: HttpStatus.BAD_REQUEST,
			data: {errors: options.errors},
		});
		this.name = 'ValidationError';
		this.errors = options.errors;
	}

	override getResponse(): Response {
		return new Response(
			JSON.stringify({
				code: this.code,
				message: this.message,
				errors: this.errors,
			}),
			{
				status: this.status,
				headers: {
					'Content-Type': 'application/json',
				},
			},
		);
	}

	static fromPath(path: string, code: string, message: string): ValidationError {
		return new ValidationError({
			errors: [{path, code, message}],
		});
	}

	static fromPaths(errors: Array<PathError>): ValidationError {
		return new ValidationError({errors});
	}
}
