// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {ValidationErrorCode} from '@fluxer/constants/src/ValidationErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import type {ValidationError} from '@fluxer/errors/src/domains/core/ValidationError';

export interface LocalizedValidationError {
	path: string;
	code: ValidationErrorCode;
	variables?: Record<string, unknown>;
}

export class InputValidationError extends BadRequestError {
	readonly localizedErrors: Array<LocalizedValidationError> | null;

	constructor(errors: Array<ValidationError>, localizedErrors?: Array<LocalizedValidationError>) {
		super({code: APIErrorCodes.INVALID_FORM_BODY, data: {errors}});
		this.localizedErrors = localizedErrors ?? null;
	}

	public getLocalizedErrors(): Array<LocalizedValidationError> | null {
		return this.localizedErrors;
	}

	static create(path: string, message: string): InputValidationError {
		return new InputValidationError([{path, message}]);
	}

	static createMultiple(errors: Array<ValidationError>): InputValidationError {
		return new InputValidationError(errors);
	}

	static fromCode(path: string, code: ValidationErrorCode, variables?: Record<string, unknown>): InputValidationError {
		return new InputValidationError([{path, message: code, code}], [{path, code, variables}]);
	}

	static fromCodes(errors: Array<LocalizedValidationError>): InputValidationError {
		const validationErrors = errors.map((e) => ({
			path: e.path,
			message: e.code,
			code: e.code,
		}));
		return new InputValidationError(validationErrors, errors);
	}
}
