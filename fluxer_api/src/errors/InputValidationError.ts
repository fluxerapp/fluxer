/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {APIErrorCodes} from '~/Constants';
import {BadRequestError} from './BadRequestError';
import type {ValidationError} from './ValidationError';

export class InputValidationError extends BadRequestError {
	constructor(errors: Array<ValidationError>) {
		super({code: APIErrorCodes.INVALID_FORM_BODY, message: 'Input Validation Error', data: {errors}});
	}

	static create(path: string, message: string): InputValidationError {
		return new InputValidationError([{path, message}]);
	}

	static createMultiple(errors: Array<{field: string; message: string}>): InputValidationError {
		return new InputValidationError(errors.map((e) => ({path: e.field, message: e.message})));
	}
}
