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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {FieldValues, Path, UseFormReturn} from 'react-hook-form';
import {APIErrorCodes} from '~/Constants';
import type {HttpError, HttpResponse} from '~/lib/HttpClient';

interface ValidationError {
	path: string;
	message: string;
}

interface APIErrorResponse {
	code: string;
	message: string;
	errors?: Array<ValidationError>;
}

export const handleError = <T extends FieldValues>(
	i18n: I18n,
	form: UseFormReturn<T>,
	error: HttpResponse<unknown> | HttpError,
	defaultPath: Path<T>,
) => {
	if ('body' in error && error.body) {
		const errorData = error.body as APIErrorResponse;

		if (errorData.code === APIErrorCodes.INVALID_FORM_BODY && errorData.errors?.length) {
			const formFields = Object.keys(form.getValues()) as Array<Path<T>>;

			for (const validationError of errorData.errors) {
				const path = validationError.path as Path<T>;
				const message = validationError.message;

				if (formFields.includes(path)) {
					form.setError(path, {type: 'server', message});
				} else {
					form.setError(defaultPath, {type: 'server', message});
				}
			}
		} else if (errorData.message) {
			form.setError(defaultPath, {type: 'server', message: errorData.message});
		}
		return;
	}

	form.setError(defaultPath, {
		type: 'server',
		message: i18n._(msg`An unexpected error occurred. Please try again.`),
	});
};
