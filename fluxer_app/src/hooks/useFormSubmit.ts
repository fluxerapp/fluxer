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

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import type {FieldValues, Path, UseFormReturn} from 'react-hook-form';
import type {HttpResponse} from '~/lib/HttpClient';
import {isAbortError} from '~/stores/SudoPromptStore';
import * as FormUtils from '~/utils/FormUtils';

interface UseFormSubmitOptions<T extends FieldValues> {
	form: UseFormReturn<T>;
	onSubmit: (data: T) => Promise<void> | void;
	defaultErrorField: Path<T>;
}

export function useFormSubmit<T extends FieldValues>({form, onSubmit, defaultErrorField}: UseFormSubmitOptions<T>) {
	const {i18n} = useLingui();

	const handleSubmit = React.useCallback(
		async (data: T) => {
			try {
				await onSubmit(data);
			} catch (error) {
				if (isAbortError(error)) {
					return;
				}
				FormUtils.handleError(i18n, form, error as HttpResponse, defaultErrorField);
				return;
			}
		},
		[form, onSubmit, defaultErrorField, i18n],
	);

	const submitWithErrorClearing = React.useCallback(async () => {
		const errors = form.formState.errors;
		const errorFields = Object.keys(errors) as Array<Path<T>>;

		errorFields.forEach((field) => {
			const error = errors[field];
			if (error && 'type' in error && error.type === 'server') {
				form.clearErrors(field);
			}
		});

		await form.handleSubmit(handleSubmit)();
	}, [form, handleSubmit]);

	return {
		handleSubmit: submitWithErrorClearing,
		isSubmitting: form.formState.isSubmitting,
	};
}
