// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {isAbortError} from '@app/features/platform/utils/PlatformHelpers';
import * as FormUtils from '@app/lib/forms';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';
import type {FieldValues, Path, UseFormReturn} from 'react-hook-form';

interface UseFormSubmitOptions<T extends FieldValues> {
	form: UseFormReturn<T>;
	onSubmit: (data: T) => Promise<void> | void;
	defaultErrorField: Path<T>;
	pathMap?: Partial<Record<string, Path<T>>>;
}

export function useFormSubmit<T extends FieldValues>({
	form,
	onSubmit,
	defaultErrorField,
	pathMap,
}: UseFormSubmitOptions<T>) {
	const {i18n} = useLingui();
	const handleSubmit = useCallback(
		async (data: T) => {
			try {
				await onSubmit(data);
			} catch (error) {
				if (isAbortError(error)) {
					return;
				}
				FormUtils.handleError(i18n, form, error as RestResponse, defaultErrorField, {pathMap});
				throw error;
			}
		},
		[form, onSubmit, defaultErrorField, pathMap, i18n],
	);
	const submitWithErrorClearing = useCallback(async () => {
		const errors = form.formState.errors;
		const errorFields = Object.keys(errors) as Array<Path<T>>;
		errorFields.forEach((field) => {
			const error = errors[field];
			if (error && 'type' in error && error.type === 'server') {
				form.clearErrors(field);
			}
		});
		await form
			.handleSubmit(handleSubmit)()
			.catch(() => undefined);
	}, [form, handleSubmit]);
	return {
		handleSubmit: submitWithErrorClearing,
		isSubmitting: form.formState.isSubmitting,
	};
}
