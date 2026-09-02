// SPDX-License-Identifier: AGPL-3.0-or-later

import {type FormSubmission, type UseFormReturn, useForm} from '@app/features/app/hooks/useForm';
import {CaptchaCancelledError, CaptchaValidationError} from '@app/features/auth/hooks/useCaptcha';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {failureMessage, failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useState} from 'react';

const AN_UNEXPECTED_ERROR_OCCURRED_DESCRIPTOR = msg({
	message: 'An unexpected error occurred',
	comment: 'Short label in the authentication auth form. Keep the tone plain and specific.',
});

type AuthFormSubmitResult = false | undefined;

interface UseAuthFormOptions {
	initialValues: Record<string, string>;
	onSubmit: (values: Record<string, string>) => Promise<AuthFormSubmitResult>;
	redirectPath?: string;
	firstFieldName?: string;
}

interface ApplyAuthFormErrorsRequest {
	error: unknown;
	form: UseFormReturn;
	i18n: I18n;
	firstFieldName: string | undefined;
	setError: (error: string | null) => void;
	setFieldErrors: (errors: ReadonlyMap<string, string> | null) => void;
}

const collectSubmittedValues = (
	submission: FormSubmission,
	initialValues: Record<string, string>,
): Record<string, string> => {
	const values: Record<string, string> = {};
	for (const fieldName of Object.keys(initialValues)) {
		values[fieldName] = submission.getValue(fieldName);
	}
	return values;
};
const collectFieldErrors = (
	violations: ReadonlyArray<{path: string; message: string}>,
): ReadonlyMap<string, string> => {
	const fieldErrors = new Map<string, string>();
	for (const {path, message} of violations) {
		const existingMessage = fieldErrors.get(path);
		fieldErrors.set(path, existingMessage ? `${existingMessage} ${message}` : message);
	}
	return fieldErrors;
};
const applyAuthFormErrors = ({
	error,
	form,
	i18n,
	firstFieldName,
	setError,
	setFieldErrors,
}: ApplyAuthFormErrorsRequest): void => {
	const fieldViolations = failureValidationErrors(error) ?? [];
	if (fieldViolations.length > 0) {
		const fieldErrors = collectFieldErrors(fieldViolations);
		setFieldErrors(fieldErrors);
		form.setErrors(fieldErrors);
		return;
	}
	const message = getAuthErrorMessage(error, i18n);
	if (!firstFieldName) {
		setError(message);
		return;
	}
	const fieldErrors = new Map([[firstFieldName, message]]);
	setFieldErrors(fieldErrors);
	form.setErrors(fieldErrors);
};

export function useAuthForm({initialValues, onSubmit, redirectPath, firstFieldName}: UseAuthFormOptions) {
	const {i18n} = useLingui();
	const [error, setError] = useState<string | null>(null);
	const [fieldErrors, setFieldErrors] = useState<ReadonlyMap<string, string> | null>(null);
	const form = useForm({
		initialValues,
		onSubmit: async (submission) => {
			setError(null);
			setFieldErrors(null);
			try {
				const shouldRedirect = await onSubmit(collectSubmittedValues(submission, initialValues));
				if (!submission.isCurrent()) {
					return;
				}
				if (shouldRedirect !== false && redirectPath) {
					RouterUtils.replaceWith(redirectPath);
				}
			} catch (err) {
				if (err instanceof CaptchaCancelledError) {
					return;
				}
				if (err instanceof CaptchaValidationError) {
					return;
				}
				if (!submission.isCurrent()) {
					return;
				}
				applyAuthFormErrors({error: err, form, i18n, firstFieldName, setError, setFieldErrors});
			}
		},
	});
	return {
		form,
		isLoading: form.isSubmitting,
		error,
		fieldErrors,
	};
}

export const getAuthErrorMessage = (error: unknown, i18n: I18n): string => {
	const message = failureMessage(error);
	if (message) {
		return message;
	}
	if (error instanceof HttpError) {
		return i18n._(AN_UNEXPECTED_ERROR_OCCURRED_DESCRIPTOR);
	}
	if (error instanceof Error) {
		return error.message;
	}
	return i18n._(AN_UNEXPECTED_ERROR_OCCURRED_DESCRIPTOR);
};
