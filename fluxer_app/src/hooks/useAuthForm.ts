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
import {useEffect, useState} from 'react';
import {APIErrorCodes} from '~/Constants';
import {HttpError, type HttpResponse} from '~/lib/HttpClient';
import * as RouterUtils from '~/utils/RouterUtils';
import {CaptchaCancelledError, CaptchaValidationError} from './useCaptcha';
import {useForm} from './useForm';

interface UseAuthFormOptions {
	initialValues: Record<string, string>;
	onSubmit: (values: Record<string, string>) => Promise<void>;
	redirectPath?: string;
	firstFieldName?: string;
}

interface ValidationError {
	path: string;
	message: string;
}

interface APIErrorResponse {
	code: string;
	message: string;
	errors?: Array<ValidationError>;
}

type LinguiT = (literals: TemplateStringsArray, ...placeholders: Array<unknown>) => string;

const isHttpResponse = (value: unknown): value is HttpResponse<unknown> =>
	typeof value === 'object' && value !== null && 'ok' in value && 'status' in value && 'body' in value;

const getErrorData = (error: unknown): APIErrorResponse | undefined => {
	if (error instanceof HttpError) {
		return error.body as APIErrorResponse | undefined;
	}
	if (isHttpResponse(error)) {
		return error.body as APIErrorResponse | undefined;
	}
	if (typeof error === 'object' && error !== null && 'body' in error) {
		return (error as {body?: APIErrorResponse}).body;
	}
	return undefined;
};

export function useAuthForm({initialValues, onSubmit, redirectPath, firstFieldName}: UseAuthFormOptions) {
	const {t} = useLingui();

	const [isLoading, setIsLoading] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const [fieldErrors, setFieldErrors] = useState<Record<string, string> | null>(null);

	const form = useForm({
		initialValues,
		onSubmit: async (values) => {
			setIsLoading(true);
			setError(null);
			setFieldErrors(null);

			try {
				await onSubmit(values);
				if (redirectPath) {
					RouterUtils.replaceWith(redirectPath);
				}
			} catch (err) {
				if (err instanceof CaptchaCancelledError) {
					return;
				}
				if (err instanceof CaptchaValidationError) {
					return;
				}
				extractErrors(err, setError, setFieldErrors, form, t, firstFieldName);
			} finally {
				setIsLoading(false);
			}
		},
	});

	useEffect(() => {
		setError(null);
		setFieldErrors(null);
	}, []);

	return {
		form,
		isLoading,
		error,
		fieldErrors,
	};
}

export const getAuthErrorMessage = (error: unknown, t?: LinguiT): string => {
	const errorData = getErrorData(error);
	const unexpected = t ? t`An unexpected error occurred` : 'An unexpected error occurred';
	const fallbackMessage = error instanceof Error ? error.message : unexpected;
	return errorData?.message || fallbackMessage;
};

const extractErrors = (
	error: unknown,
	setError: (error: string | null) => void,
	setFieldErrors: (errors: Record<string, string> | null) => void,
	form: ReturnType<typeof useForm>,
	t: LinguiT,
	firstFieldName?: string,
) => {
	const errorData = getErrorData(error);

	if (errorData?.code === APIErrorCodes.INVALID_FORM_BODY && errorData.errors?.length) {
		const fieldErrors = errorData.errors.reduce(
			(acc, {path, message}) => {
				acc[path] = message;
				return acc;
			},
			{} as Record<string, string>,
		);

		setFieldErrors(fieldErrors);
		form.setErrors(fieldErrors);
		return;
	}

	const message = getAuthErrorMessage(error, t);

	if (firstFieldName) {
		const fieldErrors = {[firstFieldName]: message};
		setFieldErrors(fieldErrors);
		form.setErrors(fieldErrors);
	} else {
		setError(message);
	}
};
