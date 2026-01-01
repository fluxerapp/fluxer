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

import type {FormEvent} from 'react';
import {useCallback, useState} from 'react';

interface FormField {
	value: string;
	error?: string;
}

interface FormState {
	[key: string]: FormField;
}

interface UseFormOptions {
	initialValues?: Record<string, string>;
	onSubmit: (values: Record<string, string>) => Promise<void>;
}

export function useForm({initialValues = {}, onSubmit}: UseFormOptions) {
	const [fields, setFields] = useState<FormState>(() => {
		const initial: FormState = {};
		for (const [key, value] of Object.entries(initialValues)) {
			initial[key] = {value};
		}
		return initial;
	});

	const [isSubmitting, setIsSubmitting] = useState(false);

	const setValue = useCallback((fieldName: string, value: string) => {
		setFields((prev) => ({
			...prev,
			[fieldName]: {...prev[fieldName], value, error: undefined},
		}));
	}, []);

	const setError = useCallback((fieldName: string, error: string) => {
		setFields((prev) => ({
			...prev,
			[fieldName]: {...prev[fieldName], error},
		}));
	}, []);

	const setErrors = useCallback((errors: Record<string, string>) => {
		setFields((prev) => {
			const updated = {...prev};
			for (const [fieldName, error] of Object.entries(errors)) {
				updated[fieldName] = {...updated[fieldName], error};
			}
			return updated;
		});
	}, []);

	const getValue = useCallback((fieldName: string): string => fields[fieldName]?.value || '', [fields]);

	const getError = useCallback((fieldName: string): string | undefined => fields[fieldName]?.error, [fields]);

	const getValues = useCallback((): Record<string, string> => {
		const values: Record<string, string> = {};
		for (const [key, field] of Object.entries(fields)) {
			values[key] = field.value;
		}
		return values;
	}, [fields]);

	const handleSubmit = useCallback(
		async (e?: FormEvent) => {
			e?.preventDefault();
			setIsSubmitting(true);
			try {
				await onSubmit(getValues());
			} finally {
				setIsSubmitting(false);
			}
		},
		[onSubmit, getValues],
	);

	return {
		setValue,
		setError,
		setErrors,
		getValue,
		getError,
		handleSubmit,
		isSubmitting,
	};
}
