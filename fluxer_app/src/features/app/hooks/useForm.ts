// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FormEvent} from 'react';
import {useCallback, useLayoutEffect, useState} from 'react';

interface FormField {
	value: string;
	error?: string;
}

type FormState = ReadonlyMap<string, FormField>;

class FormSubmissionOwner {}

interface FormLifecycle {
	activeSubmission: FormSubmissionOwner | null;
	mounted: boolean;
}

export interface FormSubmission {
	getValue: (fieldName: string) => string;
	isCurrent: () => boolean;
}

interface UseFormOptions {
	initialValues: Record<string, string>;
	onSubmit: (submission: FormSubmission) => Promise<void>;
}

export interface UseFormReturn {
	setValue: (fieldName: string, value: string) => void;
	setError: (fieldName: string, error: string) => void;
	setErrors: (errors: ReadonlyMap<string, string>) => void;
	getValue: (fieldName: string) => string;
	getError: (fieldName: string) => string | undefined;
	handleSubmit: (event?: FormEvent) => Promise<void>;
	isSubmitting: boolean;
}

function createFormState(initialValues: Record<string, string>): FormState {
	const fields = new Map<string, FormField>();
	for (const [fieldName, value] of Object.entries(initialValues)) {
		fields.set(fieldName, {value});
	}
	return fields;
}

function withFieldError(fields: FormState, fieldName: string, error: string): FormField {
	const field = fields.get(fieldName);
	return {value: field?.value ?? '', error};
}

function resolveFormValues(fields: FormState): ReadonlyMap<string, string> {
	const values = new Map<string, string>();
	for (const [fieldName, field] of fields) {
		values.set(fieldName, field.value);
	}
	return values;
}

function isFormSubmissionCurrent(lifecycle: FormLifecycle, submission: FormSubmissionOwner): boolean {
	return lifecycle.mounted && lifecycle.activeSubmission === submission;
}

export function useForm({initialValues, onSubmit}: UseFormOptions): UseFormReturn {
	const [fields, setFields] = useState<FormState>(() => createFormState(initialValues));
	const [isSubmitting, setIsSubmitting] = useState(false);
	const [lifecycle] = useState<FormLifecycle>(() => ({activeSubmission: null, mounted: false}));
	useLayoutEffect(() => {
		lifecycle.mounted = true;
		return () => {
			lifecycle.mounted = false;
			lifecycle.activeSubmission = null;
		};
	}, [lifecycle]);
	const setValue = useCallback((fieldName: string, value: string) => {
		setFields((currentFields) => new Map(currentFields).set(fieldName, {value}));
	}, []);
	const setError = useCallback((fieldName: string, error: string) => {
		setFields((currentFields) =>
			new Map(currentFields).set(fieldName, withFieldError(currentFields, fieldName, error)),
		);
	}, []);
	const setErrors = useCallback((errors: ReadonlyMap<string, string>) => {
		setFields((currentFields) => {
			const updatedFields = new Map(currentFields);
			for (const [fieldName, error] of errors) {
				updatedFields.set(fieldName, withFieldError(currentFields, fieldName, error));
			}
			return updatedFields;
		});
	}, []);
	const getValue = useCallback((fieldName: string): string => fields.get(fieldName)?.value ?? '', [fields]);
	const getError = useCallback((fieldName: string): string | undefined => fields.get(fieldName)?.error, [fields]);
	const handleSubmit = useCallback(
		async (event?: FormEvent) => {
			event?.preventDefault();
			if (lifecycle.activeSubmission != null) {
				return;
			}
			const submissionOwner = new FormSubmissionOwner();
			lifecycle.activeSubmission = submissionOwner;
			setIsSubmitting(true);
			const submittedValues = resolveFormValues(fields);
			const submission: FormSubmission = {
				getValue: (fieldName) => submittedValues.get(fieldName) ?? '',
				isCurrent: () => isFormSubmissionCurrent(lifecycle, submissionOwner),
			};
			try {
				await onSubmit(submission);
			} finally {
				if (isFormSubmissionCurrent(lifecycle, submissionOwner)) {
					lifecycle.activeSubmission = null;
					setIsSubmitting(false);
				}
			}
		},
		[fields, lifecycle, onSubmit],
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
