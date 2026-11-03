// SPDX-License-Identifier: AGPL-3.0-or-later

import FormField from '@app/features/auth/flow/AuthFormField';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {
	type RegistrationLegalConsentConfig,
	RegistrationLegalNotice,
} from '@app/features/auth/flow/RegistrationLegalNotice';
import {type MissingField, SubmitTooltip, shouldDisableSubmit} from '@app/features/auth/flow/SubmitTooltip';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {
	CREATE_ACCOUNT_DESCRIPTOR,
	DISPLAY_NAME_OPTIONAL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {failureCode, failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import {Button} from '@app/features/ui/button/Button';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {type FormEvent, useId, useState} from 'react';

export const DISPLAY_NAME_DESCRIPTOR = msg({
	message: 'Display name',
	comment: 'Label of the name field when creating an account. It is the name other people see.',
});
export const NAME_LENGTH_DESCRIPTOR = msg({
	message: 'Use 1 to 32 characters.',
	comment: 'Calm error under the display name field on account creation when the name is empty or too long.',
});
export const NAME_NOT_ALLOWED_DESCRIPTOR = msg({
	message: 'Try a different name.',
	comment: 'Calm error under the display name field on account creation when the name is reserved or not allowed.',
});

export interface InstantAccountFormErrors {
	readonly name: string | null;
	readonly other: string | null;
}

export const NO_INSTANT_ACCOUNT_ERRORS: InstantAccountFormErrors = {name: null, other: null};

export function instantAccountErrors(error: unknown, i18n: I18n): InstantAccountFormErrors {
	const faults = failureValidationErrors(error) ?? [];
	const nameFault = faults.find((fault) => fault.path === 'global_name');
	if (nameFault !== undefined) {
		const tooLong = nameFault.code === ValidationErrorCodes.GLOBAL_NAME_LENGTH_INVALID;
		return {...NO_INSTANT_ACCOUNT_ERRORS, name: i18n._(tooLong ? NAME_LENGTH_DESCRIPTOR : NAME_NOT_ALLOWED_DESCRIPTOR)};
	}
	if (failureCode(error) === APIErrorCodes.CONTENT_BLOCKED) {
		return {...NO_INSTANT_ACCOUNT_ERRORS, name: i18n._(NAME_NOT_ALLOWED_DESCRIPTOR)};
	}
	return {...NO_INSTANT_ACCOUNT_ERRORS, other: getAuthErrorMessage(error, i18n)};
}

interface InstantAccountFormProps {
	readonly initialName: string;
	readonly consentConfig: RegistrationLegalConsentConfig;
	readonly submitting: boolean;
	readonly errors: InstantAccountFormErrors;
	readonly onEdit: () => void;
	readonly onSubmit: (globalName: string) => void;
	readonly optionalName?: boolean;
	readonly afterHeader?: boolean;
	readonly submitLabel?: string;
	readonly 'data-flx'?: string;
}

export function InstantAccountForm({
	initialName,
	consentConfig,
	submitting,
	errors,
	onEdit,
	onSubmit,
	optionalName = false,
	afterHeader = false,
	submitLabel,
	'data-flx': dataFlx = 'auth.flow.instant-account-form',
}: InstantAccountFormProps) {
	const {i18n} = useLingui();
	const fieldId = useId();
	const [name, setName] = useState(initialName);
	const globalName = name.trim();
	const missingFields: Array<MissingField> = [];
	if (!optionalName && globalName.length === 0)
		missingFields.push({key: 'global_name', label: i18n._(DISPLAY_NAME_DESCRIPTOR)});
	return (
		<form
			className={clsx(styles.form, afterHeader && styles.formAfterHeader)}
			onSubmit={(event: FormEvent<HTMLFormElement>) => {
				event.preventDefault();
				if (submitting || missingFields.length > 0) return;
				onSubmit(globalName);
			}}
			data-flx={dataFlx}
		>
			<FormField
				id={`${fieldId}-global-name`}
				name="global_name"
				type="text"
				autoComplete="nickname"
				autoCapitalize="words"
				spellCheck={false}
				enterKeyHint="go"
				autoFocus
				data-step-focus="true"
				required={!optionalName}
				dir="auto"
				label={i18n._(optionalName ? DISPLAY_NAME_OPTIONAL_DESCRIPTOR : DISPLAY_NAME_DESCRIPTOR)}
				value={name}
				onChange={(value: string) => {
					onEdit();
					setName(value);
				}}
				error={errors.name ?? undefined}
				data-flx={`${dataFlx}.global-name`}
			/>
			<RegistrationLegalNotice
				config={consentConfig}
				data-flx="auth.flow.instant-account-form.registration-legal-notice"
			/>
			{errors.other === null ? null : (
				<p className={clsx(styles.hint, styles.hintError)} role="alert" dir="auto" data-flx={`${dataFlx}.error`}>
					{errors.other}
				</p>
			)}
			<SubmitTooltip missingFields={missingFields} data-flx={`${dataFlx}.submit-tooltip`}>
				<Button
					type="submit"
					fitContainer
					submitting={submitting}
					disabled={shouldDisableSubmit(missingFields)}
					data-flx={`${dataFlx}.button.create`}
				>
					{submitLabel ?? i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
				</Button>
			</SubmitTooltip>
		</form>
	);
}
