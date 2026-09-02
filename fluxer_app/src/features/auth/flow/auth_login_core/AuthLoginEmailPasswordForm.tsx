// SPDX-License-Identifier: AGPL-3.0-or-later

import FormField from '@app/features/auth/flow/AuthFormField';
import {EMAIL_DESCRIPTOR, PASSWORD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useId} from 'react';

type FieldErrors = ReadonlyMap<string, string> | null | undefined;

export interface AuthFormControllerLike {
	handleSubmit: (event: React.FormEvent<HTMLFormElement>) => void;
	getValue: (name: string) => string;
	setValue: (name: string, value: string) => void;
	getError: (name: string) => string | null | undefined;
	isSubmitting?: boolean;
}

export interface AuthEmailPasswordFormClasses {
	form: string;
}

interface Props {
	form: AuthFormControllerLike;
	isLoading: boolean;
	fieldErrors?: FieldErrors;
	submitLabel: React.ReactNode;
	classes: AuthEmailPasswordFormClasses;
	extraFields?: React.ReactNode;
	links?: React.ReactNode;
	linksWrapperClassName?: string;
	disableSubmit?: boolean;
}

export default function AuthLoginEmailPasswordForm({
	form,
	isLoading,
	fieldErrors,
	submitLabel,
	classes,
	extraFields,
	links,
	linksWrapperClassName,
	disableSubmit,
}: Props) {
	const {i18n} = useLingui();
	const emailId = useId();
	const passwordId = useId();
	const isSubmitting = Boolean(form.isSubmitting);
	const submitDisabled = isLoading || isSubmitting || Boolean(disableSubmit);
	return (
		<form
			className={classes.form}
			onSubmit={form.handleSubmit}
			autoComplete="on"
			name="login"
			data-flx="auth.flow.auth-login-core.auth-login-email-password-form.form.submit"
		>
			<FormField
				id={emailId}
				name="email"
				type="email"
				autoComplete="username"
				autoCapitalize="none"
				autoCorrect="off"
				enterKeyHint="next"
				spellCheck={false}
				data-step-focus="true"
				required
				label={i18n._(EMAIL_DESCRIPTOR)}
				value={form.getValue('email')}
				onChange={(value) => form.setValue('email', value)}
				error={form.getError('email') || fieldErrors?.get('email')}
				data-flx="auth.flow.auth-login-core.auth-login-email-password-form.form-field.set-value.email"
			/>
			<FormField
				id={passwordId}
				name="password"
				type="password"
				autoComplete="current-password"
				enterKeyHint="done"
				required
				label={i18n._(PASSWORD_DESCRIPTOR)}
				value={form.getValue('password')}
				onChange={(value) => form.setValue('password', value)}
				error={form.getError('password') || fieldErrors?.get('password')}
				data-flx="auth.flow.auth-login-core.auth-login-email-password-form.form-field.set-value.password"
			/>
			{extraFields}
			{links ? (
				<div className={linksWrapperClassName} data-flx="auth.flow.auth-login-core.auth-login-email-password-form.div">
					{links}
				</div>
			) : null}
			<Button
				type="submit"
				fitContainer
				disabled={submitDisabled}
				data-flx="auth.flow.auth-login-core.auth-login-email-password-form.button.submit"
			>
				{submitLabel}
			</Button>
		</form>
	);
}
