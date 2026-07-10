// SPDX-License-Identifier: AGPL-3.0-or-later

import {useHashParam} from '@app/features/app/hooks/useHashParam';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/ResetPasswordPage.module.css';
import FormField from '@app/features/auth/flow/AuthFormField';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {resetPassword as resetPasswordFlow} from '@app/features/auth/state/AuthFlow';
import {BACK_TO_SIGN_IN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Button} from '@app/features/ui/button/Button';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useId, useState} from 'react';

const RESET_PASSWORD_DESCRIPTOR = msg({
	message: 'Reset password',
	comment: 'Short label in the authentication reset password page. Keep the tone plain and specific.',
});
const NEW_PASSWORD_DESCRIPTOR = msg({
	message: 'New password',
	comment: 'Short label in the authentication reset password page. Keep the tone plain and specific.',
});
const CONFIRM_NEW_PASSWORD_DESCRIPTOR = msg({
	message: 'Confirm new password',
	comment: 'Short label in the authentication reset password page. Keep the tone plain and specific.',
});

type TokenStatus = 'validating' | 'valid' | 'invalid';

const API_RENDERED_FIELDS = new Set(['password']);
const ResetPasswordPage = observer(function ResetPasswordPage() {
	const {i18n} = useLingui();
	const passwordId = useId();
	const confirmPasswordId = useId();
	useFluxerDocumentTitle(i18n._(RESET_PASSWORD_DESCRIPTOR));
	const token = useHashParam('token');
	const [tokenStatus, setTokenStatus] = useState<TokenStatus>('validating');
	const {form, isLoading, error, fieldErrors} = useAuthForm({
		initialValues: {
			password: '',
			confirmPassword: '',
		},
		onSubmit: async (values) => {
			if (!token) {
				form.setError('password', 'Invalid or missing reset token');
				return;
			}
			if (values.password !== values.confirmPassword) {
				form.setError('confirmPassword', 'Passwords do not match');
				return;
			}
			const response = await resetPasswordFlow(token, values.password);
			if (response.type === 'mfa') {
				AuthenticationCommands.setMfaTicket({
					ticket: response.challenge.ticket,
					totp: response.challenge.totp,
					webauthn: response.challenge.webauthn,
				});
				RouterUtils.replaceWith('/login');
				return;
			}
			await AuthenticationCommands.completeLogin(response.payload);
		},
	});
	useEffect(() => {
		if (!token) {
			RouterUtils.replaceWith('/forgot');
			return;
		}
		let cancelled = false;
		setTokenStatus('validating');
		AuthenticationCommands.validateResetPasswordToken(token)
			.then((valid) => {
				if (cancelled) return;
				setTokenStatus(valid ? 'valid' : 'invalid');
			})
			.catch(() => {
				if (cancelled) return;
				setTokenStatus('invalid');
			});
		return () => {
			cancelled = true;
		};
	}, [token]);
	const unrenderedFieldErrors = fieldErrors
		? Object.entries(fieldErrors)
				.filter(([field]) => !API_RENDERED_FIELDS.has(field))
				.map(([, message]) => message)
		: [];
	const bannerError = error ?? unrenderedFieldErrors[0] ?? null;
	if (tokenStatus === 'validating') {
		return (
			<>
				<h1 className={styles.title} data-flx="auth.reset-password-page.title">
					<Trans>Set new password</Trans>
				</h1>
				<p className={styles.statusMessage} data-flx="auth.reset-password-page.status-message">
					<Trans>Verifying your reset link…</Trans>
				</p>
			</>
		);
	}
	if (tokenStatus === 'invalid') {
		return (
			<>
				<h1 className={styles.title} data-flx="auth.reset-password-page.title--2">
					<Trans>Reset link invalid or expired</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.reset-password-page.description">
					<Trans>This reset link has expired. Reset links last 1 hour. Please request a new one.</Trans>
				</p>
				<div className={styles.footer} data-flx="auth.reset-password-page.footer">
					<AuthRouterLink to="/forgot" className={styles.link} data-flx="auth.reset-password-page.link">
						<Trans>Request a new reset link</Trans>
					</AuthRouterLink>
				</div>
			</>
		);
	}
	return (
		<>
			<h1 className={styles.title} data-flx="auth.reset-password-page.title--3">
				<Trans>Set new password</Trans>
			</h1>
			<p className={styles.description} data-flx="auth.reset-password-page.description--2">
				<Trans>Set your new password.</Trans>
			</p>
			{bannerError ? (
				<div className={styles.formError} role="alert" data-flx="auth.reset-password-page.form-error">
					{bannerError}
				</div>
			) : null}
			<form className={styles.form} onSubmit={form.handleSubmit} data-flx="auth.reset-password-page.form.submit">
				<FormField
					id={passwordId}
					name="password"
					type="password"
					autoComplete="new-password"
					required
					label={i18n._(NEW_PASSWORD_DESCRIPTOR)}
					value={form.getValue('password')}
					onChange={(value) => form.setValue('password', value)}
					error={form.getError('password') || fieldErrors?.password}
					data-flx="auth.reset-password-page.form-field.set-value.password"
				/>
				<FormField
					id={confirmPasswordId}
					name="confirmPassword"
					type="password"
					autoComplete="new-password"
					required
					label={i18n._(CONFIRM_NEW_PASSWORD_DESCRIPTOR)}
					value={form.getValue('confirmPassword')}
					onChange={(value) => form.setValue('confirmPassword', value)}
					error={form.getError('confirmPassword')}
					data-flx="auth.reset-password-page.form-field.set-value.password--2"
				/>
				<Button
					type="submit"
					fitContainer
					disabled={isLoading || form.isSubmitting}
					data-flx="auth.reset-password-page.button.submit"
				>
					<Trans>Reset password</Trans>
				</Button>
			</form>
			<div className={styles.footer} data-flx="auth.reset-password-page.footer--2">
				<AuthRouterLink to="/login" className={styles.link} data-flx="auth.reset-password-page.link--2">
					{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
				</AuthRouterLink>
			</div>
		</>
	);
});

export default ResetPasswordPage;
