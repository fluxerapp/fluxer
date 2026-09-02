// SPDX-License-Identifier: AGPL-3.0-or-later

import {useForm} from '@app/features/app/hooks/useForm';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/ForgotPasswordPage.module.css';
import FormField from '@app/features/auth/flow/AuthFormField';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	BACK_TO_SIGN_IN_DESCRIPTOR,
	EMAIL_DESCRIPTOR,
	REGISTER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useId, useState} from 'react';

const FORGOT_PASSWORD_DESCRIPTOR = msg({
	message: 'Forgot password',
	comment: 'Short label in the authentication forgot password page. Keep the tone plain and specific.',
});
const ForgotPasswordPage = observer(function ForgotPasswordPage() {
	const {i18n} = useLingui();
	const emailId = useId();
	const [isSuccess, setIsSuccess] = useState(false);
	const [_error, setError] = useState<string | null>(null);
	useFluxerDocumentTitle(i18n._(FORGOT_PASSWORD_DESCRIPTOR));
	const form = useForm({
		initialValues: {email: ''},
		onSubmit: async (submission) => {
			setError(null);
			try {
				await AuthenticationCommands.forgotPassword(submission.getValue('email'));
				if (!submission.isCurrent()) {
					return;
				}
				setIsSuccess(true);
			} catch {
				if (!submission.isCurrent()) {
					return;
				}
				form.setError('email', 'Failed to send reset link. Try again.');
			}
		},
	});
	useEffect(() => {
		setError(null);
	}, []);
	if (isSuccess) {
		return (
			<div className={styles.container} data-flx="auth.forgot-password-page.container">
				<h1 className={styles.title} data-flx="auth.forgot-password-page.title">
					<Trans>Check your email</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.forgot-password-page.description">
					<Trans>We've sent password reset instructions to your email. Check your inbox for the reset link.</Trans>
				</p>
				<div className={styles.footer} data-flx="auth.forgot-password-page.footer">
					<AuthRouterLink to="/login" className={styles.primaryLink} data-flx="auth.forgot-password-page.primary-link">
						<Trans>Return to sign-in</Trans>
					</AuthRouterLink>
				</div>
			</div>
		);
	}
	return (
		<>
			<h1 className={styles.title} data-flx="auth.forgot-password-page.title--2">
				<Trans>Forgot your password?</Trans>
			</h1>
			<p className={styles.description} data-flx="auth.forgot-password-page.description--2">
				<Trans>Enter your email address and we'll send you a link to reset your password.</Trans>
			</p>
			<form className={styles.form} onSubmit={form.handleSubmit} data-flx="auth.forgot-password-page.form.submit">
				<FormField
					id={emailId}
					name="email"
					type="email"
					autoComplete="email"
					required
					label={i18n._(EMAIL_DESCRIPTOR)}
					value={form.getValue('email')}
					onChange={(value) => form.setValue('email', value)}
					error={form.getError('email')}
					data-flx="auth.forgot-password-page.form-field.set-value.email"
				/>
				<Button
					type="submit"
					fitContainer
					disabled={form.isSubmitting}
					data-flx="auth.forgot-password-page.button.submit"
				>
					<Trans>Send reset link</Trans>
				</Button>
			</form>
			<div className={styles.footer} data-flx="auth.forgot-password-page.footer--2">
				<div data-flx="auth.forgot-password-page.div">
					<AuthRouterLink to="/login" className={styles.link} data-flx="auth.forgot-password-page.link">
						{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
					</AuthRouterLink>
				</div>
				<div data-flx="auth.forgot-password-page.div--2">
					<span className={styles.footerLabel} data-flx="auth.forgot-password-page.footer-label">
						<Trans>Don't have an account?</Trans>{' '}
					</span>
					<AuthRouterLink
						to="/register"
						className={styles.primaryLink}
						data-flx="auth.forgot-password-page.primary-link--2"
					>
						{i18n._(REGISTER_DESCRIPTOR)}
					</AuthRouterLink>
				</div>
			</div>
		</>
	);
});

export default ForgotPasswordPage;
