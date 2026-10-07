// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import styles from '@app/features/auth/components/pages/ForgotPasswordPage.module.css';
import {resolveForgotPasswordRedirect} from '@app/features/auth/flow/AccountRecoveryRedirects';
import FormField from '@app/features/auth/flow/AuthFormField';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {useForgotPasswordForm} from '@app/features/auth/flow/useForgotPasswordForm';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {authRequestTargetFromSnapshot} from '@app/features/auth/state/AuthRequestTarget';
import {
	BACK_TO_SIGN_IN_DESCRIPTOR,
	EMAIL_DESCRIPTOR,
	REGISTER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Button} from '@app/features/ui/button/Button';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

const FORGOT_PASSWORD_DESCRIPTOR = msg({
	message: 'Forgot password',
	comment: 'Short label in the authentication forgot password page. Keep the tone plain and specific.',
});
interface ForgotPasswordPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const ForgotPasswordPageContent = observer(function ForgotPasswordPageContent({
	runtimeSnapshot,
}: ForgotPasswordPageContentProps) {
	const {i18n} = useLingui();
	const {form, emailId, isSent} = useForgotPasswordForm(authRequestTargetFromSnapshot(runtimeSnapshot));
	const redirect = resolveForgotPasswordRedirect(runtimeSnapshot);
	useEffect(() => {
		if (redirect !== null) {
			RouterUtils.replaceWith(redirect);
		}
	}, [redirect]);
	if (redirect !== null) {
		return null;
	}
	if (isSent) {
		return (
			<div className={styles.container} data-flx="auth.forgot-password-page.container">
				<h1 className={styles.title} data-flx="auth.forgot-password-page.title">
					<Trans>Check your email</Trans>
				</h1>
				<p className={styles.description} role="status" data-flx="auth.forgot-password-page.description">
					<Trans>
						If an account uses that address, we've sent it password reset instructions. Check your inbox for the reset
						link.
					</Trans>
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
					submitting={form.isSubmitting}
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

const ForgotPasswordPage = observer(function ForgotPasswordPage() {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(FORGOT_PASSWORD_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	return (
		<AuthRuntimeTargetGate data-flx="auth.forgot-password-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<ForgotPasswordPageContent runtimeSnapshot={runtimeSnapshot} data-flx="auth.forgot-password-page.content" />
			)}
		</AuthRuntimeTargetGate>
	);
});

export default ForgotPasswordPage;
