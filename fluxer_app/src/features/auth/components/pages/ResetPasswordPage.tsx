// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {useHashParam} from '@app/features/app/hooks/useHashParam';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/ResetPasswordPage.module.css';
import {resolveMissingResetTokenRedirect} from '@app/features/auth/flow/AccountRecoveryRedirects';
import FormField from '@app/features/auth/flow/AuthFormField';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {resetPassword as resetPasswordFlow} from '@app/features/auth/state/AuthFlow';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {authRequestTargetFromSnapshot} from '@app/features/auth/state/AuthRequestTarget';
import {
	checkResetPasswordToken,
	peekSettledResetPasswordTokenCheck,
	takeSettledResetPasswordTokenCheck,
} from '@app/features/auth/state/ResetPasswordTokenCheck';
import {
	BACK_TO_SIGN_IN_DESCRIPTOR,
	CONFIRM_NEW_PASSWORD_DESCRIPTOR,
	NEW_PASSWORD_DESCRIPTOR,
	PASSWORDS_DO_NOT_MATCH_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Button} from '@app/features/ui/button/Button';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useId, useMemo, useState} from 'react';

const RESET_PASSWORD_DESCRIPTOR = msg({
	message: 'Reset password',
	comment: 'Short label in the authentication reset password page. Keep the tone plain and specific.',
});
const INVALID_RESET_TOKEN_DESCRIPTOR = msg({
	message: "This link isn't valid. Open the link from your email again.",
	comment: 'Validation error shown when a password reset URL has no usable reset token.',
});

type TokenStatus = 'validating' | 'valid' | 'invalid';

const API_RENDERED_FIELDS = new Set(['password']);

function resolveBannerError(
	error: string | null,
	fieldErrors: ReadonlyMap<string, string> | null | undefined,
): string | null {
	if (error != null) {
		return error;
	}
	if (fieldErrors == null) {
		return null;
	}
	for (const [fieldName, message] of fieldErrors) {
		if (!API_RENDERED_FIELDS.has(fieldName)) {
			return message;
		}
	}
	return null;
}

interface ResetPasswordPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const ResetPasswordPageContent = observer(function ResetPasswordPageContent({
	runtimeSnapshot,
}: ResetPasswordPageContentProps) {
	const {i18n} = useLingui();
	const passwordId = useId();
	const confirmPasswordId = useId();
	const token = useHashParam('token');
	const requestTarget = useMemo(() => authRequestTargetFromSnapshot(runtimeSnapshot), [runtimeSnapshot]);
	const [tokenStatus, setTokenStatus] = useState<TokenStatus>(() =>
		token === null ? 'validating' : (peekSettledResetPasswordTokenCheck(token, requestTarget) ?? 'validating'),
	);
	const {form, isLoading, error, fieldErrors} = useAuthForm({
		initialValues: {
			password: '',
			confirmPassword: '',
		},
		onSubmit: async (values) => {
			if (!token) {
				form.setError('password', i18n._(INVALID_RESET_TOKEN_DESCRIPTOR));
				return false;
			}
			if (values.password !== values.confirmPassword) {
				form.setError('confirmPassword', i18n._(PASSWORDS_DO_NOT_MATCH_DESCRIPTOR));
				return false;
			}
			const response = await resetPasswordFlow(token, values.password, requestTarget);
			if (response.type === 'mfa') {
				AuthenticationCommands.setMfaTicket({
					ticket: response.challenge.ticket,
					totp: response.challenge.totp,
					webauthn: response.challenge.webauthn,
					backupCodes: response.challenge.backupCodes,
					runtimeSnapshot,
				});
				RouterUtils.replaceWith('/login');
				return undefined;
			}
			await AuthenticationCommands.completeLogin({...response.payload, runtimeSnapshot});
			return undefined;
		},
	});
	const usernameSignIn = usesUsernameSignIn(runtimeSnapshot.features);
	useEffect(() => {
		if (!token) {
			RouterUtils.replaceWith(resolveMissingResetTokenRedirect(runtimeSnapshot));
			return;
		}
		const settled = takeSettledResetPasswordTokenCheck(token, requestTarget);
		if (settled !== null) {
			setTokenStatus(settled);
			return;
		}
		let cancelled = false;
		setTokenStatus('validating');
		void checkResetPasswordToken(token, requestTarget).then((status) => {
			if (cancelled) return;
			setTokenStatus(status);
		});
		return () => {
			cancelled = true;
		};
	}, [token, requestTarget, runtimeSnapshot]);
	const bannerError = resolveBannerError(error, fieldErrors);
	if (!token) {
		return null;
	}
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
				{usernameSignIn ? (
					<>
						<p className={styles.description} data-flx="auth.reset-password-page.description--username">
							<Trans>
								This reset link has expired or was already used. Ask an admin for a new one, or use your recovery kit.
							</Trans>
						</p>
						<div className={styles.footer} data-flx="auth.reset-password-page.footer--username">
							<AuthRouterLink
								to={Routes.RECOVER_ACCOUNT}
								className={styles.link}
								data-flx="auth.reset-password-page.link--recover"
							>
								<Trans>Use your recovery kit</Trans>
							</AuthRouterLink>
						</div>
					</>
				) : (
					<>
						<p className={styles.description} data-flx="auth.reset-password-page.description">
							<Trans>This reset link has expired. Reset links last 1 hour. Please request a new one.</Trans>
						</p>
						<div className={styles.footer} data-flx="auth.reset-password-page.footer">
							<AuthRouterLink to="/forgot" className={styles.link} data-flx="auth.reset-password-page.link">
								<Trans>Request a new reset link</Trans>
							</AuthRouterLink>
						</div>
					</>
				)}
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
					error={form.getError('password') || fieldErrors?.get('password')}
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

const ResetPasswordPage = observer(function ResetPasswordPage() {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(RESET_PASSWORD_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	return (
		<AuthRuntimeTargetGate data-flx="auth.reset-password-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<ResetPasswordPageContent runtimeSnapshot={runtimeSnapshot} data-flx="auth.reset-password-page.content" />
			)}
		</AuthRuntimeTargetGate>
	);
});

export default ResetPasswordPage;
