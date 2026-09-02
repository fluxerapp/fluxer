// SPDX-License-Identifier: AGPL-3.0-or-later

import {useHashParam} from '@app/features/app/hooks/useHashParam';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/ResetPasswordPage.module.css';
import FormField from '@app/features/auth/flow/AuthFormField';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {BACK_TO_SIGN_IN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Button} from '@app/features/ui/button/Button';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useId} from 'react';

const SECURE_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Secure your account',
	comment: 'Short label in the authentication email revert page. Keep the tone plain and specific.',
});
const NEW_PASSWORD_DESCRIPTOR = msg({
	message: 'New password',
	comment: 'Short label in the authentication email revert page. Keep the tone plain and specific.',
});
const CONFIRM_NEW_PASSWORD_DESCRIPTOR = msg({
	message: 'Confirm new password',
	comment: 'Short label in the authentication email revert page. Keep the tone plain and specific.',
});
const EmailRevertPage = observer(function EmailRevertPage() {
	const {i18n} = useLingui();
	const passwordId = useId();
	const confirmPasswordId = useId();
	useFluxerDocumentTitle(i18n._(SECURE_YOUR_ACCOUNT_DESCRIPTOR));
	const token = useHashParam('token');
	const {form, isLoading, fieldErrors} = useAuthForm({
		initialValues: {
			password: '',
			confirmPassword: '',
		},
		onSubmit: async (values) => {
			if (!token) {
				form.setError('password', 'Invalid or missing revert token');
				return;
			}
			if (values.password !== values.confirmPassword) {
				form.setError('confirmPassword', 'Passwords do not match');
				return;
			}
			const response = await AuthenticationCommands.revertEmailChange(token, values.password);
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			await AuthenticationCommands.completeLogin({
				token: response.token,
				userId: response.user_id,
				...(userData ? {userData} : {}),
			});
		},
		firstFieldName: 'password',
	});
	useEffect(() => {
		if (!token) {
			RouterUtils.replaceWith('/login');
		}
	}, [token]);
	return (
		<>
			<h1 className={styles.title} data-flx="auth.email-revert-page.title">
				<Trans>Secure your account</Trans>
			</h1>
			<p className={styles.description} data-flx="auth.email-revert-page.description">
				<Trans>
					We'll restore your previous email, sign out old sessions, remove phone numbers, disable MFA, and secure your
					account with a new password.
				</Trans>
			</p>
			<form className={styles.form} onSubmit={form.handleSubmit} data-flx="auth.email-revert-page.form.submit">
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
					data-flx="auth.email-revert-page.form-field.set-value.password"
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
					data-flx="auth.email-revert-page.form-field.set-value.password--2"
				/>
				<Button
					type="submit"
					fitContainer
					disabled={isLoading || form.isSubmitting}
					data-flx="auth.email-revert-page.button.submit"
				>
					<Trans>Restore account</Trans>
				</Button>
			</form>
			<div className={styles.footer} data-flx="auth.email-revert-page.footer">
				<AuthRouterLink to="/login" className={styles.link} data-flx="auth.email-revert-page.link">
					{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
				</AuthRouterLink>
			</div>
		</>
	);
});

export default EmailRevertPage;
