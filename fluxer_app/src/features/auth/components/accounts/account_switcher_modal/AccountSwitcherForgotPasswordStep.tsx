// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import forgotStyles from '@app/features/auth/components/pages/ForgotPasswordPage.module.css';
import FormField from '@app/features/auth/flow/AuthFormField';
import {useForgotPasswordForm} from '@app/features/auth/flow/useForgotPasswordForm';
import {authRequestTargetFromSnapshot} from '@app/features/auth/state/AuthRequestTarget';
import {EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {flxElementClassName} from '@app/lib/react';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

interface AccountSwitcherForgotPasswordStepProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

export function AccountSwitcherForgotPasswordStep({
	runtimeSnapshot,
}: AccountSwitcherForgotPasswordStepProps): React.ReactElement {
	const {i18n} = useLingui();
	const {form, emailId, isSent} = useForgotPasswordForm(authRequestTargetFromSnapshot(runtimeSnapshot));
	if (isSent) {
		return (
			<flx-auth-account-switcher-forgot-password-confirmation
				className={flxElementClassName(forgotStyles.container)}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.confirmation"
			>
				<h1
					className={forgotStyles.title}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.confirmation-title"
				>
					<Trans>Check your email</Trans>
				</h1>
				<p
					className={forgotStyles.description}
					role="status"
					data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.confirmation-description"
				>
					<Trans>
						If an account uses that address, we've sent it password reset instructions. Check your inbox for the reset
						link.
					</Trans>
				</p>
			</flx-auth-account-switcher-forgot-password-confirmation>
		);
	}
	return (
		<flx-auth-account-switcher-forgot-password
			className={flxElementClassName()}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.root"
		>
			<h1
				className={forgotStyles.title}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.title"
			>
				<Trans>Forgot your password?</Trans>
			</h1>
			<p
				className={forgotStyles.description}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.description"
			>
				<Trans>Enter your email address and we'll send you a link to reset your password.</Trans>
			</p>
			<form
				className={forgotStyles.form}
				onSubmit={form.handleSubmit}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.form.submit"
			>
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
					data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.form-field.email"
				/>
				<Button
					type="submit"
					fitContainer
					disabled={form.isSubmitting}
					submitting={form.isSubmitting}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.button.submit"
				>
					<Trans>Send reset link</Trans>
				</Button>
			</form>
		</flx-auth-account-switcher-forgot-password>
	);
}
