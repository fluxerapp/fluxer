// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {FORGOT_YOUR_PASSWORD_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import styles from '@app/features/auth/components/pages/ForgotPasswordPage.module.css';
import FormField from '@app/features/auth/flow/AuthFormField';
import {useForgotPasswordForm} from '@app/features/auth/flow/useForgotPasswordForm';
import {type StepTitleChangeHandler, useReportStepTitle} from '@app/features/auth/flow/useReportStepTitle';
import {EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const CHECK_YOUR_EMAIL_DESCRIPTOR = msg({
	message: 'Check your email',
	comment: 'Heading shown after a password reset link was requested.',
});

interface ForgotPasswordFormProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly sentFooter?: ReactNode;
	readonly formFooter?: ReactNode;
	readonly renderContainer?: (children: ReactNode, isSent: boolean) => ReactNode;
	readonly onStepTitleChange?: StepTitleChangeHandler | null;
}

export const ForgotPasswordForm = observer(function ForgotPasswordForm({
	runtimeSnapshot,
	sentFooter,
	formFooter,
	renderContainer,
	onStepTitleChange = null,
}: ForgotPasswordFormProps) {
	const {i18n} = useLingui();
	const {form, emailId, isSent} = useForgotPasswordForm(instanceTargetFromSnapshot(runtimeSnapshot));
	const title = i18n._(isSent ? CHECK_YOUR_EMAIL_DESCRIPTOR : FORGOT_YOUR_PASSWORD_DESCRIPTOR);
	useReportStepTitle(onStepTitleChange, title);
	const heading =
		onStepTitleChange == null ? (
			<h1 className={styles.title} data-flx="auth.forgot-password-page.title">
				{title}
			</h1>
		) : null;
	if (isSent) {
		const content = (
			<>
				{heading}
				<p className={styles.description} role="status" data-flx="auth.forgot-password-page.description">
					<Trans>
						If an account uses that address, we've sent it password reset instructions. Check your inbox for the reset
						link.
					</Trans>
				</p>
				{sentFooter}
			</>
		);
		return renderContainer ? (
			renderContainer(content, true)
		) : (
			<div className={styles.container} data-flx="auth.forgot-password-page.container">
				{content}
			</div>
		);
	}
	const content = (
		<>
			{heading}
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
			{formFooter}
		</>
	);
	return renderContainer ? renderContainer(content, false) : content;
});
