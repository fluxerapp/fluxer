// SPDX-License-Identifier: AGPL-3.0-or-later

import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import AuthLoginEmailPasswordForm, {
	type AuthFormControllerLike,
} from '@app/features/auth/flow/auth_login_core/AuthLoginEmailPasswordForm';
import type {ReactNode} from 'react';

interface AuthLoginCredentialStepPanelProps {
	readonly disableSubmit?: boolean;
	readonly extraTopContent?: ReactNode;
	readonly fieldErrors: ReadonlyMap<string, string> | null;
	readonly footerStart?: ReactNode;
	readonly forgotPasswordLink?: ReactNode;
	readonly identifierField?: 'email' | 'login';
	readonly form: AuthFormControllerLike;
	readonly isLoading: boolean;
	readonly showTitle: boolean;
	readonly statusMessage?: string | null;
	readonly submitLabel: ReactNode;
	readonly switchError?: string | null;
	readonly title: ReactNode;
}

export function AuthLoginCredentialStepPanel({
	disableSubmit,
	extraTopContent,
	fieldErrors,
	footerStart,
	forgotPasswordLink,
	identifierField,
	form,
	isLoading,
	showTitle,
	statusMessage,
	submitLabel,
	switchError,
	title,
}: AuthLoginCredentialStepPanelProps) {
	const renderSwitchError = (): ReactNode => {
		if (switchError == null || switchError.length === 0) {
			return null;
		}
		return (
			<div
				className={loginStyles.loginNotice}
				role="alert"
				data-flx="auth.flow.auth-login-core.auth-login-credential-step-panel.render-switch-error.alert"
			>
				{switchError}
			</div>
		);
	};
	return (
		<flx-auth-login-credential-step-panel
			className="flx-element"
			data-flx="auth.flow.auth-login-core.auth-login-credential-step-panel.flx-element"
		>
			{extraTopContent}
			{showTitle ? (
				<h1 className={loginStyles.title} data-flx="auth.flow.auth-login-core.auth-login-credential-step-panel.h1">
					{title}
				</h1>
			) : null}
			{renderSwitchError()}
			<AuthLoginEmailPasswordForm
				form={form}
				isLoading={isLoading}
				fieldErrors={fieldErrors}
				submitLabel={submitLabel}
				classes={{form: loginStyles.form}}
				linksWrapperClassName={loginStyles.formLinks}
				links={forgotPasswordLink}
				identifierField={identifierField}
				disableSubmit={disableSubmit}
				footerStart={footerStart}
				statusMessage={statusMessage}
				data-flx="auth.flow.auth-login-core.auth-login-credential-step-panel.auth-login-email-password-form"
			/>
		</flx-auth-login-credential-step-panel>
	);
}
