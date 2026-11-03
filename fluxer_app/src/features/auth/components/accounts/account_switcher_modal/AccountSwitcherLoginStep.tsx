// SPDX-License-Identifier: AGPL-3.0-or-later

import {FORGOT_YOUR_PASSWORD_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import authStyles from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow.module.css';
import {
	type AccountSwitcherAuthPurpose,
	AccountSwitcherAuthPurposeType,
	getReloginIdentifier,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import type {StepTitleChangeHandler} from '@app/features/auth/flow/useReportStepTitle';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import type {AuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';

interface AccountSwitcherLoginStepProps {
	readonly purpose: AccountSwitcherAuthPurpose;
	readonly redirectAfterLogin: string | null;
	readonly runtimeTarget: AuthRuntimeTarget;
	readonly title: string | null;
	readonly onBackActionChange: (action: (() => void) | null) => void;
	readonly onStepTitleChange: StepTitleChangeHandler | null;
	readonly onForgotPassword: () => void;
	readonly onLoginComplete: (payload: LoginSuccessPayload) => Promise<void> | void;
	readonly onRegister: () => void;
}

export function AccountSwitcherLoginStep({
	purpose,
	redirectAfterLogin,
	runtimeTarget,
	title,
	onBackActionChange,
	onStepTitleChange,
	onForgotPassword,
	onLoginComplete,
	onRegister,
}: AccountSwitcherLoginStepProps): React.ReactElement {
	const {i18n} = useLingui();
	const registerAction = useMemo(
		() => (
			<button
				type="button"
				onClick={onRegister}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-login-step.button.register"
			>
				{i18n._(REGISTER_DESCRIPTOR)}
			</button>
		),
		[i18n, onRegister],
	);
	const forgotPasswordAction = useMemo(
		() => (
			<button
				type="button"
				className={authStyles.inlineLink}
				onClick={onForgotPassword}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-login-step.button.forgot-password"
			>
				{i18n._(FORGOT_YOUR_PASSWORD_DESCRIPTOR)}
			</button>
		),
		[i18n, onForgotPassword],
	);
	return (
		<AuthLoginLayout
			redirectPath={null}
			inviteCode={null}
			desktopHandoff={false}
			excludeCurrentUser={false}
			extraTopContent={null}
			completeLoginRedirectPath={redirectAfterLogin}
			forgotPasswordAction={forgotPasswordAction}
			showTitle={onStepTitleChange == null}
			title={title}
			forceCredentials={purpose.type === AccountSwitcherAuthPurposeType.RELOGIN}
			initialIdentifier={getReloginIdentifier(purpose)}
			runtimeTarget={runtimeTarget}
			onBackActionChange={onBackActionChange}
			onStepTitleChange={onStepTitleChange}
			startWithAddAccount={purpose.type === AccountSwitcherAuthPurposeType.ADD}
			registerLink={registerAction}
			ssoRedirectPath={null}
			suppressInlineBackButtons={true}
			allowSsoAutoStart={false}
			onLoginComplete={onLoginComplete}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-login-step.auth-login-layout"
		/>
	);
}
