// SPDX-License-Identifier: AGPL-3.0-or-later

import {FORGOT_YOUR_PASSWORD_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import authStyles from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow.module.css';
import {
	type AccountSwitcherAuthPurpose,
	AccountSwitcherAuthPurposeType,
	getReloginAccountLabel,
	getReloginIdentifier,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import type {AuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';

const SIGN_IN_AGAIN_AS_DESCRIPTOR = msg({
	message: 'Sign in again as {accountLabel}',
	comment:
		'Heading shown while re-authenticating one saved account from the account switcher. Account display name is interpolated.',
});

interface AccountSwitcherLoginStepProps {
	readonly purpose: AccountSwitcherAuthPurpose;
	readonly redirectAfterLogin: string | null;
	readonly runtimeTarget: AuthRuntimeTarget;
	readonly onBackActionChange: (action: (() => void) | null) => void;
	readonly onForgotPassword: () => void;
	readonly onLoginComplete: (payload: LoginSuccessPayload) => Promise<void> | void;
	readonly onRegister: () => void;
}

export function AccountSwitcherLoginStep({
	purpose,
	redirectAfterLogin,
	runtimeTarget,
	onBackActionChange,
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
	const reloginTitle = useMemo<React.ReactNode>(() => {
		const accountLabel = getReloginAccountLabel(purpose);
		if (accountLabel == null) {
			return null;
		}
		return i18n._(SIGN_IN_AGAIN_AS_DESCRIPTOR, {accountLabel});
	}, [i18n, purpose]);
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
			showTitle={true}
			title={reloginTitle}
			forceCredentials={purpose.type === AccountSwitcherAuthPurposeType.RELOGIN}
			initialIdentifier={getReloginIdentifier(purpose)}
			runtimeTarget={runtimeTarget}
			showInstanceSelector={null}
			onBackActionChange={onBackActionChange}
			startWithAddAccount={purpose.type === AccountSwitcherAuthPurposeType.ADD}
			registerLink={registerAction}
			ssoRedirectPath={null}
			suppressInlineBackButtons={true}
			onLoginComplete={onLoginComplete}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-login-step.auth-login-layout"
		/>
	);
}
