// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AccountSwitcherAuthShell} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthShell';
import {
	type AccountSwitcherAuthPurpose,
	AccountSwitcherAuthStep,
	getInitialRuntimeSnapshot,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {AccountSwitcherForgotPasswordStep} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherForgotPasswordStep';
import {AccountSwitcherLoginStep} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherLoginStep';
import {AccountSwitcherRegisterStep} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherRegisterStep';
import {shouldRenderInstanceStep} from '@app/features/auth/flow/auth_login_core/AuthLoginInstanceStep';
import MFAScreen from '@app/features/auth/flow/MfaScreen';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {
	AuthCardVariant,
	AuthLayoutContentMode,
	type AuthLayoutContextType,
} from '@app/features/auth/state/AuthLayoutContext';
import {AuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useState} from 'react';

interface AccountSwitcherAuthFlowProps {
	readonly purpose: AccountSwitcherAuthPurpose;
	readonly redirectAfterLogin: string | null;
	readonly onBackToAccounts: () => void;
	readonly onLoginComplete: (payload: LoginSuccessPayload) => Promise<void> | void;
}

function resolveInitialRuntimeSnapshot(purpose: AccountSwitcherAuthPurpose): RuntimeConfigSnapshot | null {
	const purposeSnapshot = getInitialRuntimeSnapshot(purpose);
	if (purposeSnapshot != null) {
		return purposeSnapshot;
	}
	if (shouldRenderInstanceStep(null)) {
		return null;
	}
	return RuntimeConfig.getSnapshotOrNull();
}

function requireSelectedRuntimeSnapshot(runtimeTarget: AuthRuntimeTarget): RuntimeConfigSnapshot {
	const runtimeSnapshot = runtimeTarget.snapshot;
	if (runtimeSnapshot == null) {
		throw new Error('Account-switcher authentication requires its selected instance runtime');
	}
	return runtimeSnapshot;
}

export const AccountSwitcherAuthFlow = observer(function AccountSwitcherAuthFlow({
	purpose,
	redirectAfterLogin,
	onBackToAccounts,
	onLoginComplete,
}: AccountSwitcherAuthFlowProps): React.ReactElement {
	const [selectedAuthStep, setSelectedAuthStep] = useState<AccountSwitcherAuthStep>(AccountSwitcherAuthStep.LOGIN);
	const [cardVariant, setCardVariant] = useState<AuthCardVariant>(AuthCardVariant.STANDARD);
	const [, setContentMode] = useState<AuthLayoutContentMode>(AuthLayoutContentMode.CARD);
	const [, setSplashCardAlignment] = useState<GuildSplashCardAlignmentValue>(GuildSplashCardAlignment.CENTER);
	const [loginBackAction, setLoginBackAction] = useState<(() => void) | null>(null);
	const [mfaChallengePurpose, setMfaChallengePurpose] = useState<AccountSwitcherAuthPurpose | null>(null);
	const authRuntimeTarget = useMemo(
		() => AuthRuntimeTarget.forInstanceSelection(resolveInitialRuntimeSnapshot(purpose)),
		[purpose],
	);
	const setSplashUrl = useCallback(() => {}, []);
	const authLayoutContextValue = useMemo<AuthLayoutContextType>(
		() => ({setSplashUrl, setCardVariant, setContentMode, setSplashCardAlignment}),
		[setSplashUrl],
	);
	const mfaTicket = Authentication.currentMfaTicket;
	const mfaMethods = Authentication.availableMfaMethods;
	const hasMfaChallenge = mfaChallengePurpose === purpose && mfaTicket != null && mfaMethods != null;
	let authStep: AccountSwitcherAuthStep;
	if (hasMfaChallenge) {
		authStep = AccountSwitcherAuthStep.MFA;
	} else {
		authStep = selectedAuthStep;
	}
	useLayoutEffect(() => {
		AuthenticationCommands.clearMfaTicket();
		setMfaChallengePurpose(purpose);
		setSelectedAuthStep(AccountSwitcherAuthStep.LOGIN);
		setLoginBackAction(null);
		return () => {
			AuthenticationCommands.clearMfaTicket();
		};
	}, [purpose]);
	const showRegisterStep = useCallback(() => {
		setSelectedAuthStep(AccountSwitcherAuthStep.REGISTER);
	}, []);
	const showForgotPasswordStep = useCallback(() => {
		setSelectedAuthStep(AccountSwitcherAuthStep.FORGOT);
	}, []);
	const showLoginStep = useCallback(() => {
		setSelectedAuthStep(AccountSwitcherAuthStep.LOGIN);
	}, []);
	const handleLoginBackActionChange = useCallback((action: (() => void) | null) => {
		setLoginBackAction(() => action);
	}, []);
	const handleMfaSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			const runtimeSnapshot = requireSelectedRuntimeSnapshot(authRuntimeTarget);
			await AuthenticationCommands.completeLogin({...payload, runtimeSnapshot}, {redirectPath: redirectAfterLogin});
			AuthenticationCommands.clearMfaTicket();
			await onLoginComplete(payload);
		},
		[authRuntimeTarget, onLoginComplete, redirectAfterLogin],
	);
	const handleMfaCancel = useCallback(() => {
		AuthenticationCommands.clearMfaTicket();
	}, []);
	const handleBrowserLoginComplete = useCallback(
		async (payload: LoginSuccessPayload) => {
			const runtimeSnapshot = requireSelectedRuntimeSnapshot(authRuntimeTarget);
			await AuthenticationCommands.completeLogin({...payload, runtimeSnapshot}, {redirectPath: redirectAfterLogin});
			await onLoginComplete(payload);
		},
		[authRuntimeTarget, onLoginComplete, redirectAfterLogin],
	);
	const handleRegisterComplete = useCallback(
		async (response: AuthenticationCommands.TokenResponse) => {
			const runtimeSnapshot = requireSelectedRuntimeSnapshot(authRuntimeTarget);
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			const payload: LoginSuccessPayload = {
				token: response.token,
				userId: response.user_id,
				...(userData ? {userData} : {}),
			};
			await AuthenticationCommands.completeLogin({...payload, runtimeSnapshot}, {redirectPath: redirectAfterLogin});
			await onLoginComplete(payload);
		},
		[authRuntimeTarget, onLoginComplete, redirectAfterLogin],
	);
	const handleBack = useCallback(() => {
		switch (authStep) {
			case AccountSwitcherAuthStep.MFA:
				AuthenticationCommands.clearMfaTicket();
				return;
			case AccountSwitcherAuthStep.REGISTER:
			case AccountSwitcherAuthStep.FORGOT:
				showLoginStep();
				return;
			case AccountSwitcherAuthStep.LOGIN:
				break;
		}
		if (loginBackAction != null) {
			loginBackAction();
			return;
		}
		onBackToAccounts();
	}, [authStep, loginBackAction, onBackToAccounts, showLoginStep]);
	const renderAuthStep = (): React.ReactNode => {
		switch (authStep) {
			case AccountSwitcherAuthStep.MFA:
				if (mfaTicket == null || mfaMethods == null) {
					return null;
				}
				return (
					<MFAScreen
						challenge={{ticket: mfaTicket, ...mfaMethods}}
						onSuccess={handleMfaSuccess}
						onCancel={handleMfaCancel}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.mfa-screen"
					/>
				);
			case AccountSwitcherAuthStep.REGISTER:
				return (
					<AccountSwitcherRegisterStep
						runtimeSnapshot={requireSelectedRuntimeSnapshot(authRuntimeTarget)}
						onRegister={handleRegisterComplete}
						onBrowserLogin={handleBrowserLoginComplete}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.register-step"
					/>
				);
			case AccountSwitcherAuthStep.FORGOT:
				return (
					<AccountSwitcherForgotPasswordStep
						runtimeSnapshot={requireSelectedRuntimeSnapshot(authRuntimeTarget)}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.forgot-password-step"
					/>
				);
			case AccountSwitcherAuthStep.LOGIN:
				return (
					<AccountSwitcherLoginStep
						purpose={purpose}
						redirectAfterLogin={redirectAfterLogin}
						runtimeTarget={authRuntimeTarget}
						onBackActionChange={handleLoginBackActionChange}
						onForgotPassword={showForgotPasswordStep}
						onLoginComplete={onLoginComplete}
						onRegister={showRegisterStep}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.login-step"
					/>
				);
		}
	};
	const returnsToAccountList = authStep === AccountSwitcherAuthStep.LOGIN && loginBackAction == null;
	return (
		<AccountSwitcherAuthShell
			cardVariant={cardVariant}
			layoutContextValue={authLayoutContextValue}
			step={authStep}
			returnsToAccountList={returnsToAccountList}
			onBack={handleBack}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.shell"
		>
			{renderAuthStep()}
		</AccountSwitcherAuthShell>
	);
});
