// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AccountSwitcherAuthShell} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthShell';
import {
	type AccountSwitcherAuthPurpose,
	AccountSwitcherAuthStep,
	getInitialRuntimeSnapshot,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {AccountSwitcherLoginStep} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherLoginStep';
import {AccountSwitcherRegisterStep} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherRegisterStep';
import forgotStyles from '@app/features/auth/components/pages/ForgotPasswordPage.module.css';
import {RecoverAccountForm} from '@app/features/auth/components/pages/RecoverAccountPage';
import {AccountRestrictionScreen} from '@app/features/auth/flow/AccountRestrictionScreen';
import {shouldRenderInstanceStep} from '@app/features/auth/flow/auth_login_core/AuthLoginInstanceStep';
import {ForgotPasswordForm} from '@app/features/auth/flow/ForgotPasswordForm';
import MFAScreen from '@app/features/auth/flow/MfaScreen';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {
	AuthCardVariant,
	AuthLayoutContentMode,
	type AuthLayoutContextType,
} from '@app/features/auth/state/AuthLayoutContext';
import {AuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {flxElementClassName} from '@app/lib/react';
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
	if (shouldRenderInstanceStep()) {
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

function renderForgotPasswordContainer(children: React.ReactNode, isSent: boolean): React.ReactElement {
	if (isSent) {
		return (
			<flx-auth-account-switcher-forgot-password-confirmation
				className={flxElementClassName(forgotStyles.container)}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.confirmation"
			>
				{children}
			</flx-auth-account-switcher-forgot-password-confirmation>
		);
	}
	return (
		<flx-auth-account-switcher-forgot-password
			className={flxElementClassName()}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-forgot-password-step.root"
		>
			{children}
		</flx-auth-account-switcher-forgot-password>
	);
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
	const accountRestriction = Authentication.accountRestriction;
	const mfaTicket = Authentication.currentMfaTicket;
	const mfaMethods = Authentication.availableMfaMethods;
	const hasMfaChallenge = mfaChallengePurpose === purpose && mfaTicket != null && mfaMethods != null;
	let authStep: AccountSwitcherAuthStep;
	if (hasMfaChallenge && selectedAuthStep !== AccountSwitcherAuthStep.FORGOT) {
		authStep = AccountSwitcherAuthStep.MFA;
	} else {
		authStep = selectedAuthStep;
	}
	useLayoutEffect(() => {
		AuthenticationCommands.clearMfaTicket();
		AuthenticationCommands.clearAccountRestriction();
		setMfaChallengePurpose(purpose);
		setSelectedAuthStep(AccountSwitcherAuthStep.LOGIN);
		setLoginBackAction(null);
		return () => {
			AuthenticationCommands.clearMfaTicket();
			AuthenticationCommands.clearAccountRestriction();
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
	const handleAccountRecovered = useCallback(
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
	const handleBackToSignIn = useCallback(() => {
		AuthenticationCommands.clearAccountRestriction();
		showLoginStep();
	}, [showLoginStep]);
	const handleBack = useCallback(() => {
		if (accountRestriction != null) {
			handleBackToSignIn();
			return;
		}
		switch (authStep) {
			case AccountSwitcherAuthStep.MFA:
				AuthenticationCommands.clearMfaTicket();
				return;
			case AccountSwitcherAuthStep.REGISTER:
				showLoginStep();
				return;
			case AccountSwitcherAuthStep.FORGOT:
				AuthenticationCommands.clearMfaTicket();
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
	}, [accountRestriction, authStep, handleBackToSignIn, loginBackAction, onBackToAccounts, showLoginStep]);
	const renderAuthStep = (): React.ReactNode => {
		if (accountRestriction != null) {
			return (
				<AccountRestrictionScreen
					restriction={accountRestriction}
					onBackToSignIn={handleBackToSignIn}
					onTryAgain={AuthenticationCommands.clearAccountRestriction}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.account-restriction-screen"
				/>
			);
		}
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
			case AccountSwitcherAuthStep.FORGOT: {
				const runtimeSnapshot = requireSelectedRuntimeSnapshot(authRuntimeTarget);
				if (usesUsernameSignIn(runtimeSnapshot.features)) {
					return (
						<RecoverAccountForm
							runtimeSnapshot={runtimeSnapshot}
							onRecovered={handleAccountRecovered}
							data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.recover-account-step"
						/>
					);
				}
				return (
					<ForgotPasswordForm
						runtimeSnapshot={runtimeSnapshot}
						renderContainer={renderForgotPasswordContainer}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.forgot-password-step"
					/>
				);
			}
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
