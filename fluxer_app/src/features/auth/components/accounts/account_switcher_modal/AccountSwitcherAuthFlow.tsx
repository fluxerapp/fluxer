// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {SIGN_IN_AGAIN_AS_DESCRIPTOR} from '@app/features/auth/components/accounts/AccountList';
import {AccountSwitcherAuthShell} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthShell';
import {
	type AccountSwitcherAuthPurpose,
	AccountSwitcherAuthPurposeType,
	AccountSwitcherAuthStep,
	getInitialRuntimeSnapshot,
	getReloginAccountLabel,
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
import {
	BACK_DESCRIPTOR,
	CANCEL_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {flxElementClassName} from '@app/lib/react';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const BACK_TO_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Back to accounts',
	comment: 'Button above the sign-in form in the account switcher. It goes back to the list of saved accounts.',
});

const ENTRY_FOCUS_SELECTOR = '[data-step-focus="true"], input:not([type="hidden"]):not([disabled])';

export interface AccountSwitcherAuthFrame {
	readonly title: string;
	readonly backLabel: string;
	readonly onBack: () => void;
	readonly content: React.ReactElement;
}

export const ADD_AN_ACCOUNT_DESCRIPTOR = msg({
	message: 'Add an account',
	comment:
		'Last tile in the account switcher grid, and the title of the step it opens, where another saved account is added by signing in or registering.',
});

function getPurposeTitle(purpose: AccountSwitcherAuthPurpose, i18n: I18n): string | null {
	if (purpose.type === AccountSwitcherAuthPurposeType.ADD) {
		return i18n._(ADD_AN_ACCOUNT_DESCRIPTOR);
	}
	const accountLabel = getReloginAccountLabel(purpose);
	if (accountLabel == null) {
		return null;
	}
	return i18n._(SIGN_IN_AGAIN_AS_DESCRIPTOR, {accountLabel});
}

interface AccountSwitcherAuthFlowProps {
	readonly purpose: AccountSwitcherAuthPurpose;
	readonly renderFrame: ((frame: AccountSwitcherAuthFrame) => React.ReactElement) | null;
	readonly redirectAfterLogin: string | null;
	readonly focusOnEntry: boolean;
	readonly exitCancels: boolean;
	readonly onExit: () => void;
	readonly onLoginComplete: (payload: LoginSuccessPayload) => Promise<void> | void;
}

function createAuthRuntimeTarget(purpose: AccountSwitcherAuthPurpose): AuthRuntimeTarget {
	const purposeSnapshot = getInitialRuntimeSnapshot(purpose);
	if (purposeSnapshot != null) {
		return AuthRuntimeTarget.forFixedInstance(purposeSnapshot);
	}
	if (shouldRenderInstanceStep()) {
		return AuthRuntimeTarget.forInstanceSelection(null);
	}
	return AuthRuntimeTarget.forInstanceSelection(RuntimeConfig.getSnapshotOrNull());
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
	renderFrame,
	redirectAfterLogin,
	focusOnEntry,
	exitCancels,
	onExit,
	onLoginComplete,
}: AccountSwitcherAuthFlowProps): React.ReactElement {
	const {i18n} = useLingui();
	const [selectedAuthStep, setSelectedAuthStep] = useState<AccountSwitcherAuthStep>(AccountSwitcherAuthStep.LOGIN);
	const [, setCardVariant] = useState<AuthCardVariant>(AuthCardVariant.STANDARD);
	const [, setContentMode] = useState<AuthLayoutContentMode>(AuthLayoutContentMode.CARD);
	const [, setSplashCardAlignment] = useState<GuildSplashCardAlignmentValue>(GuildSplashCardAlignment.CENTER);
	const [loginBackAction, setLoginBackAction] = useState<(() => void) | null>(null);
	const [stepTitle, setStepTitle] = useState<string | null>(null);
	const [mfaChallengePurpose, setMfaChallengePurpose] = useState<AccountSwitcherAuthPurpose | null>(null);
	const authRuntimeTarget = useMemo(() => createAuthRuntimeTarget(purpose), [purpose]);
	const stepTitleChange = renderFrame == null ? null : setStepTitle;
	const purposeTitle = getPurposeTitle(purpose, i18n);
	const setSplashUrl = useCallback(() => {}, []);
	const setBrandSnapshot = useCallback(() => {}, []);
	const authLayoutContextValue = useMemo<AuthLayoutContextType>(
		() => ({setSplashUrl, setBrandSnapshot, setCardVariant, setContentMode, setSplashCardAlignment}),
		[setBrandSnapshot, setSplashUrl],
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
	const shellRef = useRef<HTMLElement | null>(null);
	useEffect(() => {
		if (!focusOnEntry) {
			return;
		}
		shellRef.current?.querySelector<HTMLElement>(ENTRY_FOCUS_SELECTOR)?.focus();
	}, [focusOnEntry, purpose]);
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
		onExit();
	}, [accountRestriction, authStep, handleBackToSignIn, loginBackAction, onExit, showLoginStep]);
	const renderAuthStep = (): React.ReactNode => {
		if (accountRestriction != null) {
			return (
				<AccountRestrictionScreen
					restriction={accountRestriction}
					onBackToSignIn={handleBackToSignIn}
					onTryAgain={AuthenticationCommands.clearAccountRestriction}
					onStepTitleChange={stepTitleChange}
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
						onStepTitleChange={stepTitleChange}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.mfa-screen"
					/>
				);
			case AccountSwitcherAuthStep.REGISTER:
				return (
					<AccountSwitcherRegisterStep
						runtimeSnapshot={requireSelectedRuntimeSnapshot(authRuntimeTarget)}
						onStepTitleChange={stepTitleChange}
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
							onStepTitleChange={stepTitleChange}
							data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.recover-account-step"
						/>
					);
				}
				return (
					<ForgotPasswordForm
						runtimeSnapshot={runtimeSnapshot}
						renderContainer={renderForgotPasswordContainer}
						onStepTitleChange={stepTitleChange}
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
						title={purposeTitle}
						onBackActionChange={handleLoginBackActionChange}
						onStepTitleChange={stepTitleChange}
						onForgotPassword={showForgotPasswordStep}
						onLoginComplete={onLoginComplete}
						onRegister={showRegisterStep}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.login-step"
					/>
				);
		}
	};
	const exitsFlow = accountRestriction == null && authStep === AccountSwitcherAuthStep.LOGIN && loginBackAction == null;
	let backLabel = i18n._(BACK_DESCRIPTOR);
	if (exitsFlow) {
		backLabel = i18n._(exitCancels ? CANCEL_DESCRIPTOR : BACK_TO_ACCOUNTS_DESCRIPTOR);
	}
	if (renderFrame == null) {
		return (
			<AccountSwitcherAuthShell
				layoutContextValue={authLayoutContextValue}
				step={authStep}
				back={{label: backLabel, onBack: handleBack}}
				rootRef={shellRef}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.shell"
			>
				{renderAuthStep()}
			</AccountSwitcherAuthShell>
		);
	}
	return renderFrame({
		title: stepTitle ?? purposeTitle ?? i18n._(SIGN_IN_DESCRIPTOR),
		backLabel,
		onBack: handleBack,
		content: (
			<AccountSwitcherAuthShell
				layoutContextValue={authLayoutContextValue}
				step={authStep}
				back={null}
				rootRef={shellRef}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-flow.framed-shell"
			>
				{renderAuthStep()}
			</AccountSwitcherAuthShell>
		),
	});
});
