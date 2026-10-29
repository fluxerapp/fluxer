// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {detectDomainMigrationInstallKind} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig, {describeAPIEndpoint, type InstanceSsoConfig} from '@app/features/app/state/RuntimeConfig';
import {getAccountDisplayLabels} from '@app/features/auth/AccountDisplayUtils';
import {
	FORGOT_YOUR_PASSWORD_DESCRIPTOR,
	SIGN_IN_WITH_BROWSER_DESCRIPTOR,
} from '@app/features/auth/AuthMessageDescriptors';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AgeBlockedRemovalGate} from '@app/features/auth/components/AgeBlockedRemovalGate';
import {SIGN_IN_AGAIN_AS_DESCRIPTOR} from '@app/features/auth/components/accounts/AccountList';
import {AccountSelector} from '@app/features/auth/components/accounts/AccountSelector';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	AuthSsoPanel,
	isInstanceSsoAvailable,
	isSsoLoginEnforced,
	resolveAuthPanelSso,
	SsoButtonLabel,
} from '@app/features/auth/flow/AuthSsoPanel';
import {
	AuthLoginBrowserStep,
	resolveShouldOfferBrowserStep,
} from '@app/features/auth/flow/auth_login_core/AuthLoginBrowserStep';
import {AuthLoginCredentialStepPanel} from '@app/features/auth/flow/auth_login_core/AuthLoginCredentialStepPanel';
import {
	AuthLoginInstanceStep,
	shouldRenderInstanceStep,
} from '@app/features/auth/flow/auth_login_core/AuthLoginInstanceStep';
import {
	AuthLoginMethodStep,
	type AuthLoginSsoAction,
} from '@app/features/auth/flow/auth_login_core/AuthLoginMethodStep';
import {
	AUTH_LOGIN_STEP_ORDER,
	AuthLoginStep,
	DesktopHandoffMode,
	selectAuthLoginStep,
} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {hasInitialIdentifier, useAuthLoginFlow} from '@app/features/auth/flow/auth_login_core/useAuthLoginFlow';
import {useDesktopHandoffFlow} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {
	SIGN_IN_WITH_OLD_APP_DESCRIPTOR,
	showBrowserLoginHandoffModal,
} from '@app/features/auth/flow/BrowserLoginHandoffModal';
import {AuthClientPermissionStep} from '@app/features/auth/flow/client_intro/AuthClientPermissionStep';
import {AuthClientPreferencesStep} from '@app/features/auth/flow/client_intro/AuthClientPreferencesStep';
import {AuthClientWelcomeStep} from '@app/features/auth/flow/client_intro/AuthClientWelcomeStep';
import {useDesktopClientIntroFlow} from '@app/features/auth/flow/client_intro/useDesktopClientIntroFlow';
import DesktopHandoffAccountSelector from '@app/features/auth/flow/DesktopHandoffAccountSelector';
import {HandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import IpAuthorizationScreen from '@app/features/auth/flow/IpAuthorizationScreen';
import {AuthIdentifierFirstStep} from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep';
import {
	isSsoStartRequested,
	markSsoAutoStartSpent,
	useSsoAutoStart,
	withoutSsoStartRequest,
} from '@app/features/auth/flow/SsoAutoStart';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {type StepTitleChangeHandler, useReportStepTitle} from '@app/features/auth/flow/useReportStepTitle';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {
	PasskeyLoginCapability,
	resolveInitialPasskeyLoginCapability,
	resolvePasskeyLoginCapability,
	type StoredAccountLoginPayload,
	useAccountSwitchController,
	useLoginFormController,
} from '@app/features/auth/hooks/useLoginFlow';
import {isPasskeyAvailableForCurrentClient} from '@app/features/auth/PasskeyAvailability';
import {usePasskeyBridgeReturn} from '@app/features/auth/passkey_migration/usePasskeyBridgeReturn';
import {switcherAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import {
	type IpAuthorizationChallenge,
	type LoginSuccessPayload,
	startSsoLogin,
} from '@app/features/auth/state/AuthFlow';
import {AuthCardVariant, AuthLayoutContentMode, useAuthLayoutContext} from '@app/features/auth/state/AuthLayoutContext';
import type {AuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {accountLoginIdentifierField, accountSignInIdentifier} from '@app/features/auth/utils/AccountSignInIdentifier';
import {logoutStoredAccountFromSwitcher} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import {shouldOfferOldAppSignIn} from '@app/features/auth/utils/OldAppSignIn';
import {describePasskeyPromptEnded} from '@app/features/auth/utils/PasskeyErrorMessages';
import {
	BACK_DESCRIPTOR,
	COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import type {Account} from '@app/features/platform/state/AuthSession';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {isDesktop, navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
import type {GuildEntryResult} from '@fluxer/schema/src/domains/user/AccountSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactElement, type ReactNode, useCallback, useEffect, useMemo, useState} from 'react';

const SIGN_IN_FLOW_DESCRIPTOR = msg({
	message: 'Sign-in flow',
	comment: 'Accessible label for the stepped sign-in panel.',
});
const WELCOME_BACK_DESCRIPTOR = msg({
	message: 'Welcome back',
	comment: 'Heading on the standard sign-in form.',
});
const CHOOSE_INSTANCE_DESCRIPTOR = msg({
	message: 'Choose your instance',
	comment: 'Heading for the login step that lets a user choose the instance before signing in.',
});
const OLD_APP_SIGN_IN_HINT_DESCRIPTOR = msg({
	message: 'Approve this app from the {productName} app you already use. No password needed.',
	comment:
		'Hint under the sign-in option on fluxer.com that pairs a newly installed app with the old installed app. productName is the app name.',
});

export interface AuthLoginLayoutProps {
	readonly redirectPath: string | null;
	readonly inviteCode?: string | null;
	readonly desktopHandoff: boolean;
	readonly excludeCurrentUser?: boolean;
	readonly extraTopContent?: ReactNode;
	readonly forgotPasswordAction?: ReactNode;
	readonly showTitle?: boolean;
	readonly title?: ReactNode;
	readonly registerLink: ReactElement<Record<string, unknown>>;
	readonly onLoginComplete?: ((payload: LoginSuccessPayload) => Promise<void> | void) | null;
	readonly onBackActionChange?: ((action: (() => void) | null) => void) | null;
	readonly onStepTitleChange?: StepTitleChangeHandler | null;
	readonly completeLoginRedirectPath: string | null;
	readonly forceCredentials?: boolean;
	readonly startWithAddAccount?: boolean;
	readonly runtimeTarget: AuthRuntimeTarget;
	readonly ssoRedirectPath?: string | null;
	readonly suppressInlineBackButtons?: boolean;
	readonly initialIdentifier?: string | null;
	readonly allowSsoAutoStart?: boolean;
}

interface HandoffAccountsRequest {
	readonly accounts: Array<Account>;
	readonly currentAccountKey: string | null;
	readonly desktopHandoff: boolean;
	readonly excludeCurrentUser: boolean;
}

interface SsoRedirectPathRequest {
	readonly explicitSsoRedirectPath: string | null;
	readonly desktopHandoff: boolean;
	readonly handoffLocationPath: string;
	readonly redirectPath: string | null;
}

interface DesktopClientSetupRequest {
	readonly desktopHandoff: boolean;
	readonly forceCredentials: boolean;
	readonly hasStoredAccounts: boolean;
	readonly initialIdentifier: string | null;
	readonly initialInstanceSelected: boolean;
	readonly startWithAddAccount: boolean;
}

interface MethodSsoActionRequest {
	readonly ssoConfig: InstanceSsoConfig | null;
	readonly label: ReactNode;
	readonly isStarting: boolean;
	readonly onStart: () => void;
}

interface AuthBackActionRequest {
	readonly accountListBackAction: (() => void) | null;
	readonly authLoginStep: AuthLoginStep;
	readonly browserBackAction: (() => void) | null;
	readonly forceCredentials: boolean;
	readonly handleBackFromBrowser: () => void;
	readonly handleBackFromCredentials: () => void;
	readonly handleBackFromIpAuthorization: () => void;
	readonly handleChangeInstance: () => void;
	readonly instanceBackAction: (() => void) | null;
	readonly shouldShowInstanceSelector: boolean;
}

function getInitialInstanceUrl(snapshot: RuntimeConfigSnapshot | null): string | null {
	if (snapshot == null) {
		return null;
	}
	return describeAPIEndpoint(snapshot.apiEndpoint);
}

function usesIdentifierFirst(snapshot: RuntimeConfigSnapshot | null): boolean {
	return snapshot?.features.identifier_first === true;
}

function resolveEmailsEnabled(snapshot: RuntimeConfigSnapshot | null): boolean {
	return snapshot?.features.emails_enabled ?? false;
}

function requireAuthInstanceSnapshot(snapshot: RuntimeConfigSnapshot | null): RuntimeConfigSnapshot {
	if (snapshot == null) {
		throw new Error('Cannot render instance authentication methods without a selected runtime');
	}
	return snapshot;
}

function resolveHandoffAccounts({
	accounts,
	currentAccountKey,
	desktopHandoff,
	excludeCurrentUser,
}: HandoffAccountsRequest): Array<Account> {
	if (!desktopHandoff) {
		return accounts;
	}
	if (!excludeCurrentUser) {
		return accounts;
	}
	return accounts.filter((account) => account.storageKey !== currentAccountKey);
}

function resolveHandoffInitialMode(desktopHandoff: boolean, hasHandoffAccounts: boolean): DesktopHandoffMode {
	if (!desktopHandoff) {
		return DesktopHandoffMode.LOGIN;
	}
	if (!hasHandoffAccounts) {
		return DesktopHandoffMode.LOGIN;
	}
	return DesktopHandoffMode.SELECTING;
}

function resolveSsoRedirectPath({
	explicitSsoRedirectPath,
	desktopHandoff,
	handoffLocationPath,
	redirectPath,
}: SsoRedirectPathRequest): string | null {
	if (explicitSsoRedirectPath != null) {
		return explicitSsoRedirectPath;
	}
	if (desktopHandoff) {
		return handoffLocationPath;
	}
	return redirectPath;
}

function resolveShouldUseDesktopClientSetup({
	desktopHandoff,
	forceCredentials,
	hasStoredAccounts,
	initialIdentifier,
	initialInstanceSelected,
	startWithAddAccount,
}: DesktopClientSetupRequest): boolean {
	if (!isDesktop()) {
		return false;
	}
	if (desktopHandoff) {
		return false;
	}
	if (forceCredentials) {
		return false;
	}
	if (startWithAddAccount) {
		return false;
	}
	if (hasInitialIdentifier(initialIdentifier)) {
		return false;
	}
	if (initialInstanceSelected) {
		return false;
	}
	return !hasStoredAccounts;
}

function resolveAccountInstance(account: Account): RuntimeConfigSnapshot | null {
	if (account.instance == null) {
		return null;
	}
	return account.instance;
}

function resolveEffectiveHandoffMode(handoffMode: DesktopHandoffMode, hasHandoffAccounts: boolean): DesktopHandoffMode {
	if (handoffMode === DesktopHandoffMode.SELECTING && !hasHandoffAccounts) {
		return DesktopHandoffMode.LOGIN;
	}
	return handoffMode;
}

function isSsoAutoStartStep(step: AuthLoginStep, startRequested: boolean): boolean {
	if (step === AuthLoginStep.SSO || step === AuthLoginStep.METHOD || step === AuthLoginStep.CREDENTIALS) {
		return true;
	}
	return startRequested && step === AuthLoginStep.ACCOUNT;
}

function resolveAuthCardVariant(step: AuthLoginStep): AuthCardVariant {
	if (step === AuthLoginStep.DESKTOP_HANDOFF_APPROVAL) {
		return AuthCardVariant.COMPACT;
	}
	if (step === AuthLoginStep.IP_AUTHORIZATION) {
		return AuthCardVariant.COMPACT;
	}
	return AuthCardVariant.STANDARD;
}

function resolveStepError(showAccountSelector: boolean, switchError: string | null): string | null {
	if (showAccountSelector) {
		return null;
	}
	return switchError;
}

function resolveStepTitleDescriptor(step: AuthLoginStep): MessageDescriptor | null {
	switch (step) {
		case AuthLoginStep.INSTANCE:
			return CHOOSE_INSTANCE_DESCRIPTOR;
		case AuthLoginStep.BROWSER:
			return SIGN_IN_WITH_BROWSER_DESCRIPTOR;
		default:
			return null;
	}
}

function resolveAuthStepTitle(title: ReactNode, fallbackTitle: ReactNode): ReactNode {
	if (title == null) {
		return fallbackTitle;
	}
	return title;
}

function resolveReloginTitle(account: Account | null, format: (accountLabel: string) => string): string | null {
	if (account == null) {
		return null;
	}
	const labels = getAccountDisplayLabels(account);
	if (!labels.available || labels.displayLabel === '') {
		return null;
	}
	return format(labels.displayLabel);
}

function resolveBrowserPrefillIdentifier(prefillIdentifier: string | null, formEmail: string): string | null {
	if (prefillIdentifier != null) {
		return prefillIdentifier;
	}
	if (formEmail === '') {
		return null;
	}
	return formEmail;
}

function resolveForgotPasswordLink(
	emailsEnabled: boolean,
	forgotPasswordAction: ReactNode,
	defaultAction: ReactNode,
): ReactNode {
	if (!emailsEnabled) {
		return null;
	}
	if (forgotPasswordAction == null) {
		return defaultAction;
	}
	return forgotPasswordAction;
}

function resolveMethodSsoAction({
	ssoConfig,
	label,
	isStarting,
	onStart,
}: MethodSsoActionRequest): AuthLoginSsoAction | null {
	if (!isInstanceSsoAvailable(ssoConfig)) {
		return null;
	}
	return {isStarting, label, onStart};
}

function resolveAuthBackAction({
	accountListBackAction,
	authLoginStep,
	browserBackAction,
	forceCredentials,
	handleBackFromBrowser,
	handleBackFromCredentials,
	handleBackFromIpAuthorization,
	handleChangeInstance,
	instanceBackAction,
	shouldShowInstanceSelector,
}: AuthBackActionRequest): (() => void) | null {
	switch (authLoginStep) {
		case AuthLoginStep.BROWSER:
			if (browserBackAction != null) {
				return browserBackAction;
			}
			return handleBackFromBrowser;
		case AuthLoginStep.CREDENTIALS:
			return forceCredentials ? null : handleBackFromCredentials;
		case AuthLoginStep.IP_AUTHORIZATION:
			return handleBackFromIpAuthorization;
		case AuthLoginStep.METHOD:
			if (shouldShowInstanceSelector) {
				return handleChangeInstance;
			}
			return accountListBackAction;
		case AuthLoginStep.INSTANCE:
			if (instanceBackAction != null) {
				return instanceBackAction;
			}
			return accountListBackAction;
		default:
			return null;
	}
}

export const AuthLoginLayout = observer(function AuthLoginLayout({
	redirectPath,
	inviteCode = null,
	desktopHandoff,
	excludeCurrentUser = false,
	extraTopContent = null,
	forgotPasswordAction = null,
	showTitle = true,
	title = null,
	registerLink,
	onLoginComplete = null,
	onBackActionChange = null,
	onStepTitleChange = null,
	completeLoginRedirectPath,
	forceCredentials = false,
	startWithAddAccount = false,
	runtimeTarget,
	ssoRedirectPath: explicitSsoRedirectPath = null,
	suppressInlineBackButtons = false,
	initialIdentifier = null,
	allowSsoAutoStart = true,
}: AuthLoginLayoutProps) {
	const {i18n} = useLingui();
	const {setContentMode} = useAuthLayoutContext();
	const location = useLocation();
	const currentAccountKey = Accounts.currentAccountKey;
	const accounts = switcherAccounts();
	const hasStoredAccounts = accounts.length > 0;
	const shouldShowInstanceSelector = shouldRenderInstanceStep() && runtimeTarget.canChangeInstance;
	const [isStartingSso, setIsStartingSso] = useState(false);
	const [browserStepStartsSso, setBrowserStepStartsSso] = useState(false);
	const resolvedAuthRuntimeSnapshot = runtimeTarget.snapshot;
	const initialInstanceSelected = runtimeTarget.initialSnapshot != null;
	const ssoConfig = resolvedAuthRuntimeSnapshot == null ? null : resolveAuthPanelSso(resolvedAuthRuntimeSnapshot);
	const isSsoEnforced = isSsoLoginEnforced(ssoConfig);
	const emailsEnabled = resolveEmailsEnabled(resolvedAuthRuntimeSnapshot);
	const handoffAccounts = resolveHandoffAccounts({
		accounts,
		currentAccountKey,
		desktopHandoff,
		excludeCurrentUser,
	});
	const [handoffStartsSso] = useState(() => desktopHandoff && isSsoStartRequested(location.search));
	const hasHandoffAccounts = handoffAccounts.length > 0;
	const offersHandoffAccounts = hasHandoffAccounts && !handoffStartsSso;
	const handoff = useDesktopHandoffFlow({
		enabled: desktopHandoff,
		hasStoredAccounts: offersHandoffAccounts,
		initialMode: resolveHandoffInitialMode(desktopHandoff, offersHandoffAccounts),
	});
	const [ipAuthChallenge, setIpAuthChallenge] = useState<IpAuthorizationChallenge | null>(null);
	const flow = useAuthLoginFlow({
		startWithAddAccount,
		forceCredentials,
		desktopHandoff,
		hasStoredAccounts,
		initialIdentifier,
		initialInstanceSelected,
		shouldShowInstanceSelector,
	});
	const {
		showAccountSelector,
		hasCompletedInstanceStep,
		hasSelectedLoginMethod,
		showBrowserStep,
		methodBackTarget,
		prefillIdentifier,
		error: switchError,
		syncFromProps,
		showFormForAccount,
		addAnotherAccount,
		backToAccountList,
		changeInstance,
		continueFromInstance,
		selectEmailMethod,
		enterBrowserStep,
		backFromCredentials,
		backFromBrowser,
		setError: setSwitchError,
	} = flow;
	const [instanceBackAction, setInstanceBackAction] = useState<(() => void) | null>(null);
	const [browserBackAction, setBrowserBackAction] = useState<(() => void) | null>(null);
	const [childStepTitle, setChildStepTitle] = useState<string | null>(null);
	const ssoRedirectPath = resolveSsoRedirectPath({
		explicitSsoRedirectPath,
		desktopHandoff,
		handoffLocationPath: withoutSsoStartRequest(`${location.pathname}${location.search}`),
		redirectPath,
	});
	const shouldUseDesktopClientSetup = resolveShouldUseDesktopClientSetup({
		desktopHandoff,
		forceCredentials,
		hasStoredAccounts,
		initialIdentifier,
		initialInstanceSelected,
		startWithAddAccount,
	});
	const [passkeyLoginCapability, setPasskeyLoginCapability] = useState<PasskeyLoginCapability>(
		resolveInitialPasskeyLoginCapability,
	);
	const {
		showWelcome: showClientWelcome,
		showPreferences: shouldShowClientPreferencesStep,
		permission: clientPermission,
		permissionIndex: clientPermissionIndex,
		permissionCount: clientPermissionCount,
		continueFromWelcome: continueFromClientWelcome,
		completePreferences: completeClientPreferences,
		advancePermission: advanceClientPermission,
		skipPermission: skipClientPermission,
	} = useDesktopClientIntroFlow(shouldUseDesktopClientSetup);
	const shouldShowInstanceStep = shouldShowInstanceSelector && !hasCompletedInstanceStep;
	const handleLoginSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			if (desktopHandoff) {
				const runtimeSnapshot = requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot);
				await Accounts.refreshStoredAccount({
					userId: payload.userId,
					token: payload.token,
					userData: payload.userData,
					runtimeSnapshot,
				});
				await handoff.start({...payload, runtimeSnapshot});
				return;
			}
			await AuthenticationCommands.completeLogin(
				{...payload, runtimeSnapshot: requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot)},
				{redirectPath: completeLoginRedirectPath},
			);
			if (onLoginComplete != null) {
				await onLoginComplete(payload);
			}
		},
		[completeLoginRedirectPath, desktopHandoff, handoff, onLoginComplete, resolvedAuthRuntimeSnapshot],
	);
	const handleShowBrowserStep = useCallback(() => {
		setBrowserStepStartsSso(false);
		enterBrowserStep();
	}, [enterBrowserStep]);
	const handleShowSsoBrowserStep = useCallback(() => {
		setBrowserStepStartsSso(true);
		enterBrowserStep();
	}, [enterBrowserStep]);
	const {form, identifierField, isLoading, fieldErrors, handlePasskeyLogin, isPasskeyLoading, connectingMessage} =
		useLoginFormController({
			redirectPath: redirectPath ?? undefined,
			inviteCode: inviteCode ?? undefined,
			runtimeSnapshot: resolvedAuthRuntimeSnapshot,
			onDesktopPasskeyHandoff: handleShowBrowserStep,
			onLoginSuccess: handleLoginSuccess,
			onRequireMfa: (challenge) => {
				AuthenticationCommands.setMfaTicket({
					...challenge,
					runtimeSnapshot: requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot),
				});
			},
			onRequireIpAuthorization: (challenge) => {
				setIpAuthChallenge(challenge);
			},
		});
	const isPasskeyBridgeRedeeming = usePasskeyBridgeReturn({
		redirectPath: redirectPath ?? undefined,
		onLoginSuccess: handleLoginSuccess,
		onRequireMfa: (challenge) => {
			AuthenticationCommands.setMfaTicket({
				...challenge,
				runtimeSnapshot: requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot),
			});
		},
		onCancelled: () => {
			ToastCommands.error(describePasskeyPromptEnded(i18n));
		},
		onFailure: () => {
			setSwitchError(i18n._(COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR));
		},
	});
	const clearCredentials = useCallback(() => {
		form.setValue('email', '');
		form.setValue('login', '');
		form.setValue('password', '');
	}, [form.setValue]);
	const [reloginAccount, setReloginAccount] = useState<Account | null>(null);
	const showLoginFormForAccount = useCallback(
		(account: Account) => {
			const identifier = accountSignInIdentifier(account);
			const runtimeSnapshot = resolveAccountInstance(account);
			clearCredentials();
			form.setValue(accountLoginIdentifierField(account), identifier ?? '');
			if (runtimeSnapshot == null) {
				runtimeTarget.reset();
			} else {
				runtimeTarget.select(runtimeSnapshot);
			}
			setReloginAccount(account);
			showFormForAccount(identifier, null, runtimeSnapshot != null);
		},
		[clearCredentials, form.setValue, runtimeTarget, showFormForAccount],
	);
	const handleStoredAccountLogin = useCallback(
		async (payload: StoredAccountLoginPayload) => {
			await AuthenticationCommands.completeLogin(payload, {redirectPath: completeLoginRedirectPath});
			if (onLoginComplete != null) {
				await onLoginComplete(payload);
			}
			if (redirectPath != null && redirectPath !== '') {
				RouterUtils.replaceWith(redirectPath);
			}
		},
		[completeLoginRedirectPath, onLoginComplete, redirectPath],
	);
	const handleSignOutStoredAccount = useCallback(
		async (account: Account) => {
			setSwitchError(null);
			await logoutStoredAccountFromSwitcher(account, setSwitchError);
		},
		[setSwitchError],
	);
	const {isSwitching, switchToAccount} = useAccountSwitchController({
		redirectPath,
		onError: setSwitchError,
		onSessionExpired: showLoginFormForAccount,
		onLoginWithStoredAccount: handleStoredAccountLogin,
	});
	useEffect(() => {
		let cancelled = false;
		void resolvePasskeyLoginCapability()
			.catch(() => PasskeyLoginCapability.UNAVAILABLE)
			.then((capability) => {
				if (!cancelled) {
					setPasskeyLoginCapability(capability);
				}
			});
		return () => {
			cancelled = true;
		};
	}, []);
	const showPasskeyOption = passkeyLoginCapability !== PasskeyLoginCapability.UNAVAILABLE;
	const passkeyControlsDisabled =
		isLoading || Boolean(form.isSubmitting) || isPasskeyLoading || isPasskeyBridgeRedeeming;
	const handleContinueFromInstance = useCallback(
		(snapshot: RuntimeConfigSnapshot) => {
			runtimeTarget.select(snapshot);
			continueFromInstance();
		},
		[continueFromInstance, runtimeTarget],
	);
	const handleContinueFromClientWelcome = useCallback(() => {
		setSwitchError(null);
		setContentMode(AuthLayoutContentMode.CARD);
		continueFromClientWelcome();
	}, [continueFromClientWelcome, setContentMode, setSwitchError]);
	const handleContinueFromClientPreferences = useCallback(() => {
		setSwitchError(null);
		completeClientPreferences();
	}, [completeClientPreferences, setSwitchError]);
	const canReturnToAccountList =
		hasStoredAccounts && !desktopHandoff && !forceCredentials && !hasInitialIdentifier(initialIdentifier);
	const handleBackFromCredentials = useCallback(() => {
		clearCredentials();
		setReloginAccount(null);
		backFromCredentials(canReturnToAccountList);
	}, [backFromCredentials, canReturnToAccountList, clearCredentials]);
	const handleBackToAccountList = useCallback(() => {
		clearCredentials();
		setReloginAccount(null);
		backToAccountList();
	}, [backToAccountList, clearCredentials]);
	const handleBackFromBrowser = useCallback(() => {
		backFromBrowser();
	}, [backFromBrowser]);
	const handleBrowserLoginSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			await handleLoginSuccess(payload);
			if (redirectPath != null && redirectPath !== '') {
				RouterUtils.replaceWith(redirectPath);
			}
			backFromBrowser();
		},
		[backFromBrowser, handleLoginSuccess, redirectPath],
	);
	const offerOldAppSignIn = useMemo(
		() =>
			!desktopHandoff &&
			shouldOfferOldAppSignIn({
				origin: window.location.origin,
				installKind: detectDomainMigrationInstallKind(),
				hasStoredAccounts,
			}),
		[desktopHandoff, hasStoredAccounts],
	);
	const handleOldAppSignIn = useCallback(() => {
		showBrowserLoginHandoffModal(
			async (payload) => {
				await handleLoginSuccess(payload);
				if (redirectPath != null && redirectPath !== '') {
					RouterUtils.replaceWith(redirectPath);
				}
			},
			undefined,
			'old_app',
		);
	}, [handleLoginSuccess, redirectPath]);
	const handleBrowserBackActionChange = useCallback((action: (() => void) | null) => {
		setBrowserBackAction(() => action);
	}, []);
	const handleInstanceBackActionChange = useCallback((action: (() => void) | null) => {
		setInstanceBackAction(() => action);
	}, []);
	const handleBackFromIpAuthorization = useCallback(() => {
		setIpAuthChallenge(null);
	}, []);
	const handleChangeInstance = useCallback(() => {
		clearCredentials();
		runtimeTarget.reset();
		changeInstance();
	}, [changeInstance, clearCredentials, runtimeTarget]);
	const handleIpAuthorizationComplete = useCallback(
		async (payload: LoginSuccessPayload) => {
			await handleLoginSuccess(payload);
			if (redirectPath != null && redirectPath !== '') {
				RouterUtils.replaceWith(redirectPath);
			}
			setIpAuthChallenge(null);
		},
		[handleLoginSuccess, redirectPath],
	);
	useEffect(() => {
		syncFromProps({
			startWithAddAccount,
			forceCredentials,
			initialIdentifier,
			initialInstanceSelected,
			shouldShowInstanceSelector,
		});
	}, [
		forceCredentials,
		initialIdentifier,
		initialInstanceSelected,
		shouldShowInstanceSelector,
		startWithAddAccount,
		syncFromProps,
	]);
	useEffect(() => {
		if (prefillIdentifier != null) {
			form.setValue(identifierField, prefillIdentifier);
		}
	}, [form.setValue, identifierField, prefillIdentifier]);
	const handleAddAnotherAccount = useCallback(() => {
		clearCredentials();
		setReloginAccount(null);
		runtimeTarget.reset();
		addAnotherAccount();
	}, [addAnotherAccount, clearCredentials, runtimeTarget]);
	const handleHandoffReLogin = useCallback(
		(account: Account) => {
			showLoginFormForAccount(account);
			handoff.switchToLogin();
		},
		[handoff, showLoginFormForAccount],
	);
	const handleChooseHandoffAccount = useCallback(() => {
		clearCredentials();
		setReloginAccount(null);
		runtimeTarget.reset();
		addAnotherAccount();
		handoff.retry();
	}, [addAnotherAccount, clearCredentials, handoff, runtimeTarget]);
	const handleStartSso = useCallback(async () => {
		if (!isInstanceSsoAvailable(ssoConfig)) {
			return;
		}
		if (isDesktop()) {
			handleShowSsoBrowserStep();
			return;
		}
		try {
			setIsStartingSso(true);
			markSsoAutoStartSpent();
			const runtimeSnapshot = requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot);
			const {authorizationUrl} = await startSsoLogin({
				redirectTo: ssoRedirectPath,
				runtimeSnapshot,
			});
			await navigateToExternalURL(authorizationUrl);
		} catch (error) {
			setSwitchError(getAuthErrorMessage(error, i18n));
		} finally {
			setIsStartingSso(false);
		}
	}, [handleShowSsoBrowserStep, i18n, resolvedAuthRuntimeSnapshot, setSwitchError, ssoConfig, ssoRedirectPath]);
	const authLoginStep: AuthLoginStep = useMemo(() => {
		return selectAuthLoginStep({
			desktopHandoff,
			handoffMode: resolveEffectiveHandoffMode(handoff.mode, hasHandoffAccounts),
			hasIpAuthorizationChallenge: ipAuthChallenge != null,
			hasStoredAccounts,
			isSsoEnforced,
			shouldShowBrowserStep: showBrowserStep && resolveShouldOfferBrowserStep(),
			clientPermission,
			shouldShowClientPreferencesStep,
			shouldShowInstanceStep,
			shouldShowMethodStep: !hasSelectedLoginMethod,
			showAccountSelector,
		});
	}, [
		desktopHandoff,
		handoff.mode,
		hasHandoffAccounts,
		hasSelectedLoginMethod,
		hasStoredAccounts,
		ipAuthChallenge,
		isSsoEnforced,
		clientPermission,
		shouldShowClientPreferencesStep,
		shouldShowInstanceStep,
		showAccountSelector,
		showBrowserStep,
	]);
	const isOnSsoAutoStartStep = useCallback(
		(startRequested: boolean) => isSsoAutoStartStep(authLoginStep, startRequested),
		[authLoginStep],
	);
	useSsoAutoStart({
		allowed: allowSsoAutoStart,
		desktopApp: isDesktop(),
		desktopHandoff,
		onSignInStep: isOnSsoAutoStartStep,
		ssoAvailable: isInstanceSsoAvailable(ssoConfig),
		ssoEnforced: isSsoEnforced,
		pathname: location.pathname,
		search: location.search,
		start: handleStartSso,
	});
	const cardVariant = useMemo<AuthCardVariant>(() => resolveAuthCardVariant(authLoginStep), [authLoginStep]);
	const accountListBackAction = useMemo<(() => void) | null>(() => {
		if (desktopHandoff) {
			return hasHandoffAccounts ? handleChooseHandoffAccount : null;
		}
		if (methodBackTarget !== AuthLoginStep.ACCOUNT || !canReturnToAccountList) {
			return null;
		}
		return handleBackToAccountList;
	}, [
		canReturnToAccountList,
		desktopHandoff,
		handleBackToAccountList,
		handleChooseHandoffAccount,
		hasHandoffAccounts,
		methodBackTarget,
	]);
	const backAction = useMemo<(() => void) | null>(
		() =>
			resolveAuthBackAction({
				accountListBackAction,
				authLoginStep,
				browserBackAction,
				forceCredentials,
				handleBackFromBrowser,
				handleBackFromCredentials,
				handleBackFromIpAuthorization,
				handleChangeInstance,
				instanceBackAction,
				shouldShowInstanceSelector,
			}),
		[
			accountListBackAction,
			authLoginStep,
			browserBackAction,
			forceCredentials,
			handleBackFromBrowser,
			handleBackFromCredentials,
			handleBackFromIpAuthorization,
			handleChangeInstance,
			instanceBackAction,
			shouldShowInstanceSelector,
		],
	);
	useEffect(() => {
		if (onBackActionChange == null) {
			return;
		}
		const notifyBackActionChange = onBackActionChange;
		notifyBackActionChange(backAction);
		return () => notifyBackActionChange(null);
	}, [backAction, onBackActionChange]);
	const childStepTitleChange = onStepTitleChange == null ? null : setChildStepTitle;
	const stepTitleDescriptor = resolveStepTitleDescriptor(authLoginStep);
	let stepTitle: string | null = stepTitleDescriptor == null ? null : i18n._(stepTitleDescriptor);
	if (authLoginStep === AuthLoginStep.IP_AUTHORIZATION || authLoginStep === AuthLoginStep.SSO) {
		stepTitle = childStepTitle;
	}
	useReportStepTitle(onStepTitleChange, stepTitle);
	useAuthPresentation(
		showClientWelcome
			? {contentMode: AuthLayoutContentMode.FULL, variant: AuthCardVariant.STANDARD}
			: {contentMode: AuthLayoutContentMode.CARD, variant: cardVariant},
	);
	const formTitle =
		title ?? resolveReloginTitle(reloginAccount, (accountLabel) => i18n._(SIGN_IN_AGAIN_AS_DESCRIPTOR, {accountLabel}));
	const renderOldAppSignIn = (): ReactNode => {
		if (!offerOldAppSignIn) {
			return null;
		}
		return (
			<div className={styles.ssoBlock} data-flx="auth.flow.auth-login-layout.old-app-block">
				<Button
					fitContainer
					onClick={handleOldAppSignIn}
					type="button"
					data-flx="auth.flow.auth-login-layout.button.old-app-sign-in"
				>
					{i18n._(SIGN_IN_WITH_OLD_APP_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</Button>
				<div className={styles.ssoSubtitle} data-flx="auth.flow.auth-login-layout.old-app-subtitle">
					{i18n._(OLD_APP_SIGN_IN_HINT_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</div>
			</div>
		);
	};
	const renderMethodStep = (): ReactNode => {
		let inlineInstanceBackAction: (() => void) | null = null;
		if (!suppressInlineBackButtons && shouldShowInstanceSelector) {
			inlineInstanceBackAction = handleChangeInstance;
		}
		return (
			<AuthLoginMethodStep
				extraTopContent={extraTopContent}
				identifierField={identifierField}
				showTitle={showTitle}
				title={resolveAuthStepTitle(formTitle, i18n._(WELCOME_BACK_DESCRIPTOR))}
				switchError={resolveStepError(showAccountSelector, switchError)}
				leadingAction={renderOldAppSignIn()}
				disabled={passkeyControlsDisabled || isStartingSso}
				passkeyAvailableForInstance={isPasskeyAvailableForCurrentClient(resolvedAuthRuntimeSnapshot)}
				registerLink={registerLink}
				showBrowserOption={resolveShouldOfferBrowserStep() && resolvedAuthRuntimeSnapshot != null}
				showPasskeyOption={showPasskeyOption}
				onBrowserLogin={handleShowBrowserStep}
				onEmailLogin={selectEmailMethod}
				onPasskeyLogin={handlePasskeyLogin}
				onBackToInstance={inlineInstanceBackAction}
				ssoAction={resolveMethodSsoAction({
					ssoConfig,
					label: (
						<SsoButtonLabel
							ssoConfig={ssoConfig}
							data-flx="auth.flow.auth-login-layout.render-method-step.sso-button-label"
						/>
					),
					isStarting: isStartingSso,
					onStart: handleStartSso,
				})}
				data-flx="auth.flow.auth-login-layout.auth-login-method-step"
			/>
		);
	};
	const handleIdentifierFirstSignedIn = async (payload: LoginSuccessPayload, entry: GuildEntryResult | null) => {
		await handleLoginSuccess(payload);
		if (desktopHandoff) {
			return;
		}
		if (entry !== null) {
			RouterUtils.replaceWith(Routes.guildChannel(entry.guild_id));
			return;
		}
		if (redirectPath != null && redirectPath !== '') {
			RouterUtils.replaceWith(redirectPath);
		}
	};
	const renderIdentifierFirstAlternatives = (): ReactNode => {
		const actions: Array<ReactNode> = [];
		const oldAppSignIn = renderOldAppSignIn();
		if (oldAppSignIn !== null) {
			actions.push(oldAppSignIn);
		}
		if (isInstanceSsoAvailable(ssoConfig)) {
			actions.push(
				<Button
					key="sso"
					fitContainer
					variant={ButtonVariant.SECONDARY}
					submitting={isStartingSso}
					disabled={passkeyControlsDisabled}
					onClick={handleStartSso}
					data-flx="auth.flow.auth-login-layout.identifier-first.button.sso"
				>
					<SsoButtonLabel
						ssoConfig={ssoConfig}
						data-flx="auth.flow.auth-login-layout.render-identifier-first-alternatives.sso-button-label"
					/>
				</Button>,
			);
		}
		if (resolveShouldOfferBrowserStep() && resolvedAuthRuntimeSnapshot != null) {
			actions.push(
				<Button
					key="browser"
					fitContainer
					variant={ButtonVariant.SECONDARY}
					disabled={passkeyControlsDisabled}
					onClick={handleShowBrowserStep}
					data-flx="auth.flow.auth-login-layout.identifier-first.button.browser"
				>
					{i18n._(SIGN_IN_WITH_BROWSER_DESCRIPTOR)}
				</Button>,
			);
		}
		return actions.length === 0 ? null : actions;
	};
	const renderIdentifierFirstStep = (): ReactNode => (
		<AuthIdentifierFirstStep
			runtimeSnapshot={requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot)}
			title={formTitle}
			showTitle={showTitle}
			extraTopContent={extraTopContent}
			notice={resolveStepError(showAccountSelector, switchError)}
			initialEmail={prefillIdentifier ?? initialIdentifier}
			inviteCode={inviteCode}
			passkeyAvailable={showPasskeyOption && isPasskeyAvailableForCurrentClient(resolvedAuthRuntimeSnapshot)}
			passkeyDisabled={passkeyControlsDisabled}
			onPasskeyLogin={handlePasskeyLogin}
			alternativeActions={renderIdentifierFirstAlternatives()}
			onSignedIn={handleIdentifierFirstSignedIn}
			onRequireMfa={(challenge) => {
				AuthenticationCommands.setMfaTicket({
					...challenge,
					runtimeSnapshot: requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot),
				});
			}}
			onRequireIpAuthorization={setIpAuthChallenge}
			data-flx="auth.flow.auth-login-layout.identifier-first-step"
		/>
	);
	const renderCredentialFooterStart = (): ReactNode => {
		if (suppressInlineBackButtons) {
			return null;
		}
		return (
			<Button
				type="button"
				variant={ButtonVariant.SECONDARY}
				onClick={handleBackFromCredentials}
				disabled={isLoading}
				data-flx="auth.flow.auth-login-layout.button.back-from-credentials"
			>
				{i18n._(BACK_DESCRIPTOR)}
			</Button>
		);
	};
	const renderCredentialStep = (): ReactNode => {
		const forgotPasswordLink =
			identifierField === 'login'
				? (forgotPasswordAction ?? (
						<AuthRouterLink
							to={Routes.RECOVER_ACCOUNT}
							className={styles.link}
							data-flx="auth.flow.auth-login-layout.link.recover"
						>
							{i18n._(FORGOT_YOUR_PASSWORD_DESCRIPTOR)}
						</AuthRouterLink>
					))
				: resolveForgotPasswordLink(
						emailsEnabled,
						forgotPasswordAction,
						<AuthRouterLink to="/forgot" className={styles.link} data-flx="auth.flow.auth-login-layout.link">
							{i18n._(FORGOT_YOUR_PASSWORD_DESCRIPTOR)}
						</AuthRouterLink>,
					);
		return (
			<AuthLoginCredentialStepPanel
				extraTopContent={extraTopContent}
				showTitle={showTitle}
				title={resolveAuthStepTitle(formTitle, i18n._(WELCOME_BACK_DESCRIPTOR))}
				switchError={resolveStepError(showAccountSelector, switchError)}
				form={form}
				isLoading={isLoading}
				fieldErrors={fieldErrors}
				submitLabel={i18n._(SIGN_IN_DESCRIPTOR)}
				forgotPasswordLink={forgotPasswordLink}
				identifierField={identifierField}
				disableSubmit={isPasskeyLoading || isPasskeyBridgeRedeeming}
				footerStart={renderCredentialFooterStart()}
				statusMessage={connectingMessage}
				data-flx="auth.flow.auth-login-layout.auth-login-credential-step-panel"
			/>
		);
	};
	const renderAuthLoginStep = (): ReactNode => {
		if (authLoginStep === AuthLoginStep.DESKTOP_HANDOFF_ACCOUNT) {
			return (
				<DesktopHandoffAccountSelector
					excludeCurrentUser={excludeCurrentUser}
					onSelectNewAccount={handoff.switchToLogin}
					onReLoginAccount={handleHandoffReLogin}
					onAccountSelected={handoff.start}
					data-flx="auth.flow.auth-login-layout.desktop-handoff-account-selector"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.SSO) {
			const runtimeSnapshot = requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot);
			return (
				<AuthSsoPanel
					redirectPath={ssoRedirectPath ?? undefined}
					runtimeSnapshot={runtimeSnapshot}
					onStart={isDesktop() ? handleShowSsoBrowserStep : undefined}
					onStepTitleChange={childStepTitleChange}
					dataFlx="auth.flow.auth-login-layout.sso-panel"
					data-flx="auth.flow.auth-login-layout.render-auth-login-step.auth-sso-panel"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.ACCOUNT) {
			return (
				<AccountSelector
					accounts={accounts}
					currentAccountKey={currentAccountKey}
					error={switchError}
					disabled={isSwitching}
					onSelectAccount={switchToAccount}
					onSignOutAccount={handleSignOutStoredAccount}
					onAddAccount={handleAddAnotherAccount}
					data-flx="auth.flow.auth-login-layout.account-selector"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.DESKTOP_HANDOFF_APPROVAL) {
			return <HandoffApprovalFlow handoff={handoff} data-flx="auth.flow.auth-login-layout.handoff-approval-flow" />;
		}
		if (authLoginStep === AuthLoginStep.IP_AUTHORIZATION && ipAuthChallenge != null) {
			const runtimeSnapshot = requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot);
			return (
				<IpAuthorizationScreen
					challenge={ipAuthChallenge}
					runtimeSnapshot={runtimeSnapshot}
					onAuthorized={handleIpAuthorizationComplete}
					onBack={suppressInlineBackButtons ? undefined : handleBackFromIpAuthorization}
					onStepTitleChange={childStepTitleChange}
					data-flx="auth.flow.auth-login-layout.ip-authorization-screen"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.INSTANCE) {
			return (
				<AuthLoginInstanceStep
					extraTopContent={extraTopContent}
					showTitle={onStepTitleChange == null}
					title={i18n._(CHOOSE_INSTANCE_DESCRIPTOR)}
					initialInstanceUrl={getInitialInstanceUrl(resolvedAuthRuntimeSnapshot)}
					onContinue={handleContinueFromInstance}
					onBackActionChange={handleInstanceBackActionChange}
					suppressInlineBackButton={suppressInlineBackButtons}
					data-flx="auth.flow.auth-login-layout.auth-login-instance-step"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.CLIENT_PREFERENCES) {
			return (
				<AuthClientPreferencesStep
					onContinue={handleContinueFromClientPreferences}
					data-flx="auth.flow.auth-login-layout.auth-client-preferences-step"
				/>
			);
		}
		if (clientPermission != null) {
			return (
				<AuthClientPermissionStep
					kind={clientPermission}
					index={clientPermissionIndex}
					count={clientPermissionCount}
					onAllowed={advanceClientPermission}
					onNotNow={skipClientPermission}
					data-flx="auth.flow.auth-login-layout.auth-client-permission-step"
				/>
			);
		}
		if (
			usesIdentifierFirst(resolvedAuthRuntimeSnapshot) &&
			(authLoginStep === AuthLoginStep.METHOD || authLoginStep === AuthLoginStep.CREDENTIALS)
		) {
			return renderIdentifierFirstStep();
		}
		if (authLoginStep === AuthLoginStep.METHOD) {
			return (
				<AgeBlockedRemovalGate data-flx="auth.flow.auth-login-layout.age-blocked-removal-gate">
					{renderMethodStep()}
				</AgeBlockedRemovalGate>
			);
		}
		if (authLoginStep === AuthLoginStep.BROWSER) {
			const runtimeSnapshot = requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot);
			let instanceSelectorChangeAction: (() => void) | null = null;
			if (shouldShowInstanceSelector) {
				instanceSelectorChangeAction = handleChangeInstance;
			}
			return (
				<AuthLoginBrowserStep
					extraTopContent={extraTopContent}
					prefillIdentifier={resolveBrowserPrefillIdentifier(prefillIdentifier, form.getValue(identifierField))}
					showTitle={showTitle}
					title={i18n._(SIGN_IN_WITH_BROWSER_DESCRIPTOR)}
					runtimeSnapshot={runtimeSnapshot}
					onBack={handleBackFromBrowser}
					onBackActionChange={handleBrowserBackActionChange}
					onChangeInstance={instanceSelectorChangeAction}
					onSuccess={handleBrowserLoginSuccess}
					showBackButton={!suppressInlineBackButtons}
					startSso={browserStepStartsSso}
					data-flx="auth.flow.auth-login-layout.auth-login-browser-step"
				/>
			);
		}
		return renderCredentialStep();
	};
	if (showClientWelcome) {
		return (
			<AuthClientWelcomeStep
				onContinue={handleContinueFromClientWelcome}
				data-flx="auth.flow.auth-login-layout.auth-client-welcome-step"
			/>
		);
	}
	return (
		<SteppedCarousel
			step={authLoginStep}
			steps={AUTH_LOGIN_STEP_ORDER}
			focusOnStepChange
			ariaLabel={i18n._(SIGN_IN_FLOW_DESCRIPTOR)}
			data-flx="auth.flow.auth-login-layout.carousel"
		>
			{renderAuthLoginStep()}
		</SteppedCarousel>
	);
});
