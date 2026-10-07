// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {detectDomainMigrationInstallKind} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {describeAPIEndpoint, type InstanceSsoConfig} from '@app/features/app/state/RuntimeConfig';
import {
	FORGOT_YOUR_PASSWORD_DESCRIPTOR,
	SIGN_IN_WITH_BROWSER_DESCRIPTOR,
} from '@app/features/auth/AuthMessageDescriptors';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AccountSelector} from '@app/features/auth/components/accounts/AccountSelector';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthSsoPanel, CONTINUE_WITH_SSO_DESCRIPTOR, resolveAuthPanelSso} from '@app/features/auth/flow/AuthSsoPanel';
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
import {useAuthLoginFlow} from '@app/features/auth/flow/auth_login_core/useAuthLoginFlow';
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
import {ConnectedHandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import IpAuthorizationScreen from '@app/features/auth/flow/IpAuthorizationScreen';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {
	PasskeyLoginCapability,
	resolveInitialPasskeyLoginCapability,
	resolvePasskeyLoginCapability,
	type StoredAccountLoginPayload,
	sessionExpiredMessage,
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
import {shouldOfferOldAppSignIn} from '@app/features/auth/utils/OldAppSignIn';
import {
	BACK_DESCRIPTOR,
	COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import type {Account} from '@app/features/platform/state/AuthSession';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {isDesktop, navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
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
	readonly completeLoginRedirectPath?: string | null;
	readonly forceCredentials?: boolean;
	readonly startWithAddAccount?: boolean;
	readonly runtimeTarget: AuthRuntimeTarget;
	readonly showInstanceSelector?: boolean | null;
	readonly ssoRedirectPath?: string | null;
	readonly suppressInlineBackButtons?: boolean;
	readonly initialIdentifier?: string | null;
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

function isInstanceSsoAvailable(ssoConfig: InstanceSsoConfig | null): boolean {
	return ssoConfig?.enabled === true;
}

function isSsoLoginEnforced(ssoConfig: InstanceSsoConfig | null): boolean {
	if (!isInstanceSsoAvailable(ssoConfig)) {
		return false;
	}
	return ssoConfig?.enforced === true;
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

function hasInitialIdentifier(initialIdentifier: string | null): boolean {
	if (initialIdentifier == null) {
		return false;
	}
	return initialIdentifier !== '';
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

function resolveAuthCardVariant(step: AuthLoginStep): AuthCardVariant {
	if (step === AuthLoginStep.DESKTOP_HANDOFF_APPROVAL) {
		return AuthCardVariant.COMPACT;
	}
	if (step === AuthLoginStep.IP_AUTHORIZATION) {
		return AuthCardVariant.COMPACT;
	}
	return AuthCardVariant.STANDARD;
}

function shouldUseFullscreenAuthPresentation(showClientWelcome: boolean, step: AuthLoginStep): boolean {
	if (showClientWelcome) {
		return true;
	}
	if (step === AuthLoginStep.ACCOUNT) {
		return true;
	}
	return step === AuthLoginStep.DESKTOP_HANDOFF_ACCOUNT;
}

function resolveStepError(showAccountSelector: boolean, switchError: string | null): string | null {
	if (showAccountSelector) {
		return null;
	}
	return switchError;
}

function resolveAuthStepTitle(title: ReactNode, fallbackTitle: ReactNode): ReactNode {
	if (title == null) {
		return fallbackTitle;
	}
	return title;
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

function resolveCompleteLoginOptions(
	completeLoginRedirectPath: string | null,
): AuthenticationCommands.CompleteLoginOptions {
	if (completeLoginRedirectPath == null) {
		return {};
	}
	return {redirectPath: completeLoginRedirectPath};
}

function resolveAuthBackAction({
	accountListBackAction,
	authLoginStep,
	browserBackAction,
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
			return handleBackFromCredentials;
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
	completeLoginRedirectPath = null,
	forceCredentials = false,
	startWithAddAccount = false,
	runtimeTarget,
	showInstanceSelector = null,
	ssoRedirectPath: explicitSsoRedirectPath = null,
	suppressInlineBackButtons = false,
	initialIdentifier = null,
}: AuthLoginLayoutProps) {
	const {i18n} = useLingui();
	const {setContentMode} = useAuthLayoutContext();
	const location = useLocation();
	const currentAccountKey = Accounts.currentAccountKey;
	const accounts = switcherAccounts();
	const hasStoredAccounts = accounts.length > 0;
	const shouldShowInstanceSelector = shouldRenderInstanceStep(showInstanceSelector);
	const [isStartingSso, setIsStartingSso] = useState(false);
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
	const hasHandoffAccounts = handoffAccounts.length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: desktopHandoff,
		hasStoredAccounts: hasHandoffAccounts,
		initialMode: resolveHandoffInitialMode(desktopHandoff, hasHandoffAccounts),
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
	const ssoRedirectPath = resolveSsoRedirectPath({
		explicitSsoRedirectPath,
		desktopHandoff,
		handoffLocationPath: `${location.pathname}${location.search}`,
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
				resolveCompleteLoginOptions(completeLoginRedirectPath),
			);
			if (onLoginComplete != null) {
				await onLoginComplete(payload);
			}
		},
		[completeLoginRedirectPath, desktopHandoff, handoff, onLoginComplete, resolvedAuthRuntimeSnapshot],
	);
	const handleShowBrowserStep = useCallback(() => {
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
		onFailure: () => {
			setSwitchError(i18n._(COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR));
		},
	});
	const clearCredentials = useCallback(() => {
		form.setValue('email', '');
		form.setValue('login', '');
		form.setValue('password', '');
	}, [form.setValue]);
	const showLoginFormForAccount = useCallback(
		(account: Account, message: string | null) => {
			const identifier = accountSignInIdentifier(account);
			const runtimeSnapshot = resolveAccountInstance(account);
			clearCredentials();
			form.setValue(accountLoginIdentifierField(account), identifier ?? '');
			if (runtimeSnapshot == null) {
				runtimeTarget.reset();
			} else {
				runtimeTarget.select(runtimeSnapshot);
			}
			showFormForAccount(identifier, message, runtimeSnapshot != null);
		},
		[clearCredentials, form.setValue, runtimeTarget, showFormForAccount],
	);
	const handleStoredAccountLogin = useCallback(
		async (payload: StoredAccountLoginPayload) => {
			await AuthenticationCommands.completeLogin(payload, resolveCompleteLoginOptions(completeLoginRedirectPath));
			if (onLoginComplete != null) {
				await onLoginComplete(payload);
			}
			if (redirectPath != null && redirectPath !== '') {
				RouterUtils.replaceWith(redirectPath);
			}
		},
		[completeLoginRedirectPath, onLoginComplete, redirectPath],
	);
	const {isSwitching, switchToAccount} = useAccountSwitchController({
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
		backFromCredentials(canReturnToAccountList);
	}, [backFromCredentials, canReturnToAccountList, clearCredentials]);
	const handleBackToAccountList = useCallback(() => {
		clearCredentials();
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
		runtimeTarget.reset();
		addAnotherAccount();
	}, [addAnotherAccount, clearCredentials, runtimeTarget]);
	const handleHandoffReLogin = useCallback(
		(account: Account) => {
			showLoginFormForAccount(account, sessionExpiredMessage(i18n, account));
			handoff.switchToLogin();
		},
		[handoff, i18n, showLoginFormForAccount],
	);
	const handleChooseHandoffAccount = useCallback(() => {
		clearCredentials();
		runtimeTarget.reset();
		addAnotherAccount();
		handoff.retry();
	}, [addAnotherAccount, clearCredentials, handoff, runtimeTarget]);
	const handleStartSso = useCallback(async () => {
		if (!isInstanceSsoAvailable(ssoConfig)) {
			return;
		}
		if (isDesktop()) {
			enterBrowserStep();
			return;
		}
		try {
			setIsStartingSso(true);
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
	}, [enterBrowserStep, i18n, resolvedAuthRuntimeSnapshot, setSwitchError, ssoConfig, ssoRedirectPath]);
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
	const shouldUseFullscreenPresentation = shouldUseFullscreenAuthPresentation(showClientWelcome, authLoginStep);
	useAuthPresentation(
		shouldUseFullscreenPresentation
			? {contentMode: AuthLayoutContentMode.FULL, variant: AuthCardVariant.STANDARD}
			: {contentMode: AuthLayoutContentMode.CARD, variant: cardVariant},
	);
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
					{i18n._(SIGN_IN_WITH_OLD_APP_DESCRIPTOR, {productName: PRODUCT_NAME})}
				</Button>
				<div className={styles.ssoSubtitle} data-flx="auth.flow.auth-login-layout.old-app-subtitle">
					{i18n._(OLD_APP_SIGN_IN_HINT_DESCRIPTOR, {productName: PRODUCT_NAME})}
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
				title={resolveAuthStepTitle(title, i18n._(WELCOME_BACK_DESCRIPTOR))}
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
					label: i18n._(CONTINUE_WITH_SSO_DESCRIPTOR),
					isStarting: isStartingSso,
					onStart: handleStartSso,
				})}
				data-flx="auth.flow.auth-login-layout.auth-login-method-step"
			/>
		);
	};
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
			identifierField === 'login' ? (
				<AuthRouterLink
					to={Routes.RECOVER_ACCOUNT}
					className={styles.link}
					data-flx="auth.flow.auth-login-layout.link.recover"
				>
					{i18n._(FORGOT_YOUR_PASSWORD_DESCRIPTOR)}
				</AuthRouterLink>
			) : (
				resolveForgotPasswordLink(
					emailsEnabled,
					forgotPasswordAction,
					<AuthRouterLink to="/forgot" className={styles.link} data-flx="auth.flow.auth-login-layout.link">
						{i18n._(FORGOT_YOUR_PASSWORD_DESCRIPTOR)}
					</AuthRouterLink>,
				)
			);
		return (
			<AuthLoginCredentialStepPanel
				extraTopContent={extraTopContent}
				showTitle={showTitle}
				title={resolveAuthStepTitle(title, i18n._(WELCOME_BACK_DESCRIPTOR))}
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
					onStart={isDesktop() ? handleShowBrowserStep : undefined}
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
					onAddAccount={handleAddAnotherAccount}
					data-flx="auth.flow.auth-login-layout.account-selector"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.DESKTOP_HANDOFF_APPROVAL) {
			return (
				<ConnectedHandoffApprovalFlow
					handoff={handoff}
					data-flx="auth.flow.auth-login-layout.connected-handoff-approval-flow"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.IP_AUTHORIZATION && ipAuthChallenge != null) {
			const runtimeSnapshot = requireAuthInstanceSnapshot(resolvedAuthRuntimeSnapshot);
			return (
				<IpAuthorizationScreen
					challenge={ipAuthChallenge}
					runtimeSnapshot={runtimeSnapshot}
					onAuthorized={handleIpAuthorizationComplete}
					onBack={suppressInlineBackButtons ? undefined : handleBackFromIpAuthorization}
					data-flx="auth.flow.auth-login-layout.ip-authorization-screen"
				/>
			);
		}
		if (authLoginStep === AuthLoginStep.INSTANCE) {
			return (
				<AuthLoginInstanceStep
					extraTopContent={extraTopContent}
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
		if (authLoginStep === AuthLoginStep.METHOD) {
			return renderMethodStep();
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
