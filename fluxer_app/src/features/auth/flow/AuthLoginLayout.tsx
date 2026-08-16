// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AccountSelector} from '@app/features/auth/components/accounts/AccountSelector';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	AuthSsoPanel,
	CONTINUE_WITH_SSO_DESCRIPTOR,
	FAILED_TO_START_SSO_DESCRIPTOR,
	PREFER_SSO_DESCRIPTOR,
	SSO_REQUIRED_DESCRIPTOR,
} from '@app/features/auth/flow/AuthSsoPanel';
import AuthLoginEmailPasswordForm from '@app/features/auth/flow/auth_login_core/AuthLoginEmailPasswordForm';
import AuthLoginPasskeyActions, {
	AuthLoginDivider,
} from '@app/features/auth/flow/auth_login_core/AuthLoginPasskeyActions';
import {isApprovalFlowMode, useDesktopHandoffFlow} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import DesktopHandoffAccountSelector from '@app/features/auth/flow/DesktopHandoffAccountSelector';
import {ConnectedHandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import IpAuthorizationScreen from '@app/features/auth/flow/IpAuthorizationScreen';
import {useAuthCardPresentation} from '@app/features/auth/flow/useAuthCardPresentation';
import {useLoginFormController} from '@app/features/auth/hooks/useLoginFlow';
import AccountManager from '@app/features/auth/state/AccountManager';
import {
	type IpAuthorizationChallenge,
	type LoginSuccessPayload,
	startSsoLogin,
} from '@app/features/auth/state/AuthFlow';
import {NEED_ACCOUNT_DESCRIPTOR, SIGN_IN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import {IS_DEV} from '@app/features/platform/types/Env';
import {Button} from '@app/features/ui/button/Button';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import {cloneElement, type ReactElement, type ReactNode, useCallback, useEffect, useMemo, useState} from 'react';

const SESSION_EXPIRED_SIGN_IN_AGAIN_DESCRIPTOR = msg({
	message: 'Session expired for {identifier}. Sign in again.',
	comment:
		'Login layout banner shown when the session for a specific account has expired. Account identifier is interpolated.',
});
const FAILED_TO_SWITCH_ACCOUNT_DESCRIPTOR = msg({
	message: 'Failed to switch account',
	comment: 'Login flow error shown when switching to a saved account fails.',
});
const SIGN_IN_FLOW_DESCRIPTOR = msg({
	message: 'Sign-in flow',
	comment: 'Accessible label for the stepped sign-in panel.',
});
const WELCOME_BACK_DESCRIPTOR = msg({
	message: 'Welcome back',
	comment: 'Heading on the standard sign-in form.',
});
const FORGOT_PASSWORD_DESCRIPTOR = msg({
	message: 'Forgot your password?',
	comment: 'Authentication link label that opens password recovery.',
});
const SIGN_IN_VIA_BROWSER_DESCRIPTOR = msg({
	message: 'Sign in via browser',
	comment: 'Passkey sign-in action that opens the browser flow from the desktop app.',
});

type AuthLoginStep =
	| 'account'
	| 'credentials'
	| 'desktop_handoff_account'
	| 'desktop_handoff_approval'
	| 'ip_authorization'
	| 'sso';

const AUTH_LOGIN_STEP_ORDER: ReadonlyArray<AuthLoginStep> = [
	'account',
	'desktop_handoff_account',
	'sso',
	'credentials',
	'ip_authorization',
	'desktop_handoff_approval',
];

interface AuthLoginLayoutProps {
	redirectPath?: string;
	inviteCode?: string;
	desktopHandoff?: boolean;
	excludeCurrentUser?: boolean;
	extraTopContent?: ReactNode;
	showTitle?: boolean;
	title?: ReactNode;
	registerLink: ReactElement<Record<string, unknown>>;
	onLoginComplete?: (payload: LoginSuccessPayload) => Promise<void> | void;
	initialEmail?: string;
}

export const AuthLoginLayout = observer(function AuthLoginLayout({
	redirectPath,
	inviteCode,
	desktopHandoff = false,
	excludeCurrentUser = false,
	extraTopContent,
	showTitle = true,
	title,
	registerLink,
	onLoginComplete,
	initialEmail,
}: AuthLoginLayoutProps) {
	const {i18n} = useLingui();
	const location = useLocation();
	const currentUserId = AccountManager.currentUserId;
	const accounts = AccountManager.orderedAccounts;
	const hasStoredAccounts = accounts.length > 0;
	const ssoConfig = RuntimeConfig.sso;
	const isSsoEnforced = Boolean(ssoConfig?.enabled && ssoConfig.enforced);
	const ssoDisplayName = ssoConfig?.display_name ?? 'Single Sign-On';
	const [isStartingSso, setIsStartingSso] = useState(false);
	const handoffAccounts =
		desktopHandoff && excludeCurrentUser ? accounts.filter((a) => a.userId !== currentUserId) : accounts;
	const hasHandoffAccounts = handoffAccounts.length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: desktopHandoff,
		hasStoredAccounts: hasHandoffAccounts,
		initialMode: desktopHandoff && hasHandoffAccounts ? 'selecting' : 'login',
	});
	const [ipAuthChallenge, setIpAuthChallenge] = useState<IpAuthorizationChallenge | null>(null);
	const [showAccountSelector, setShowAccountSelector] = useState(!desktopHandoff && hasStoredAccounts && !initialEmail);
	const [isSwitching, setIsSwitching] = useState(false);
	const [switchError, setSwitchError] = useState<string | null>(null);
	const [prefillEmail, setPrefillEmail] = useState<string | null>(() => initialEmail ?? null);
	const ssoRedirectPath = desktopHandoff ? `${location.pathname}${location.search}` : redirectPath;
	const showLoginFormForAccount = useCallback((account: Account, message?: string | null) => {
		setShowAccountSelector(false);
		setSwitchError(message ?? null);
		setPrefillEmail(account.userData?.email ?? null);
	}, []);
	const handleLoginSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			if (desktopHandoff) {
				await handoff.start(payload);
				return;
			}
			await AuthenticationCommands.completeLogin(payload);
			await onLoginComplete?.(payload);
		},
		[desktopHandoff, handoff, onLoginComplete],
	);
	const {form, isLoading, fieldErrors, handlePasskeyLogin, handlePasskeyBrowserLogin, isPasskeyLoading} =
		useLoginFormController({
			redirectPath,
			inviteCode,
			onLoginSuccess: handleLoginSuccess,
			onRequireMfa: (challenge) => {
				AuthenticationCommands.setMfaTicket(challenge);
			},
			onRequireIpAuthorization: (challenge) => {
				setIpAuthChallenge(challenge);
			},
		});
	const showBrowserPasskey = IS_DEV || isDesktop();
	const passkeyControlsDisabled = isLoading || Boolean(form.isSubmitting) || isPasskeyLoading;
	const handleIpAuthorizationComplete = useCallback(
		async (payload: LoginSuccessPayload) => {
			await handleLoginSuccess(payload);
			if (redirectPath) {
				RouterUtils.replaceWith(redirectPath);
			}
			setIpAuthChallenge(null);
		},
		[handleLoginSuccess, redirectPath],
	);
	useEffect(() => {
		setPrefillEmail(initialEmail ?? null);
		if (initialEmail) {
			setShowAccountSelector(false);
		}
	}, [initialEmail]);
	useEffect(() => {
		if (prefillEmail !== null) {
			form.setValue('email', prefillEmail);
		}
	}, [form.setValue, prefillEmail]);
	const handleSelectExistingAccount = useCallback(
		async (account: Account) => {
			const identifier = account.userData?.email ?? account.userData?.username ?? account.userId;
			const expiredMessage = i18n._(SESSION_EXPIRED_SIGN_IN_AGAIN_DESCRIPTOR, {identifier});
			if (account.isValid === false || !AccountManager.canSwitchAccounts) {
				showLoginFormForAccount(account, expiredMessage);
				return;
			}
			setIsSwitching(true);
			setSwitchError(null);
			try {
				await AccountManager.switchToAccount(account.userId);
			} catch (error) {
				const updatedAccount = AccountManager.accounts.get(account.userId);
				if (error instanceof SessionExpiredError || updatedAccount?.isValid === false) {
					showLoginFormForAccount(updatedAccount ?? account, expiredMessage);
					return;
				}
				setSwitchError(
					error && typeof error === 'object' && 'body' in error
						? FormUtils.extractErrorMessage(i18n, error)
						: i18n._(FAILED_TO_SWITCH_ACCOUNT_DESCRIPTOR),
				);
			} finally {
				setIsSwitching(false);
			}
		},
		[i18n, showLoginFormForAccount],
	);
	const handleAddAnotherAccount = useCallback(() => {
		setShowAccountSelector(false);
		setSwitchError(null);
		setPrefillEmail(null);
	}, []);
	const handleStartSso = useCallback(async () => {
		if (!ssoConfig?.enabled) return;
		try {
			setIsStartingSso(true);
			const {authorizationUrl} = await startSsoLogin({
				redirectTo: ssoRedirectPath,
			});
			window.location.assign(authorizationUrl);
		} catch (error) {
			setSwitchError(
				error && typeof error === 'object' && 'body' in error
					? FormUtils.extractErrorMessage(i18n, error)
					: i18n._(FAILED_TO_START_SSO_DESCRIPTOR),
			);
		} finally {
			setIsStartingSso(false);
		}
	}, [ssoConfig?.enabled, ssoRedirectPath, i18n]);
	const styledRegisterLink = useMemo(() => {
		const {className: linkClassName} = registerLink.props as {className?: string};
		return cloneElement(registerLink, {
			className: clsx(styles.footerLink, linkClassName),
		});
	}, [registerLink]);
	const authLoginStep: AuthLoginStep = useMemo(() => {
		if (desktopHandoff && handoff.mode === 'selecting') return 'desktop_handoff_account';
		if (desktopHandoff && isApprovalFlowMode(handoff.mode)) return 'desktop_handoff_approval';
		if (isSsoEnforced) return 'sso';
		if (showAccountSelector && hasStoredAccounts && !desktopHandoff) return 'account';
		if (ipAuthChallenge) return 'ip_authorization';
		return 'credentials';
	}, [desktopHandoff, handoff.mode, hasStoredAccounts, ipAuthChallenge, isSsoEnforced, showAccountSelector]);
	const hasExtraTopContent = extraTopContent !== undefined && extraTopContent !== null;
	const startedFromAccountSelector = hasStoredAccounts && !desktopHandoff && !initialEmail;
	const showSplitLogo =
		authLoginStep === 'credentials' && !desktopHandoff && !hasExtraTopContent && !startedFromAccountSelector;
	const cardVariant = useMemo(() => {
		if (showSplitLogo) return 'default';
		if (authLoginStep === 'account' || authLoginStep === 'desktop_handoff_account') return 'standard';
		if (authLoginStep === 'desktop_handoff_approval' || authLoginStep === 'ip_authorization') return 'compact';
		return 'standard';
	}, [authLoginStep, showSplitLogo]);
	useAuthCardPresentation({showLogoSide: showSplitLogo, variant: cardVariant});
	const renderAuthLoginStep = () => {
		if (authLoginStep === 'desktop_handoff_account') {
			return (
				<DesktopHandoffAccountSelector
					excludeCurrentUser={excludeCurrentUser}
					onSelectNewAccount={handoff.switchToLogin}
					onAccountSelected={handoff.start}
					data-flx="auth.flow.auth-login-layout.desktop-handoff-account-selector"
				/>
			);
		}
		if (authLoginStep === 'sso') {
			return (
				<AuthSsoPanel
					redirectPath={ssoRedirectPath}
					dataFlx="auth.flow.auth-login-layout.sso-panel"
					data-flx="auth.flow.auth-login-layout.render-auth-login-step.auth-sso-panel"
				/>
			);
		}
		if (authLoginStep === 'account') {
			return (
				<AccountSelector
					accounts={accounts}
					currentAccountId={currentUserId}
					error={switchError}
					disabled={isSwitching}
					clickableRows
					onSelectAccount={handleSelectExistingAccount}
					onAddAccount={handleAddAnotherAccount}
					data-flx="auth.flow.auth-login-layout.account-selector"
				/>
			);
		}
		if (authLoginStep === 'desktop_handoff_approval') {
			return (
				<ConnectedHandoffApprovalFlow
					handoff={handoff}
					data-flx="auth.flow.auth-login-layout.connected-handoff-approval-flow"
				/>
			);
		}
		if (authLoginStep === 'ip_authorization' && ipAuthChallenge) {
			return (
				<IpAuthorizationScreen
					challenge={ipAuthChallenge}
					onAuthorized={handleIpAuthorizationComplete}
					onBack={() => setIpAuthChallenge(null)}
					data-flx="auth.flow.auth-login-layout.ip-authorization-screen"
				/>
			);
		}
		return (
			<>
				{extraTopContent}
				{showTitle ? (
					<h1 className={styles.title} data-flx="auth.flow.auth-login-layout.title--2">
						{title ?? i18n._(WELCOME_BACK_DESCRIPTOR)}
					</h1>
				) : null}
				{!showAccountSelector && switchError ? (
					<div className={styles.loginNotice} role="alert" data-flx="auth.flow.auth-login-layout.login-notice--2">
						{switchError}
					</div>
				) : null}
				{ssoConfig?.enabled ? (
					<div className={styles.ssoBlock} data-flx="auth.flow.auth-login-layout.sso-block">
						<Button
							fitContainer
							onClick={handleStartSso}
							submitting={isStartingSso}
							type="button"
							data-flx="auth.flow.auth-login-layout.button.start-sso--2"
						>
							{i18n._(CONTINUE_WITH_SSO_DESCRIPTOR)}
						</Button>
						<div className={styles.ssoSubtitle} data-flx="auth.flow.auth-login-layout.sso-subtitle--2">
							{ssoConfig.enforced ? i18n._(SSO_REQUIRED_DESCRIPTOR) : i18n._(PREFER_SSO_DESCRIPTOR, {ssoDisplayName})}
						</div>
					</div>
				) : null}
				<AuthLoginEmailPasswordForm
					form={form}
					isLoading={isLoading}
					fieldErrors={fieldErrors}
					submitLabel={i18n._(SIGN_IN_DESCRIPTOR)}
					classes={{form: styles.form}}
					linksWrapperClassName={styles.formLinks}
					links={
						RuntimeConfig.emailsEnabled ? (
							<AuthRouterLink to="/forgot" className={styles.link} data-flx="auth.flow.auth-login-layout.link">
								{i18n._(FORGOT_PASSWORD_DESCRIPTOR)}
							</AuthRouterLink>
						) : null
					}
					disableSubmit={isPasskeyLoading}
					data-flx="auth.flow.auth-login-layout.auth-login-email-password-form"
				/>
				<AuthLoginDivider
					classes={{
						divider: styles.divider,
						dividerLine: styles.dividerLine,
						dividerText: styles.dividerText,
					}}
					data-flx="auth.flow.auth-login-layout.auth-login-divider"
				/>
				<AuthLoginPasskeyActions
					classes={{
						wrapper: styles.passkeyActions,
					}}
					disabled={passkeyControlsDisabled}
					onPasskeyLogin={handlePasskeyLogin}
					showBrowserOption={showBrowserPasskey}
					onBrowserLogin={handlePasskeyBrowserLogin}
					browserLabel={i18n._(SIGN_IN_VIA_BROWSER_DESCRIPTOR)}
					data-flx="auth.flow.auth-login-layout.auth-login-passkey-actions"
				/>
				<div className={styles.footer} data-flx="auth.flow.auth-login-layout.footer">
					<div className={styles.footerText} data-flx="auth.flow.auth-login-layout.footer-text">
						<span className={styles.footerLabel} data-flx="auth.flow.auth-login-layout.footer-label">
							{i18n._(NEED_ACCOUNT_DESCRIPTOR)}{' '}
						</span>
						{styledRegisterLink}
					</div>
				</div>
			</>
		);
	};
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
