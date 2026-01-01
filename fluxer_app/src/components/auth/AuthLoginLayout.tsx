/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import {cloneElement, type ReactElement, type ReactNode, useCallback, useEffect, useMemo, useState} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import {AccountSelector} from '~/components/accounts/AccountSelector';
import AuthLoginEmailPasswordForm from '~/components/auth/AuthLoginCore/AuthLoginEmailPasswordForm';
import AuthLoginPasskeyActions, {AuthLoginDivider} from '~/components/auth/AuthLoginCore/AuthLoginPasskeyActions';
import {useDesktopHandoffFlow} from '~/components/auth/AuthLoginCore/useDesktopHandoffFlow';
import {AuthRouterLink} from '~/components/auth/AuthRouterLink';
import DesktopHandoffAccountSelector from '~/components/auth/DesktopHandoffAccountSelector';
import {HandoffCodeDisplay} from '~/components/auth/HandoffCodeDisplay';
import IpAuthorizationScreen from '~/components/auth/IpAuthorizationScreen';
import styles from '~/components/pages/LoginPage.module.css';
import {type IpAuthorizationChallenge, type LoginSuccessPayload, useLoginFormController} from '~/hooks/useLoginFlow';
import {IS_DEV} from '~/lib/env';
import {SessionExpiredError} from '~/lib/SessionManager';
import AccountManager, {type AccountSummary} from '~/stores/AccountManager';
import {isDesktop} from '~/utils/NativeUtils';
import * as RouterUtils from '~/utils/RouterUtils';

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

const AuthLoginLayout = observer(function AuthLoginLayout({
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
	const {t} = useLingui();
	const currentUserId = AccountManager.currentUserId;
	const accounts = AccountManager.orderedAccounts;
	const hasStoredAccounts = accounts.length > 0;

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

	const showLoginFormForAccount = useCallback((account: AccountSummary, message?: string | null) => {
		setShowAccountSelector(false);
		setSwitchError(message ?? null);
		setPrefillEmail(account.userData?.email ?? null);
	}, []);

	const handleLoginSuccess = useCallback(
		async ({token, userId}: LoginSuccessPayload) => {
			if (desktopHandoff) {
				await handoff.start({token, userId});
				return;
			}
			await AuthenticationActionCreators.completeLogin({token, userId});
			await onLoginComplete?.({token, userId});
		},
		[desktopHandoff, handoff, onLoginComplete],
	);

	const {form, isLoading, fieldErrors, handlePasskeyLogin, handlePasskeyBrowserLogin, isPasskeyLoading} =
		useLoginFormController({
			redirectPath,
			inviteCode,
			onLoginSuccess: handleLoginSuccess,
			onRequireMfa: (challenge) => {
				AuthenticationActionCreators.setMfaTicket(challenge);
			},
			onRequireIpAuthorization: (challenge) => {
				setIpAuthChallenge(challenge);
			},
		});

	const showBrowserPasskey = IS_DEV || isDesktop();
	const passkeyControlsDisabled = isLoading || Boolean(form.isSubmitting) || isPasskeyLoading;

	const handleIpAuthorizationComplete = useCallback(
		async ({token, userId}: LoginSuccessPayload) => {
			await handleLoginSuccess({token, userId});
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
	}, [form, prefillEmail]);

	const handleSelectExistingAccount = useCallback(
		async (account: AccountSummary) => {
			const identifier = account.userData?.email ?? account.userData?.username ?? account.userId;
			const expiredMessage = t`Session expired for ${identifier}. Please log in again.`;

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

				setSwitchError(error instanceof Error ? error.message : t`Failed to switch account`);
			} finally {
				setIsSwitching(false);
			}
		},
		[showLoginFormForAccount],
	);

	const handleAddAnotherAccount = useCallback(() => {
		setShowAccountSelector(false);
		setSwitchError(null);
		setPrefillEmail(null);
	}, []);

	const styledRegisterLink = useMemo(() => {
		const {className: linkClassName} = registerLink.props as {className?: string};
		return cloneElement(registerLink, {
			className: clsx(styles.footerLink, linkClassName),
		});
	}, [registerLink]);

	if (desktopHandoff && handoff.mode === 'selecting') {
		return (
			<DesktopHandoffAccountSelector
				excludeCurrentUser={excludeCurrentUser}
				onSelectNewAccount={handoff.switchToLogin}
			/>
		);
	}

	if (showAccountSelector && hasStoredAccounts && !desktopHandoff) {
		return (
			<AccountSelector
				accounts={accounts}
				currentAccountId={currentUserId}
				error={switchError}
				disabled={isSwitching}
				showInstance
				onSelectAccount={handleSelectExistingAccount}
				onAddAccount={handleAddAnotherAccount}
			/>
		);
	}

	if (desktopHandoff && (handoff.mode === 'generating' || handoff.mode === 'displaying' || handoff.mode === 'error')) {
		return (
			<HandoffCodeDisplay
				code={handoff.code}
				isGenerating={handoff.mode === 'generating'}
				error={handoff.mode === 'error' ? handoff.error : null}
				onRetry={handoff.retry}
			/>
		);
	}

	if (ipAuthChallenge) {
		return (
			<IpAuthorizationScreen
				challenge={ipAuthChallenge}
				onAuthorized={handleIpAuthorizationComplete}
				onBack={() => setIpAuthChallenge(null)}
			/>
		);
	}

	return (
		<>
			{extraTopContent}

			{showTitle ? <h1 className={styles.title}>{title ?? <Trans>Welcome back</Trans>}</h1> : null}

			{!showAccountSelector && switchError ? <div className={styles.loginNotice}>{switchError}</div> : null}

			<AuthLoginEmailPasswordForm
				form={form}
				isLoading={isLoading}
				fieldErrors={fieldErrors}
				submitLabel={<Trans>Log in</Trans>}
				classes={{form: styles.form}}
				linksWrapperClassName={styles.formLinks}
				links={
					<AuthRouterLink to="/forgot" className={styles.link}>
						<Trans>Forgot your password?</Trans>
					</AuthRouterLink>
				}
				disableSubmit={isPasskeyLoading}
			/>

			<AuthLoginDivider
				classes={{
					divider: styles.divider,
					dividerLine: styles.dividerLine,
					dividerText: styles.dividerText,
				}}
			/>

			<AuthLoginPasskeyActions
				classes={{
					wrapper: styles.passkeyActions,
				}}
				disabled={passkeyControlsDisabled}
				onPasskeyLogin={handlePasskeyLogin}
				showBrowserOption={showBrowserPasskey}
				onBrowserLogin={handlePasskeyBrowserLogin}
				browserLabel={<Trans>Log in via browser or custom instance</Trans>}
			/>

			<div className={styles.footer}>
				<div className={styles.footerText}>
					<span className={styles.footerLabel}>
						<Trans>Need an account?</Trans>{' '}
					</span>
					{styledRegisterLink}
				</div>
			</div>
		</>
	);
});

export {AuthLoginLayout};
