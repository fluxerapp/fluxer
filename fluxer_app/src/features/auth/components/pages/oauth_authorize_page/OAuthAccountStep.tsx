// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AccountList} from '@app/features/auth/components/accounts/AccountList';
import {USE_ANOTHER_ACCOUNT_DESCRIPTOR} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import {AccountSwitcherAuthFlow} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow';
import {
	type AccountSwitcherAuthPurpose,
	AccountSwitcherAuthPurposeType,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import styles from '@app/features/auth/components/pages/OAuthAuthorizePage.module.css';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {currentInstanceAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import {AuthRegisterDraftContext, useAuthRegisterDraft} from '@app/features/auth/state/AuthRegisterDraftContext';
import {
	logoutCurrentAccountFromSwitcher,
	logoutStoredAccountFromSwitcher,
	switchStoredAccountFromSwitcher,
} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {UserPlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const CHOOSE_AN_ACCOUNT_DESCRIPTOR = msg({
	message: 'Choose an account',
	comment: 'OAuth authorization step title. The user must choose which account to authorize with.',
});
const CLIENT_WILL_USE_THIS_ACCOUNT_DESCRIPTOR = msg({
	message: '{clientName} will use this account for this authorization.',
	comment:
		'OAuth authorization account step description. {clientName} is the OAuth application name requesting access.',
});
const CONTINUE_DESCRIPTOR = msg({
	message: 'Continue',
	comment: 'OAuth authorization primary button. Advances to the next step after choosing an account.',
});

function isUsable(account: Account | null): account is Account {
	return account != null && account.isValid !== false;
}

function initialSignInPurpose(
	sessionExpired: boolean,
	accounts: ReadonlyArray<Account>,
): AccountSwitcherAuthPurpose | null {
	const currentAccount = Accounts.currentAccount;
	if (sessionExpired && currentAccount != null) {
		return {type: AccountSwitcherAuthPurposeType.RELOGIN, account: currentAccount};
	}
	if (accounts.length === 0) {
		return {type: AccountSwitcherAuthPurposeType.ADD, instance: RuntimeConfig.getSnapshotOrNull()};
	}
	return null;
}

function currentPagePath(): string {
	return `${window.location.pathname}${window.location.search}`;
}

function currentPageLoginPath(): string {
	return setPathQueryParams(Routes.LOGIN, {redirect_to: currentPagePath()});
}

interface OAuthAccountStepProps {
	clientLabel: string;
	sessionExpired: boolean;
	onCancel: () => void;
	onContinue: () => void;
	onAccountChanged: () => void;
}

export const OAuthAccountStep: React.FC<OAuthAccountStepProps> = observer(
	({clientLabel, sessionExpired, onCancel, onContinue, onAccountChanged}) => {
		const {i18n} = useLingui();
		const authRegisterDraftContextValue = useAuthRegisterDraft();
		const accounts = currentInstanceAccounts();
		const currentAccountKey = Accounts.currentAccountKey;
		const isBusy = Accounts.isSwitching || Accounts.isLoading;
		const [signInPurpose, setSignInPurpose] = useState<AccountSwitcherAuthPurpose | null>(() =>
			initialSignInPurpose(sessionExpired, accounts),
		);
		const [chosenAccountKey, setChosenAccountKey] = useState<string | null>(() =>
			!sessionExpired && isUsable(Accounts.currentAccount) ? currentAccountKey : null,
		);
		const [error, setError] = useState<string | null>(null);
		const [focusSignInOnShow, setFocusSignInOnShow] = useState(false);
		const [returnFocusPurpose, setReturnFocusPurpose] = useState<AccountSwitcherAuthPurpose | null>(null);
		const activeRowRef = useRef<HTMLButtonElement | null>(null);
		const addAccountButtonRef = useRef<HTMLButtonElement | null>(null);
		const chosenAccount = accounts.find((account) => getAccountKey(account) === chosenAccountKey) ?? null;
		const startRelogin = useCallback((account: Account) => {
			setSignInPurpose({type: AccountSwitcherAuthPurposeType.RELOGIN, account});
			setFocusSignInOnShow(true);
		}, []);
		const startAddAccount = useCallback(() => {
			setSignInPurpose({type: AccountSwitcherAuthPurposeType.ADD, instance: RuntimeConfig.getSnapshotOrNull()});
			setFocusSignInOnShow(true);
		}, []);
		const handleSelect = useCallback(
			(account: Account) => {
				if (account.isValid === false) {
					startRelogin(account);
					return;
				}
				setChosenAccountKey(getAccountKey(account));
			},
			[startRelogin],
		);
		const handleContinue = useCallback(() => {
			if (!isUsable(chosenAccount)) {
				return;
			}
			const accountKey = getAccountKey(chosenAccount);
			if (accountKey === currentAccountKey && !sessionExpired) {
				onContinue();
				return;
			}
			setError(null);
			void switchStoredAccountFromSwitcher({
				accountKey,
				onSessionExpired: startRelogin,
				onSuccess: onAccountChanged,
				onError: setError,
				redirectAfterSwitch: null,
				switchAccount: null,
			});
		}, [chosenAccount, currentAccountKey, onAccountChanged, onContinue, sessionExpired, startRelogin]);
		const canSignOut = useCallback(
			(account: Account) => sessionExpired || getAccountKey(account) !== Accounts.currentAccountKey,
			[sessionExpired],
		);
		const handleSignOut = useCallback(async (account: Account) => {
			setError(null);
			if (getAccountKey(account) === Accounts.currentAccountKey) {
				await logoutCurrentAccountFromSwitcher(currentPageLoginPath(), setError);
				return;
			}
			await logoutStoredAccountFromSwitcher(account, setError);
		}, []);
		const backToAccounts = useCallback(() => {
			AuthenticationCommands.clearMfaTicket();
			setReturnFocusPurpose(signInPurpose);
			setSignInPurpose(null);
		}, [signInPurpose]);
		useEffect(() => {
			if (signInPurpose != null || returnFocusPurpose == null) {
				return;
			}
			setReturnFocusPurpose(null);
			if (returnFocusPurpose.type === AccountSwitcherAuthPurposeType.RELOGIN) {
				activeRowRef.current?.focus();
				return;
			}
			addAccountButtonRef.current?.focus();
		}, [returnFocusPurpose, signInPurpose]);
		if (signInPurpose != null) {
			return (
				<div className={styles.page} data-flx="auth.o-auth-authorize-page.account-step.sign-in">
					<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
						<AccountSwitcherAuthFlow
							purpose={signInPurpose}
							renderFrame={null}
							redirectAfterLogin={currentPagePath()}
							focusOnEntry={focusSignInOnShow}
							exitCancels={accounts.length === 0}
							onExit={accounts.length === 0 ? onCancel : backToAccounts}
							onLoginComplete={onAccountChanged}
							data-flx="auth.o-auth-authorize-page.account-step.account-switcher-auth-flow"
						/>
					</AuthRegisterDraftContext.Provider>
				</div>
			);
		}
		return (
			<div className={styles.page} data-flx="auth.o-auth-authorize-page.account-step.page">
				<div className={styles.heroCard} data-flx="auth.o-auth-authorize-page.account-step.hero-card">
					<div className={styles.heroCopy} data-flx="auth.o-auth-authorize-page.account-step.hero-copy">
						<h1 className={styles.heroTitle} data-flx="auth.o-auth-authorize-page.account-step.hero-title">
							{i18n._(CHOOSE_AN_ACCOUNT_DESCRIPTOR)}
						</h1>
						<p className={styles.heroDescription} data-flx="auth.o-auth-authorize-page.account-step.hero-description">
							{i18n._(CLIENT_WILL_USE_THIS_ACCOUNT_DESCRIPTOR, {clientName: clientLabel})}
						</p>
					</div>
				</div>
				<AccountList
					accounts={accounts}
					currentAccountKey={sessionExpired ? null : currentAccountKey}
					selectedAccountKey={chosenAccountKey}
					initialActiveAccountKey={
						returnFocusPurpose?.type === AccountSwitcherAuthPurposeType.RELOGIN
							? getAccountKey(returnFocusPurpose.account)
							: null
					}
					activeRowRef={activeRowRef}
					emptyFocusRef={addAccountButtonRef}
					error={error}
					disabled={isBusy}
					onSelect={handleSelect}
					onSignOut={handleSignOut}
					canSignOut={canSignOut}
					data-flx="auth.o-auth-authorize-page.account-step.account-list"
				/>
				<Button
					ref={addAccountButtonRef}
					type="button"
					variant="secondary"
					leftIcon={
						<UserPlusIcon
							size={remFromPx(18)}
							weight="bold"
							data-flx="auth.o-auth-authorize-page.account-step.user-plus-icon"
						/>
					}
					onClick={startAddAccount}
					disabled={isBusy}
					fitContainer
					data-flx="auth.o-auth-authorize-page.account-step.button.add-account"
				>
					{i18n._(USE_ANOTHER_ACCOUNT_DESCRIPTOR)}
				</Button>
				<div className={styles.sectionDivider} data-flx="auth.o-auth-authorize-page.account-step.section-divider" />
				<div className={styles.actions} data-flx="auth.o-auth-authorize-page.account-step.actions">
					<Button
						type="button"
						variant="secondary"
						onClick={onCancel}
						disabled={isBusy}
						className={styles.actionButton}
						data-flx="auth.o-auth-authorize-page.account-step.button.cancel"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="button"
						onClick={handleContinue}
						disabled={!isUsable(chosenAccount)}
						submitting={isBusy}
						className={styles.actionButton}
						data-flx="auth.o-auth-authorize-page.account-step.button.continue"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		);
	},
);
