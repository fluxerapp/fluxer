// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {getAccountAvatarUrl, getAccountDisplayName} from '@app/features/auth/components/accounts/AccountListItem';
import {showBrowserLoginHandoffModal} from '@app/features/auth/flow/BrowserLoginHandoffModal';
import AccountManager from '@app/features/auth/state/AccountManager';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {SignOutIcon} from '@phosphor-icons/react';
import type React from 'react';

const WE_COULDN_T_SWITCH_ACCOUNTS_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't switch accounts. Try again.",
	comment: 'Toast error shown when switching to a different saved account fails.',
});
const SIGNING_OUT_FAILED_TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR = msg({
	message: 'Signing out failed. Try again in a moment.',
	comment: 'Toast error shown when signing out from the account switcher fails.',
});
const WE_COULDN_T_REMOVE_THIS_ACCOUNT_PLEASE_TRY_DESCRIPTOR = msg({
	message: "Couldn't remove this account. Try again.",
	comment: 'Toast error shown when removing the current account from the device fails.',
});
const logger = new Logger('AccountSwitcherModalUtils');

function showAccountSwitcherErrorModal(message: MessageDescriptor): void {
	showGenericErrorModal({
		title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
		message: () => i18n._(message),
		dataFlx: 'auth.account-switcher-modal-utils.error-modal',
		defer: true,
	});
}

export interface AccountSwitcherLogic {
	currentAccount: Account | null;
	accounts: Array<Account>;
	isBusy: boolean;
	handleSwitchAccount: (userId: string) => Promise<void>;
	handleLogout: () => Promise<void>;
	handleLogoutStoredAccount: (account: Account) => Promise<void>;
	handleAddAccount: () => void;
	handleReLogin: (_userId: string) => void;
	handleRemoveAccount: (userId: string) => Promise<void>;
	getAvatarUrl: (account: Account) => string | undefined;
}

export interface AccountSwitcherLogicOptions {
	redirectAfterSwitch?: string | null;
	redirectAfterLogin?: string | null;
	switchAccount?: (userId: string) => Promise<void>;
}

export function useAccountSwitcherLogic(options: AccountSwitcherLogicOptions = {}): AccountSwitcherLogic {
	const {redirectAfterSwitch = undefined, redirectAfterLogin = undefined, switchAccount} = options;
	const currentAccount = AccountManager.currentAccount;
	const accounts = AccountManager.getAllAccounts();
	const isBusy = AccountManager.isSwitching || AccountManager.isLoading;
	const handleLoginSuccess = async (payload: LoginSuccessPayload): Promise<void> => {
		await AuthenticationCommands.completeLogin(payload, {redirectPath: redirectAfterLogin});
		ModalCommands.popAll();
	};
	const handleReLogin = (userId: string): void => {
		const account = AccountManager.accounts.get(userId);
		if (!account) {
			showAccountSwitcherErrorModal(WE_COULDN_T_SWITCH_ACCOUNTS_PLEASE_TRY_AGAIN_DESCRIPTOR);
			return;
		}
		const email = account.userData?.email ?? undefined;
		showBrowserLoginHandoffModal(handleLoginSuccess, email);
	};
	const handleSwitchAccount = async (userId: string): Promise<void> => {
		if (isBusy) {
			return;
		}
		try {
			if (switchAccount) {
				await switchAccount(userId);
			} else {
				await AccountManager.switchToAccount(userId, redirectAfterSwitch);
			}
			ModalCommands.pop();
		} catch (error) {
			if (error instanceof SessionExpiredError) {
				handleReLogin(userId);
			} else {
				logger.error('Failed to switch account', error);
				showAccountSwitcherErrorModal(WE_COULDN_T_SWITCH_ACCOUNTS_PLEASE_TRY_AGAIN_DESCRIPTOR);
			}
		}
	};
	const handleLogout = async (): Promise<void> => {
		if (isBusy) {
			return;
		}
		try {
			await AccountManager.logout();
			ModalCommands.pop();
		} catch (error) {
			logger.error('Logout failed', error);
			showAccountSwitcherErrorModal(SIGNING_OUT_FAILED_TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR);
		}
	};
	const handleLogoutStoredAccount = async (account: Account): Promise<void> => {
		if (isBusy) {
			return;
		}
		try {
			await http.post(Endpoints.AUTH_LOGOUT, {
				headers: {Authorization: account.token},
				timeoutMs: 5000,
				retries: 0,
				auth: 'none',
			});
		} catch (error) {
			logger.warn('Failed to log out stored account', error);
		}
		await handleRemoveAccount(account.userId);
	};
	const handleAddAccount = (): void => {
		showBrowserLoginHandoffModal(handleLoginSuccess);
	};
	const handleRemoveAccount = async (userId: string): Promise<void> => {
		if (isBusy) {
			return;
		}
		try {
			await AccountManager.removeStoredAccount(userId);
		} catch (error) {
			logger.error('Failed to remove account', error);
			showAccountSwitcherErrorModal(WE_COULDN_T_REMOVE_THIS_ACCOUNT_PLEASE_TRY_DESCRIPTOR);
		}
	};
	return {
		currentAccount,
		accounts,
		isBusy,
		handleSwitchAccount,
		handleLogout,
		handleLogoutStoredAccount,
		handleAddAccount,
		handleReLogin,
		handleRemoveAccount,
		getAvatarUrl: getAccountAvatarUrl,
	};
}

export interface OpenSignOutConfirmOptions {
	account: Account;
	currentAccountId: string | null;
	hasMultipleAccounts: boolean;
	onLogout: () => Promise<void>;
	onLogoutStoredAccount: (account: Account) => Promise<void>;
}

export function openSignOutConfirm({
	account,
	currentAccountId,
	hasMultipleAccounts,
	onLogout,
	onLogoutStoredAccount,
}: OpenSignOutConfirmOptions): void {
	const displayName = getAccountDisplayName(account, account.userId);
	const isCurrentAccount = account.userId === currentAccountId;
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={<Trans>Sign out of {displayName}</Trans>}
				description={
					isCurrentAccount ? (
						hasMultipleAccounts ? (
							<Trans>Signing out will bring you to the sign-in screen so you can pick another account.</Trans>
						) : (
							<Trans>Signing out will bring you to the sign-in screen.</Trans>
						)
					) : (
						<Trans>Signing out will remove this account from the device.</Trans>
					)
				}
				primaryText={<Trans>Sign out</Trans>}
				primaryVariant="danger"
				onPrimary={async () => {
					if (isCurrentAccount) {
						await onLogout();
					} else {
						await onLogoutStoredAccount(account);
					}
				}}
				data-flx="auth.account-switcher-modal-utils.open-sign-out-confirm.confirm-modal"
			/>
		)),
	);
}

export interface OpenAccountContextMenuOptions {
	account: Account;
	currentAccountId: string | null;
	hasMultipleAccounts: boolean;
	onSwitch: (userId: string) => void;
	onReLogin: (userId: string) => void;
	onLogout: () => Promise<void>;
	onLogoutStoredAccount: (account: Account) => Promise<void>;
}

export function openAccountContextMenu(
	event: React.MouseEvent<HTMLButtonElement>,
	{
		account,
		currentAccountId,
		hasMultipleAccounts,
		onSwitch,
		onReLogin,
		onLogout,
		onLogoutStoredAccount,
	}: OpenAccountContextMenuOptions,
): void {
	const isCurrent = account.userId === currentAccountId;
	ContextMenuCommands.openFromEvent(event, (props) => (
		<MenuGroup data-flx="auth.account-switcher-modal-utils.open-account-context-menu.menu-group">
			{isCurrent ? (
				<MenuItem
					danger
					icon={
						<SignOutIcon
							size={18}
							data-flx="auth.account-switcher-modal-utils.open-account-context-menu.sign-out-icon"
						/>
					}
					onClick={() => {
						props.onClose();
						openSignOutConfirm({
							account,
							currentAccountId,
							hasMultipleAccounts,
							onLogout,
							onLogoutStoredAccount,
						});
					}}
					data-flx="auth.account-switcher-modal-utils.open-account-context-menu.menu-item.close"
				>
					<Trans>Sign out</Trans>
				</MenuItem>
			) : (
				<>
					{account.isValid === false ? (
						<MenuItem
							icon={
								<SignOutIcon
									size={18}
									data-flx="auth.account-switcher-modal-utils.open-account-context-menu.sign-out-icon--2"
								/>
							}
							onClick={() => {
								props.onClose();
								onReLogin(account.userId);
							}}
							data-flx="auth.account-switcher-modal-utils.open-account-context-menu.menu-item.close--2"
						>
							<Trans>Sign in again</Trans>
						</MenuItem>
					) : (
						<MenuItem
							icon={
								<SignOutIcon
									size={18}
									data-flx="auth.account-switcher-modal-utils.open-account-context-menu.sign-out-icon--3"
								/>
							}
							onClick={() => {
								props.onClose();
								onSwitch(account.userId);
							}}
							data-flx="auth.account-switcher-modal-utils.open-account-context-menu.menu-item.close--3"
						>
							<Trans>Switch to this account</Trans>
						</MenuItem>
					)}
					<MenuItem
						danger
						icon={
							<SignOutIcon
								size={18}
								data-flx="auth.account-switcher-modal-utils.open-account-context-menu.sign-out-icon--4"
							/>
						}
						onClick={() => {
							props.onClose();
							openSignOutConfirm({
								account,
								currentAccountId,
								hasMultipleAccounts,
								onLogout,
								onLogoutStoredAccount,
							});
						}}
						data-flx="auth.account-switcher-modal-utils.open-account-context-menu.menu-item.close--4"
					>
						<Trans>Sign out</Trans>
					</MenuItem>
				</>
			)}
		</MenuGroup>
	));
}
