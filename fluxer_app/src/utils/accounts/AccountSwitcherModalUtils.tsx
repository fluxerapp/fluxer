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

import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {SignOutIcon} from '@phosphor-icons/react';
import type React from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {getAccountAvatarUrl} from '~/components/accounts/AccountListItem';
import {showBrowserLoginHandoffModal} from '~/components/auth/BrowserLoginHandoffModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import i18n from '~/i18n';
import {SessionExpiredError} from '~/lib/SessionManager';
import AccountManager, {type AccountSummary} from '~/stores/AccountManager';
import {describeApiEndpoint} from '~/stores/RuntimeConfigStore';

export interface AccountSwitcherLogic {
	currentAccount: AccountSummary | null;
	accounts: Array<AccountSummary>;
	secondaryAccounts: Array<AccountSummary>;
	isBusy: boolean;
	currentInstanceLabel: string | null;
	handleSwitchAccount: (userId: string) => Promise<void>;
	handleLogout: () => Promise<void>;
	handleAddAccount: () => void;
	handleReLogin: (_userId: string) => void;
	handleRemoveAccount: (userId: string) => Promise<void>;
	getAvatarUrl: (account: AccountSummary) => string | undefined;
}

const handleLoginSuccess = async ({token, userId}: {token: string; userId: string}): Promise<void> => {
	await AuthenticationActionCreators.completeLogin({token, userId});
	ModalActionCreators.popAll();
};

export const useAccountSwitcherLogic = (): AccountSwitcherLogic => {
	const currentAccount = AccountManager.currentAccount;
	const accounts = AccountManager.getAllAccounts();

	const isBusy = AccountManager.isSwitching || AccountManager.isLoading;

	const secondaryAccounts = accounts.filter((a) => a.userId !== currentAccount?.userId);

	const currentInstanceLabel = currentAccount?.instance
		? describeApiEndpoint(currentAccount.instance.apiEndpoint)
		: null;

	const handleReLogin = (userId: string): void => {
		const account = AccountManager.accounts.get(userId);
		const email = account?.userData?.email ?? undefined;
		showBrowserLoginHandoffModal(handleLoginSuccess, undefined, email);
	};

	const handleSwitchAccount = async (userId: string): Promise<void> => {
		if (isBusy) {
			return;
		}

		try {
			await AccountManager.switchToAccount(userId);
			ModalActionCreators.pop();
		} catch (error) {
			if (error instanceof SessionExpiredError) {
				handleReLogin(userId);
			} else {
				console.error('Failed to switch account', error);
				ToastActionCreators.error(i18n._(msg`We couldn't switch accounts. Please try again.`));
			}
		}
	};

	const handleLogout = async (): Promise<void> => {
		if (isBusy) {
			return;
		}

		try {
			await AccountManager.logout();
			ModalActionCreators.pop();
		} catch (error) {
			console.error('Logout failed', error);
			ToastActionCreators.error(i18n._(msg`Logging out failed. Try again in a moment.`));
		}
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
			console.error('Failed to remove account', error);
			ToastActionCreators.error(i18n._(msg`Could not remove this account. Please try again.`));
		}
	};

	return {
		currentAccount,
		accounts,
		secondaryAccounts,
		isBusy,
		currentInstanceLabel,
		handleSwitchAccount,
		handleLogout,
		handleAddAccount,
		handleReLogin,
		handleRemoveAccount,
		getAvatarUrl: getAccountAvatarUrl,
	};
};

export interface OpenSignOutConfirmOptions {
	account: AccountSummary;
	currentAccountId: string | null;
	hasMultipleAccounts: boolean;
	onLogout: () => Promise<void>;
	onRemoveAccount: (userId: string) => Promise<void>;
}

export const openSignOutConfirm = ({
	account,
	currentAccountId,
	hasMultipleAccounts,
	onLogout,
	onRemoveAccount,
}: OpenSignOutConfirmOptions): void => {
	const displayName = account.userData?.username ?? account.userId;
	const isCurrentAccount = account.userId === currentAccountId;

	ModalActionCreators.push(
		modal(() => (
			<ConfirmModal
				title={<Trans>Sign out of {displayName}</Trans>}
				description={
					isCurrentAccount ? (
						hasMultipleAccounts ? (
							<Trans>Signing out will bring you to the login screen so you can pick another account.</Trans>
						) : (
							<Trans>Signing out will bring you to the login screen.</Trans>
						)
					) : (
						<Trans>This will remove the cached session for this account.</Trans>
					)
				}
				primaryText={<Trans>Sign out</Trans>}
				primaryVariant="danger-primary"
				onPrimary={async () => {
					if (isCurrentAccount) {
						await onLogout();
					} else {
						await onRemoveAccount(account.userId);
					}
				}}
			/>
		)),
	);
};

export interface OpenAccountContextMenuOptions {
	account: AccountSummary;
	currentAccountId: string | null;
	hasMultipleAccounts: boolean;
	onSwitch: (userId: string) => void;
	onReLogin: (userId: string) => void;
	onLogout: () => Promise<void>;
	onRemoveAccount: (userId: string) => Promise<void>;
}

export const openAccountContextMenu = (
	event: React.MouseEvent<HTMLButtonElement>,
	{
		account,
		currentAccountId,
		hasMultipleAccounts,
		onSwitch,
		onReLogin,
		onLogout,
		onRemoveAccount,
	}: OpenAccountContextMenuOptions,
): void => {
	const isCurrent = account.userId === currentAccountId;

	ContextMenuActionCreators.openFromEvent(event, (props) => (
		<MenuGroup>
			{isCurrent ? (
				<MenuItem
					danger
					icon={<SignOutIcon size={18} />}
					onClick={() => {
						props.onClose();
						openSignOutConfirm({
							account,
							currentAccountId,
							hasMultipleAccounts,
							onLogout,
							onRemoveAccount,
						});
					}}
				>
					<Trans>Sign out</Trans>
				</MenuItem>
			) : account.isValid === false ? (
				<MenuItem
					icon={<SignOutIcon size={18} />}
					onClick={() => {
						props.onClose();
						onReLogin(account.userId);
					}}
				>
					<Trans>Re-login</Trans>
				</MenuItem>
			) : (
				<MenuItem
					icon={<SignOutIcon size={18} />}
					onClick={() => {
						props.onClose();
						onSwitch(account.userId);
					}}
				>
					<Trans>Switch to this account</Trans>
				</MenuItem>
			)}
		</MenuGroup>
	));
};
