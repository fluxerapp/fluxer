// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {AccountRow} from '@app/features/auth/components/accounts/AccountRow';
import styles from '@app/features/auth/components/accounts/AccountSwitcherModal.module.css';
import {openAccountContextMenu, useAccountSwitcherLogic} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import {Button} from '@app/features/ui/button/Button';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Trans} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface AccountSwitcherModalProps {
	redirectAfterSwitch?: string | null;
	redirectAfterLogin?: string | null;
	switchAccount?: (userId: string) => Promise<void>;
	'data-flx'?: string;
}

const AccountSwitcherModal = observer((props: AccountSwitcherModalProps) => {
	const {redirectAfterSwitch, redirectAfterLogin, switchAccount} = props;
	const {
		accounts,
		currentAccount,
		isBusy,
		handleSwitchAccount,
		handleReLogin,
		handleAddAccount,
		handleLogout,
		handleLogoutStoredAccount,
	} = useAccountSwitcherLogic({redirectAfterSwitch, redirectAfterLogin, switchAccount});
	const hasMultipleAccounts = accounts.length > 1;
	const openMenu = useCallback(
		(account: (typeof accounts)[number]) => (event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			openAccountContextMenu(event, {
				account,
				currentAccountId: currentAccount?.userId ?? null,
				hasMultipleAccounts,
				onSwitch: handleSwitchAccount,
				onReLogin: handleReLogin,
				onLogout: handleLogout,
				onLogoutStoredAccount: handleLogoutStoredAccount,
			});
		},
		[
			currentAccount?.userId,
			hasMultipleAccounts,
			handleSwitchAccount,
			handleReLogin,
			handleLogout,
			handleLogoutStoredAccount,
		],
	);
	const handleAccountClick = useCallback(
		(account: (typeof accounts)[number]) => {
			if (isBusy) {
				return;
			}
			if (account.userId === currentAccount?.userId) {
				return;
			}
			if (account.isValid === false) {
				handleReLogin(account.userId);
				return;
			}
			void handleSwitchAccount(account.userId);
		},
		[currentAccount?.userId, handleReLogin, handleSwitchAccount, isBusy],
	);
	return (
		<Modal.Root size="small" centered data-flx="auth.accounts.account-switcher-modal.modal-root">
			<Modal.Header
				title={<Trans>Manage accounts</Trans>}
				data-flx="auth.accounts.account-switcher-modal.modal-header"
			/>
			<Modal.Content className={styles.content} data-flx="auth.accounts.account-switcher-modal.content">
				{isBusy && accounts.length === 0 ? (
					<div className={styles.loadingContainer} data-flx="auth.accounts.account-switcher-modal.loading-container">
						<Spinner data-flx="auth.accounts.account-switcher-modal.spinner" />
					</div>
				) : accounts.length === 0 ? (
					<div className={styles.noAccounts} data-flx="auth.accounts.account-switcher-modal.no-accounts">
						<Trans>No accounts</Trans>
					</div>
				) : (
					<Scroller
						className={styles.scroller}
						key="account-switcher-scroller"
						data-flx="auth.accounts.account-switcher-modal.scroller"
					>
						<div className={styles.accountList} data-flx="auth.accounts.account-switcher-modal.account-list">
							{accounts.map((account) => {
								const isCurrent = account.userId === currentAccount?.userId;
								return (
									<AccountRow
										key={account.userId}
										account={account}
										variant="manage"
										isCurrent={isCurrent}
										isExpired={account.isValid === false}
										onClick={isCurrent ? undefined : () => handleAccountClick(account)}
										onMenuClick={openMenu(account)}
										data-flx="auth.accounts.account-switcher-modal.account-row"
									/>
								);
							})}
						</div>
					</Scroller>
				)}
			</Modal.Content>
			<Modal.Footer className={styles.footer} data-flx="auth.accounts.account-switcher-modal.footer">
				<Button
					variant="secondary"
					leftIcon={<PlusIcon size={18} weight="bold" data-flx="auth.accounts.account-switcher-modal.plus-icon" />}
					onClick={handleAddAccount}
					disabled={isBusy}
					data-flx="auth.accounts.account-switcher-modal.button.add-account"
				>
					<Trans>Add an account</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

export default AccountSwitcherModal;
