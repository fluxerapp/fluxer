// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {getAccountDisplayName} from '@app/features/auth/components/accounts/AccountListItem';
import {AccountRow} from '@app/features/auth/components/accounts/AccountRow';
import styles from '@app/features/auth/components/accounts/AccountSelector.module.css';
import AccountManager from '@app/features/auth/state/AccountManager';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PlusIcon, SignOutIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const WE_COULDN_T_REMOVE_THAT_ACCOUNT_PLEASE_TRY_DESCRIPTOR = msg({
	message: "We couldn't remove that account. Try again.",
	comment: 'Toast error shown in the account switcher when removing a saved account from the device fails.',
});

interface AccountSelectorProps {
	accounts: Array<Account>;
	currentAccountId?: string | null;
	title?: React.ReactNode;
	description?: React.ReactNode;
	error?: string | null;
	disabled?: boolean;
	clickableRows?: boolean;
	addButtonLabel?: React.ReactNode;
	onSelectAccount: (account: Account) => void;
	onAddAccount?: () => void;
	scrollerKey?: string;
}

const logger = new Logger('AccountSelector');
export const AccountSelector = observer(
	({
		accounts,
		currentAccountId,
		title,
		description,
		error,
		disabled = false,
		clickableRows = false,
		addButtonLabel,
		onSelectAccount,
		onAddAccount,
		scrollerKey,
	}: AccountSelectorProps) => {
		const {i18n} = useLingui();
		const defaultTitle = <Trans>Choose an account</Trans>;
		const defaultDescription = <Trans>Select an account to continue, or add a different one.</Trans>;
		const hasMultipleAccounts = accounts.length > 1;
		const openSignOutConfirm = useCallback(
			(account: Account) => {
				const displayName = getAccountDisplayName(account, account.userId);
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={<Trans>Remove {displayName}</Trans>}
							description={
								hasMultipleAccounts ? (
									<Trans>This will remove the saved session for this account.</Trans>
								) : (
									<Trans>This will remove the only saved account on this device.</Trans>
								)
							}
							primaryText={<Trans>Remove</Trans>}
							primaryVariant="danger"
							onPrimary={async () => {
								try {
									await AccountManager.removeStoredAccount(account.userId);
								} catch (error) {
									logger.error('Failed to remove account', error);
									showGenericErrorModal({
										title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
										message: () => i18n._(WE_COULDN_T_REMOVE_THAT_ACCOUNT_PLEASE_TRY_DESCRIPTOR),
										dataFlx: 'auth.accounts.account-selector.remove-account-error-modal',
										defer: true,
									});
								}
							}}
							data-flx="auth.accounts.account-selector.open-sign-out-confirm.confirm-modal"
						/>
					)),
				);
			},
			[hasMultipleAccounts, i18n],
		);
		const openMenu = useCallback(
			(account: Account) => (event: React.MouseEvent<HTMLButtonElement>) => {
				event.preventDefault();
				event.stopPropagation();
				ContextMenuCommands.openFromEvent(event, (props) => (
					<MenuGroup data-flx="auth.accounts.account-selector.open-menu.menu-group">
						<MenuItem
							icon={<SignOutIcon size={18} data-flx="auth.accounts.account-selector.open-menu.sign-out-icon" />}
							onClick={() => {
								props.onClose();
								onSelectAccount(account);
							}}
							data-flx="auth.accounts.account-selector.open-menu.menu-item.close"
						>
							<Trans>Select account</Trans>
						</MenuItem>
						<MenuItem
							danger
							icon={<SignOutIcon size={18} data-flx="auth.accounts.account-selector.open-menu.sign-out-icon--2" />}
							onClick={() => {
								props.onClose();
								openSignOutConfirm(account);
							}}
							data-flx="auth.accounts.account-selector.open-menu.menu-item.close--2"
						>
							<Trans>Remove</Trans>
						</MenuItem>
					</MenuGroup>
				));
			},
			[openSignOutConfirm, onSelectAccount],
		);
		return (
			<div className={styles.container} data-flx="auth.accounts.account-selector.container">
				<h1 className={styles.title} data-flx="auth.accounts.account-selector.title">
					{title ?? defaultTitle}
				</h1>
				<p className={styles.description} data-flx="auth.accounts.account-selector.description">
					{description ?? defaultDescription}
				</p>
				{error && (
					<div className={styles.error} role="alert" data-flx="auth.accounts.account-selector.error">
						{error}
					</div>
				)}
				<div className={styles.accountListWrapper} data-flx="auth.accounts.account-selector.account-list-wrapper">
					{accounts.length === 0 ? (
						<div className={styles.noAccounts} data-flx="auth.accounts.account-selector.no-accounts">
							<Trans>No accounts</Trans>
						</div>
					) : (
						<Scroller
							className={styles.scroller}
							key={scrollerKey ?? 'account-selector-scroller'}
							data-flx="auth.accounts.account-selector.scroller"
						>
							<div className={styles.accountList} data-flx="auth.accounts.account-selector.account-list">
								{accounts.map((account) => {
									const isCurrent = account.userId === currentAccountId;
									return (
										<AccountRow
											key={account.userId}
											account={account}
											variant="manage"
											isCurrent={isCurrent}
											isExpired={account.isValid === false}
											onClick={clickableRows && !disabled ? () => onSelectAccount(account) : undefined}
											showCaretIndicator={clickableRows}
											onMenuClick={!clickableRows && !disabled ? openMenu(account) : undefined}
											data-flx="auth.accounts.account-selector.account-row.select-account"
										/>
									);
								})}
							</div>
						</Scroller>
					)}
				</div>
				{onAddAccount && (
					<Button
						variant="secondary"
						leftIcon={<PlusIcon size={18} weight="bold" data-flx="auth.accounts.account-selector.plus-icon" />}
						onClick={onAddAccount}
						fitContainer
						data-flx="auth.accounts.account-selector.button.add-account"
					>
						{addButtonLabel ?? <Trans>Add an account</Trans>}
					</Button>
				)}
			</div>
		);
	},
);
