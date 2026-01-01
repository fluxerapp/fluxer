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
import {PlusIcon, SignOutIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Button} from '~/components/uikit/Button/Button';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {Scroller} from '~/components/uikit/Scroller';
import AccountManager, {type AccountSummary} from '~/stores/AccountManager';
import {AccountRow} from './AccountRow';
import styles from './AccountSelector.module.css';

interface AccountSelectorProps {
	accounts: Array<AccountSummary>;
	currentAccountId?: string | null;
	title?: React.ReactNode;
	description?: React.ReactNode;
	error?: string | null;
	disabled?: boolean;
	showInstance?: boolean;
	clickableRows?: boolean;
	addButtonLabel?: React.ReactNode;
	onSelectAccount: (account: AccountSummary) => void;
	onAddAccount?: () => void;
	scrollerKey?: string;
}

export const AccountSelector = observer(
	({
		accounts,
		currentAccountId,
		title,
		description,
		error,
		disabled = false,
		showInstance = false,
		clickableRows = false,
		addButtonLabel,
		onSelectAccount,
		onAddAccount,
		scrollerKey,
	}: AccountSelectorProps) => {
		const {t} = useLingui();
		const defaultTitle = <Trans>Choose an account</Trans>;
		const defaultDescription = <Trans>Select an account to continue, or add a different one.</Trans>;
		const hasMultipleAccounts = accounts.length > 1;

		const openSignOutConfirm = React.useCallback(
			(account: AccountSummary) => {
				const displayName = account.userData?.username ?? account.userId;

				ModalActionCreators.push(
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
							primaryVariant="danger-primary"
							onPrimary={async () => {
								try {
									await AccountManager.removeStoredAccount(account.userId);
								} catch (error) {
									console.error('Failed to remove account', error);
									ToastActionCreators.error(t`We couldn't remove that account. Please try again.`);
								}
							}}
						/>
					)),
				);
			},
			[hasMultipleAccounts, t],
		);

		const openMenu = React.useCallback(
			(account: AccountSummary) => (event: React.MouseEvent<HTMLButtonElement>) => {
				event.preventDefault();
				event.stopPropagation();
				ContextMenuActionCreators.openFromEvent(event, (props) => (
					<MenuGroup>
						<MenuItem
							icon={<SignOutIcon size={18} />}
							onClick={() => {
								props.onClose();
								onSelectAccount(account);
							}}
						>
							<Trans>Select account</Trans>
						</MenuItem>
						<MenuItem
							danger
							icon={<SignOutIcon size={18} />}
							onClick={() => {
								props.onClose();
								openSignOutConfirm(account);
							}}
						>
							<Trans>Remove</Trans>
						</MenuItem>
					</MenuGroup>
				));
			},
			[openSignOutConfirm, onSelectAccount],
		);

		return (
			<div className={styles.container}>
				<h1 className={styles.title}>{title ?? defaultTitle}</h1>
				<p className={styles.description}>{description ?? defaultDescription}</p>

				{error && <div className={styles.error}>{error}</div>}

				<div className={styles.accountListWrapper}>
					{accounts.length === 0 ? (
						<div className={styles.noAccounts}>
							<Trans>No accounts</Trans>
						</div>
					) : (
						<Scroller className={styles.scroller} key={scrollerKey ?? 'account-selector-scroller'}>
							<div className={styles.accountList}>
								{accounts.map((account) => {
									const isCurrent = account.userId === currentAccountId;
									return (
										<AccountRow
											key={account.userId}
											account={account}
											variant="manage"
											isCurrent={isCurrent}
											isExpired={account.isValid === false}
											showInstance={showInstance}
											onClick={clickableRows && !disabled ? () => onSelectAccount(account) : undefined}
											showCaretIndicator={clickableRows}
											onMenuClick={!clickableRows && !disabled ? openMenu(account) : undefined}
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
						leftIcon={<PlusIcon size={18} weight="bold" />}
						onClick={onAddAccount}
						fitContainer
					>
						{addButtonLabel ?? <Trans>Add an account</Trans>}
					</Button>
				)}
			</div>
		);
	},
);
