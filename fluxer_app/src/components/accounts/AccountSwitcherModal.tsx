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

import {Trans} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {openAccountContextMenu, useAccountSwitcherLogic} from '~/utils/accounts/AccountSwitcherModalUtils';
import {AccountRow} from './AccountRow';
import styles from './AccountSwitcherModal.module.css';

const AccountSwitcherModal = observer(() => {
	const {
		accounts,
		currentAccount,
		isBusy,
		handleSwitchAccount,
		handleReLogin,
		handleAddAccount,
		handleLogout,
		handleRemoveAccount,
	} = useAccountSwitcherLogic();

	const hasMultipleAccounts = accounts.length > 1;

	const openMenu = React.useCallback(
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
				onRemoveAccount: handleRemoveAccount,
			});
		},
		[
			currentAccount?.userId,
			hasMultipleAccounts,
			handleSwitchAccount,
			handleReLogin,
			handleLogout,
			handleRemoveAccount,
		],
	);

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={<Trans>Manage Accounts</Trans>} />
			<Modal.Content className={styles.content}>
				{isBusy && accounts.length === 0 ? (
					<div className={styles.loadingContainer}>
						<Spinner />
					</div>
				) : accounts.length === 0 ? (
					<div className={styles.noAccounts}>
						<Trans>No accounts</Trans>
					</div>
				) : (
					<Scroller className={styles.scroller} key="account-switcher-scroller">
						<div className={styles.accountList}>
							{accounts.map((account) => {
								const isCurrent = account.userId === currentAccount?.userId;
								return (
									<AccountRow
										key={account.userId}
										account={account}
										variant="manage"
										isCurrent={isCurrent}
										isExpired={account.isValid === false}
										showInstance
										onMenuClick={openMenu(account)}
									/>
								);
							})}
						</div>
					</Scroller>
				)}
			</Modal.Content>
			<Modal.Footer className={styles.footer}>
				<Button
					variant="secondary"
					leftIcon={<PlusIcon size={18} weight="bold" />}
					onClick={handleAddAccount}
					disabled={isBusy}
					fitContainer
				>
					<Trans>Add an account</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

export default AccountSwitcherModal;
