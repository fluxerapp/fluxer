// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountList} from '@app/features/auth/components/accounts/AccountList';
import styles from '@app/features/auth/components/accounts/AccountSelector.module.css';
import {USE_ANOTHER_ACCOUNT_DESCRIPTOR} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as FormUtils from '@app/lib/forms';
import type {I18n} from '@lingui/core';
import {Trans, useLingui} from '@lingui/react/macro';
import {UserPlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface AccountSelectorProps {
	accounts: Array<Account>;
	currentAccountKey?: string | null;
	title?: React.ReactNode;
	description?: React.ReactNode;
	error?: unknown;
	disabled?: boolean;
	onSelectAccount: (account: Account) => void;
	onSignOutAccount?: ((account: Account) => Promise<void>) | null;
	onAddAccount?: () => void;
}

function resolveAccountSelectorError(i18n: I18n, error: unknown): string | null {
	if (error == null) {
		return null;
	}
	if (typeof error === 'string') {
		return error.length === 0 ? null : error;
	}
	return FormUtils.extractErrorMessage(i18n, error);
}

export const AccountSelector = observer(
	({
		accounts,
		currentAccountKey = null,
		title,
		description,
		error,
		disabled = false,
		onSelectAccount,
		onSignOutAccount = null,
		onAddAccount,
	}: AccountSelectorProps) => {
		const {i18n} = useLingui();
		const errorMessage = resolveAccountSelectorError(i18n, error);
		return (
			<div className={styles.container} data-flx="auth.accounts.account-selector.container">
				<div className={styles.header} data-flx="auth.accounts.account-selector.header">
					<h1 className={styles.title} data-flx="auth.accounts.account-selector.title">
						{title ?? <Trans>Choose an account</Trans>}
					</h1>
					<p className={styles.description} data-flx="auth.accounts.account-selector.description">
						{description ?? <Trans>Select an account to continue, or add a different one.</Trans>}
					</p>
				</div>
				<AccountList
					accounts={accounts}
					currentAccountKey={currentAccountKey}
					error={errorMessage}
					disabled={disabled}
					onSelect={onSelectAccount}
					onSignOut={onSignOutAccount}
					data-flx="auth.accounts.account-selector.account-list"
				/>
				{onAddAccount && (
					<Button
						variant="secondary"
						leftIcon={
							<UserPlusIcon
								size={remFromPx(18)}
								weight="bold"
								data-flx="auth.accounts.account-selector.user-plus-icon"
							/>
						}
						onClick={onAddAccount}
						disabled={disabled}
						fitContainer
						data-flx="auth.accounts.account-selector.button.add-account"
					>
						{i18n._(USE_ANOTHER_ACCOUNT_DESCRIPTOR)}
					</Button>
				)}
			</div>
		);
	},
);
