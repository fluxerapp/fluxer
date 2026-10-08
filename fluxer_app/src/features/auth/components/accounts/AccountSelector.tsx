// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountRow} from '@app/features/auth/components/accounts/AccountRow';
import styles from '@app/features/auth/components/accounts/AccountSelector.module.css';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import type {Account} from '@app/features/platform/state/AuthSession';
import {Button} from '@app/features/ui/button/Button';
import {Scroller} from '@app/features/ui/components/Scroller';
import * as FormUtils from '@app/lib/forms';
import type {I18n} from '@lingui/core';
import {Trans, useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface AccountSelectorProps {
	accounts: Array<Account>;
	currentAccountKey?: string | null;
	title?: React.ReactNode;
	description?: React.ReactNode;
	error?: unknown;
	disabled?: boolean;
	addButtonLabel?: React.ReactNode;
	onSelectAccount: (account: Account) => void;
	onAddAccount?: () => void;
	scrollerKey?: string;
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
		currentAccountKey,
		title,
		description,
		error,
		disabled = false,
		addButtonLabel,
		onSelectAccount,
		onAddAccount,
		scrollerKey,
	}: AccountSelectorProps) => {
		const {i18n} = useLingui();
		const defaultTitle = <Trans>Choose an account</Trans>;
		const defaultDescription = <Trans>Select an account to continue, or add a different one.</Trans>;
		const errorMessage = resolveAccountSelectorError(i18n, error);
		return (
			<div className={styles.container} aria-busy={disabled} data-flx="auth.accounts.account-selector.container">
				<h1 className={styles.title} data-flx="auth.accounts.account-selector.title">
					{title ?? defaultTitle}
				</h1>
				<p className={styles.description} data-flx="auth.accounts.account-selector.description">
					{description ?? defaultDescription}
				</p>
				{errorMessage != null && (
					<div className={styles.error} role="alert" data-flx="auth.accounts.account-selector.error">
						{errorMessage}
					</div>
				)}
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
								const accountKey = getAccountKey(account);
								return (
									<AccountRow
										key={accountKey}
										account={account}
										isCurrent={accountKey === currentAccountKey}
										isExpired={account.isValid === false}
										onClick={() => onSelectAccount(account)}
										disabled={disabled}
										showCaretIndicator
										data-flx="auth.accounts.account-selector.account-row.select-account"
									/>
								);
							})}
						</div>
					</Scroller>
				)}
				{onAddAccount && (
					<Button
						variant="secondary"
						leftIcon={<PlusIcon size={18} weight="bold" data-flx="auth.accounts.account-selector.plus-icon" />}
						onClick={onAddAccount}
						disabled={disabled}
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
