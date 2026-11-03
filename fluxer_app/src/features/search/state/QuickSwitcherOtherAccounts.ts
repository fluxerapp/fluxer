// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import {
	ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR,
	getAccountDisplayLabels,
	resolveAccountInstanceKey,
	resolveAccountInstanceLabel,
} from '@app/features/auth/AccountDisplayUtils';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {switcherAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import type {Account} from '@app/features/platform/state/AuthSession';
import type {OtherAccountInfo, OtherAccountsContext} from '@app/features/search/state/QuickSwitcherAccountResults';
import type {AccountEngineAccountActivity} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {I18n} from '@lingui/core';

export function accountsSpanSeveralInstances(accounts: ReadonlyArray<Account>): boolean {
	const instanceKeys = new Set<string>();
	for (const account of accounts) {
		const instanceKey = resolveAccountInstanceKey(account);
		if (instanceKey != null) instanceKeys.add(instanceKey);
		if (instanceKeys.size > 1) return true;
	}
	return false;
}

export function otherAccountDisplayName(account: Account, i18n: I18n): string {
	const labels = getAccountDisplayLabels(account);
	if (labels.available) return labels.displayLabel;
	const user = AccountEngine.getSummary(getAccountKey(account))?.user ?? null;
	if (user != null) return user.globalName ?? user.username;
	return i18n._(ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR);
}

function toOtherAccountInfo(account: Account, i18n: I18n): OtherAccountInfo {
	const accountKey = getAccountKey(account);
	const labels = getAccountDisplayLabels(account);
	const summary = AccountEngine.getSummary(accountKey);
	return {
		accountKey,
		displayName: otherAccountDisplayName(account, i18n),
		username: labels.available ? labels.usernameLabel : (summary?.user?.username ?? null),
		host: resolveAccountInstanceLabel(account),
		isExpired: account.isValid === false,
		mentionCount: AccountEngine.getMentionCount(accountKey),
		hasUnread: AccountEngine.hasUnread(accountKey),
		lastActivityAt: summary?.lastMessageAt ?? null,
	};
}

export function buildOtherAccountsContext(
	activity: ReadonlyMap<string, AccountEngineAccountActivity>,
	i18n: I18n,
): OtherAccountsContext {
	const accounts = switcherAccounts();
	const currentAccountKey = Accounts.currentAccountKey;
	return {
		accounts: accounts
			.filter((account) => getAccountKey(account) !== currentAccountKey)
			.map((account) => toOtherAccountInfo(account, i18n)),
		activity,
		showHost: accountsSpanSeveralInstances(accounts),
	};
}
