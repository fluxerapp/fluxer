// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig, {
	type RuntimeConfigSnapshot,
	runtimeConfigSnapshotsAreSameInstance,
	runtimeInstanceKey,
} from '@app/features/app/state/RuntimeConfig';
import {resolveAccountInstanceKey} from '@app/features/auth/AccountDisplayUtils';
import {getAccountKey, parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {compareSwitcherOrder, mergeSwitcherOrder} from '@app/features/auth/state/AccountSwitcherOrder';
import Accounts from '@app/features/auth/state/Accounts';
import type {Account} from '@app/features/platform/state/AuthSession';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';

function accountIsSelectableOnCurrentInstance(
	account: Account,
	currentInstance: RuntimeConfigSnapshot,
	currentInstanceKey: string,
): boolean {
	if (account.instance != null) {
		return runtimeConfigSnapshotsAreSameInstance(account.instance, currentInstance);
	}
	return parseAccountStorageKey(account.storageKey)?.instanceKey === currentInstanceKey;
}

export function currentInstanceAccounts(): Array<Account> {
	const accounts = Accounts.getAllAccounts();
	const currentInstance = RuntimeConfig.getSnapshotOrNull();
	if (currentInstance === null) {
		return [...accounts].sort(compareSwitcherOrder);
	}
	const currentInstanceKey = runtimeInstanceKey(currentInstance);
	if (currentInstanceKey === null) {
		throw new Error('Current runtime snapshot has no instance identity');
	}
	return accounts
		.filter((account) => accountIsSelectableOnCurrentInstance(account, currentInstance, currentInstanceKey))
		.sort(compareSwitcherOrder);
}

export function switcherAccounts(): Array<Account> {
	if (isDesktop()) {
		return [...Accounts.getAllAccounts()].sort(compareSwitcherOrder);
	}
	return currentInstanceAccounts();
}

export async function saveSwitcherOrder(orderedAccountKeys: ReadonlyArray<string>): Promise<void> {
	const allKeys = [...Accounts.getAllAccounts()].sort(compareSwitcherOrder).map(getAccountKey);
	await Accounts.reorderAccounts(mergeSwitcherOrder(allKeys, orderedAccountKeys));
}

export function adjacentSwitcherAccount(direction: 1 | -1): Account | null {
	const accounts = switcherAccounts();
	if (accounts.length < 2) {
		return null;
	}
	const currentAccountKey = Accounts.currentAccountKey;
	const currentIndex = accounts.findIndex((account) => getAccountKey(account) === currentAccountKey);
	if (currentIndex < 0) {
		return direction === 1 ? accounts[0] : accounts[accounts.length - 1];
	}
	return accounts[(currentIndex + direction + accounts.length) % accounts.length];
}

export function mostRecentOtherAccount(): Account | null {
	const currentAccountKey = Accounts.currentAccountKey;
	let mostRecent: Account | null = null;
	for (const account of switcherAccounts()) {
		if (getAccountKey(account) === currentAccountKey) {
			continue;
		}
		if (mostRecent == null || account.lastActive > mostRecent.lastActive) {
			mostRecent = account;
		}
	}
	return mostRecent;
}

export function accountsSpanSeveralInstances(accounts: ReadonlyArray<Account>): boolean {
	const instanceKeys = new Set<string | null>();
	for (const account of accounts) {
		instanceKeys.add(resolveAccountInstanceKey(account));
		if (instanceKeys.size > 1) {
			return true;
		}
	}
	return false;
}
