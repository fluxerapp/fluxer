// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig, {
	type RuntimeConfigSnapshot,
	runtimeConfigSnapshotsAreSameInstance,
	runtimeInstanceKey,
} from '@app/features/app/state/RuntimeConfig';
import {parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
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

export function switcherAccounts(): Array<Account> {
	const accounts = Accounts.getAllAccounts();
	if (isDesktop()) {
		return accounts;
	}
	const currentInstance = RuntimeConfig.getSnapshotOrNull();
	if (currentInstance === null) {
		return accounts;
	}
	const currentInstanceKey = runtimeInstanceKey(currentInstance);
	if (currentInstanceKey === null) {
		throw new Error('Current runtime snapshot has no instance identity');
	}
	return accounts.filter((account) =>
		accountIsSelectableOnCurrentInstance(account, currentInstance, currentInstanceKey),
	);
}
