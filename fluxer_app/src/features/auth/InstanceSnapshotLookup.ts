// SPDX-License-Identifier: AGPL-3.0-or-later

import InstanceSnapshotStore from '@app/features/app/state/InstanceSnapshotStore';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {resolveAccountInstanceKey} from '@app/features/auth/AccountDisplayUtils';
import Accounts from '@app/features/auth/state/Accounts';

export function findInstanceSnapshot(instanceKey: string): RuntimeConfigSnapshot | null {
	const cached = InstanceSnapshotStore.get(instanceKey);
	if (cached != null) {
		return cached;
	}
	for (const account of Accounts.orderedAccounts) {
		if (account.instance != null && resolveAccountInstanceKey(account) === instanceKey) {
			return account.instance;
		}
	}
	return null;
}
