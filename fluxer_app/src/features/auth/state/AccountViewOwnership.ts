// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import SessionManager from '@app/features/platform/state/AuthSession';

export function accountOwnsActiveView(accountKey: string | null): boolean {
	return !AccountScopedWork.isSuspended && SessionManager.currentAccountKey === accountKey;
}
