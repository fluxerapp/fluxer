// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAccountDisplayLabels, resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import Accounts from '@app/features/auth/state/Accounts';
import Users from '@app/features/user/state/Users';

export function useCurrentAccountLabels(): {username: string; instanceName: string} {
	const account = Accounts.currentAccount;
	const labels = account == null ? null : getAccountDisplayLabels(account);
	return {
		username: labels?.available ? labels.usernameLabel : (Users.currentUser?.username ?? ''),
		instanceName: (account == null ? null : resolveAccountInstanceLabel(account)) ?? '',
	};
}
