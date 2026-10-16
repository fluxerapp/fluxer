// SPDX-License-Identifier: AGPL-3.0-or-later

import accountStorage from '@app/features/auth/state/AccountStorage';
import type {UserData} from '@app/features/auth/state/AccountStorageContract';
import Accounts from '@app/features/auth/state/Accounts';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('AccountUserDataSync');

export interface AccountUserIdentity {
	readonly username: string;
	readonly discriminator: string;
	readonly global_name?: string | null;
	readonly email?: string | null;
	readonly avatar?: string | null;
}

export function syncAccountUserData(accountKey: string, user: AccountUserIdentity): void {
	const previous = Accounts.getAccount(accountKey)?.userData;
	const userData: UserData = {
		username: user.username,
		discriminator: user.discriminator,
		globalName: user.global_name === undefined ? previous?.globalName : user.global_name,
		email: user.email === undefined ? previous?.email : (user.email ?? undefined),
		avatar: user.avatar ?? undefined,
	};
	void accountStorage.updateAccountUserData(accountKey, userData).catch((error) => {
		logger.warn(`Failed to persist account user data for ${accountKey}`, error);
	});
	Accounts.updateAccountUserData(accountKey, userData);
}
