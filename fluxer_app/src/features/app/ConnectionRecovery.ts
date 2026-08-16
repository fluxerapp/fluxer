// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountManager from '@app/features/auth/state/AccountManager';
import SessionManager from '@app/features/platform/state/AuthSession';

class StalledConnectionAccountSwitchUnavailableError extends Error {
	constructor() {
		super('Cannot switch accounts from the current authentication session state');
		this.name = 'StalledConnectionAccountSwitchUnavailableError';
	}
}

export function canSwitchAccountFromStalledConnection(): boolean {
	return SessionManager.canSwitchAccount() || SessionManager.isConnecting;
}

export async function switchAccountFromStalledConnection(userId: string): Promise<void> {
	if (!canSwitchAccountFromStalledConnection()) {
		throw new StalledConnectionAccountSwitchUnavailableError();
	}
	SessionManager.requireAccountOnCurrentInstance(userId);
	if (SessionManager.isConnecting) {
		SessionManager.handleConnectionFailed();
	}
	await AccountManager.switchToAccount(userId, null);
}
