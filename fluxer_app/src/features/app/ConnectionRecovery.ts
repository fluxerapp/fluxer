// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accounts from '@app/features/auth/state/Accounts';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
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

export async function switchAccountFromStalledConnection(accountKey: string): Promise<void> {
	if (!canSwitchAccountFromStalledConnection()) {
		throw new StalledConnectionAccountSwitchUnavailableError();
	}
	SessionManager.requireSwitchableAccount(accountKey);
	if (SessionManager.isConnecting) {
		SessionManager.handleConnectionFailed();
	}
	await Accounts.switchToAccount(accountKey, Routes.ME);
}

export function retryStalledConnection(): void {
	const accountKey = SessionManager.currentAccountKey;
	if (accountKey === null) {
		return;
	}
	GatewaySessions.recoverForegroundSession(accountKey);
}
