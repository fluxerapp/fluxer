// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import SessionManager from '@app/features/platform/state/AuthSession';

export function retryStalledConnection(): void {
	const accountKey = SessionManager.currentAccountKey;
	if (accountKey === null) {
		return;
	}
	GatewaySessions.recoverForegroundSession(accountKey);
}
