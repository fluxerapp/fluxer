// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
import Accounts from '@app/features/auth/state/Accounts';
import BootStateRestore from '@app/features/boot_state/BootStateRestore';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';

export function isClientBooting(): boolean {
	if (DeveloperOptions.bypassLoadingSkeleton) {
		return false;
	}
	if (DeveloperOptions.forceLoadingSkeleton) {
		return true;
	}
	return !Initialization.hasCompletedInitialLoad && !Initialization.isStale;
}

export function isClientReconnecting(): boolean {
	if (isClientBooting() || Accounts.transitioning || BootStateRestore.staleGraceActive) {
		return false;
	}
	return GatewaySessions.foregroundConnection.isConnectionInterrupted || !Initialization.isReady;
}
