// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';

export function isClientBooting(): boolean {
	if (DeveloperOptions.bypassLoadingSkeleton) {
		return false;
	}
	if (DeveloperOptions.forceLoadingSkeleton) {
		return true;
	}
	return !Initialization.hasCompletedInitialLoad;
}

export function isClientReconnecting(): boolean {
	if (isClientBooting()) {
		return false;
	}
	return GatewayConnection.isConnectionInterrupted || !Initialization.isReady;
}
