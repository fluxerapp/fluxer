// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstancePremiumMode} from '@app/api/instance/InstanceConfigRepository';

let cachedPremiumMode: InstancePremiumMode = 'everyone';

export function getCachedInstancePremiumMode(): InstancePremiumMode {
	return cachedPremiumMode;
}

export function setCachedInstancePremiumMode(mode: InstancePremiumMode): void {
	cachedPremiumMode = mode;
}
