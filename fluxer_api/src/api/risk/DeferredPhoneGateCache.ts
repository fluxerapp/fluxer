// SPDX-License-Identifier: AGPL-3.0-or-later

let cachedEnabled = false;

export function resolveDeferredPhoneGateEnabled(policy: {
	deferred_phone_gate_enabled: boolean;
	single_community_enabled: boolean;
}): boolean {
	return policy.deferred_phone_gate_enabled && !policy.single_community_enabled;
}

export function getCachedDeferredPhoneGateEnabled(): boolean {
	return cachedEnabled;
}

export function setCachedDeferredPhoneGateEnabled(enabled: boolean): void {
	cachedEnabled = enabled;
}
