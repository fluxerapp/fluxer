// SPDX-License-Identifier: AGPL-3.0-or-later

// Null until a policy has actually been read. A deferral is only ever granted
// while the gate is on, so reading "not known yet" as "off" revokes it from every
// account holding one and answers their next request with the phone requirement
// the deferral exists to hold back.
let cachedEnabled: boolean | null = null;

export function resolveDeferredPhoneGateEnabled(policy: {
	deferred_phone_gate_enabled: boolean;
	single_community_enabled: boolean;
}): boolean {
	return policy.deferred_phone_gate_enabled && !policy.single_community_enabled;
}

export function getCachedDeferredPhoneGateEnabled(): boolean | null {
	return cachedEnabled;
}

export function setCachedDeferredPhoneGateEnabled(enabled: boolean): void {
	cachedEnabled = enabled;
}
