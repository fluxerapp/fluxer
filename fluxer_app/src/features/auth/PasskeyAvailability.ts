// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';

export const DESKTOP_PASSKEY_RELYING_PARTIES: ReadonlyArray<string> = Object.freeze(['fluxer.app']);

function hostFromEndpoint(endpoint: string | undefined): string | null {
	if (endpoint == null || endpoint.length === 0) {
		return null;
	}
	try {
		return new URL(endpoint).hostname.toLowerCase();
	} catch {
		return null;
	}
}

export function resolvePasskeyRelyingPartyId(snapshot: RuntimeConfigSnapshot | null | undefined): string | null {
	const host = hostFromEndpoint(snapshot?.webAppEndpoint);
	if (host == null) {
		return null;
	}
	const labels = host.split('.');
	if (labels.length < 2) {
		return null;
	}
	return labels.slice(-2).join('.');
}

export function isRelyingPartyAssertableFromHost(relyingPartyId: string, host: string): boolean {
	const rp = relyingPartyId.toLowerCase();
	const origin = host.toLowerCase();
	return origin === rp || origin.endsWith(`.${rp}`);
}

export interface PasskeyAvailabilityRequest {
	readonly snapshot: RuntimeConfigSnapshot | null | undefined;
	readonly currentHost: string;
	readonly desktop: boolean;
}

export function isPasskeyAvailableForInstance({snapshot, currentHost, desktop}: PasskeyAvailabilityRequest): boolean {
	const relyingPartyId = resolvePasskeyRelyingPartyId(snapshot);
	if (relyingPartyId == null) {
		return false;
	}
	if (desktop) {
		return DESKTOP_PASSKEY_RELYING_PARTIES.includes(relyingPartyId);
	}
	return isRelyingPartyAssertableFromHost(relyingPartyId, currentHost);
}

export function isPasskeyAvailableForCurrentClient(snapshot: RuntimeConfigSnapshot | null | undefined): boolean {
	let currentHost = '';
	if (globalThis.window != null) {
		currentHost = window.location.hostname;
	}
	return isPasskeyAvailableForInstance({snapshot, currentHost, desktop: isDesktop()});
}
