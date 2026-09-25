// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isPasskeyBridgeReady,
	PASSKEY_BRIDGE_PATH,
	PASSKEY_BRIDGE_REQUEST_TYPE,
	PASSKEY_BRIDGE_TIMEOUT_MS,
	PASSKEY_BRIDGE_VERSION,
	type PasskeyBridgeKind,
	type PasskeyBridgeOptionsMap,
	type PasskeyBridgeRequest,
	type PasskeyBridgeResponseMap,
	parsePasskeyBridgeResult,
	resolvePasskeyBridgeLegacyOrigin,
} from '@app/features/auth/utils/PasskeyBridgeProtocol';
import {PasskeyDomainUnsupportedError} from '@app/features/auth/utils/WebAuthnUtils';
import {Platform} from '@app/features/platform/types/Platform';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const PASSKEY_BRIDGE_POPUP_FEATURES = 'popup,width=460,height=620';
const PASSKEY_BRIDGE_CLOSED_POLL_MS = 500;
const SUGGEST_BRIDGE_ERROR_NAMES: ReadonlySet<string> = new Set(['NotAllowedError', 'SecurityError']);

const PASSKEY_BRIDGE_POPUP_BLOCKED_DESCRIPTOR = msg({
	message: 'Your browser blocked the passkey pop-up window. Allow pop-ups for this site and try again.',
	comment: 'Error shown when the browser blocks the pop-up window used for password manager passkeys.',
});
const PASSKEY_BRIDGE_FAILED_DESCRIPTOR = msg({
	message: "Couldn't use your passkey in the pop-up window. Try again.",
	comment: 'Error shown when the pop-up window used for password manager passkeys does not finish.',
});

export class PasskeyBridgeError extends Error {
	constructor(name: string, message: string) {
		super(message);
		this.name = name;
	}
}

export function isPasskeyBridgeAvailable(): boolean {
	return !Platform.isElectron && resolvePasskeyBridgeLegacyOrigin(window.location.origin) !== null;
}

export function shouldSuggestPasskeyBridge(error: unknown): boolean {
	if (!isPasskeyBridgeAvailable() || error instanceof PasskeyBridgeError) {
		return false;
	}
	return (
		error instanceof PasskeyDomainUnsupportedError ||
		(error instanceof Error && SUGGEST_BRIDGE_ERROR_NAMES.has(error.name))
	);
}

function isPasskeyBridgeDismissal(error: unknown): boolean {
	return error instanceof PasskeyBridgeError && (error.name === 'AbortError' || error.name === 'NotAllowedError');
}

export function describePasskeyBridgeFailure(error: unknown): MessageDescriptor | null {
	if (isPasskeyBridgeDismissal(error)) {
		return null;
	}
	if (error instanceof PasskeyBridgeError && error.name === 'PopupBlockedError') {
		return PASSKEY_BRIDGE_POPUP_BLOCKED_DESCRIPTOR;
	}
	return PASSKEY_BRIDGE_FAILED_DESCRIPTOR;
}

export function runPasskeyViaBridge<K extends PasskeyBridgeKind>(
	kind: K,
	options: Promise<PasskeyBridgeOptionsMap[K]>,
): Promise<PasskeyBridgeResponseMap[K]> {
	const legacyOrigin = isPasskeyBridgeAvailable() ? resolvePasskeyBridgeLegacyOrigin(window.location.origin) : null;
	const popup =
		legacyOrigin === null
			? null
			: window.open(`${legacyOrigin}${PASSKEY_BRIDGE_PATH}`, '_blank', PASSKEY_BRIDGE_POPUP_FEATURES);
	if (legacyOrigin === null || popup === null) {
		options.catch(() => {});
		const name = legacyOrigin === null ? 'NotSupportedError' : 'PopupBlockedError';
		return Promise.reject(new PasskeyBridgeError(name, 'The passkey window could not be opened'));
	}
	return new Promise((resolve, reject) => {
		const id = crypto.randomUUID();
		let ready = false;
		let settled = false;
		let requestSent = false;
		let closedSeen = false;
		let resolvedOptions: PasskeyBridgeOptionsMap[K] | null = null;
		const cleanup = () => {
			window.removeEventListener('message', handleMessage);
			window.clearInterval(closedPoll);
			window.clearTimeout(timeout);
		};
		const fail = (error: unknown) => {
			if (settled) {
				return;
			}
			settled = true;
			cleanup();
			if (!popup.closed) {
				popup.close();
			}
			reject(error);
		};
		const sendRequest = () => {
			if (!ready || resolvedOptions === null || requestSent || settled) {
				return;
			}
			requestSent = true;
			const request = {
				type: PASSKEY_BRIDGE_REQUEST_TYPE,
				v: PASSKEY_BRIDGE_VERSION,
				id,
				kind,
				options: resolvedOptions,
			} as PasskeyBridgeRequest;
			popup.postMessage(request, legacyOrigin);
			popup.focus();
		};
		const handleMessage = (event: MessageEvent) => {
			if (settled || event.origin !== legacyOrigin || event.source !== popup) {
				return;
			}
			if (isPasskeyBridgeReady(event.data)) {
				ready = true;
				sendRequest();
				return;
			}
			const result = parsePasskeyBridgeResult(event.data, id);
			if (result === null) {
				return;
			}
			if (!result.ok) {
				fail(new PasskeyBridgeError(result.error.name, result.error.message));
				return;
			}
			settled = true;
			cleanup();
			resolve(result.response as PasskeyBridgeResponseMap[K]);
		};
		const closedPoll = window.setInterval(() => {
			if (!popup.closed) {
				return;
			}
			if (closedSeen) {
				fail(new PasskeyBridgeError('AbortError', 'The passkey window was closed'));
			}
			closedSeen = true;
		}, PASSKEY_BRIDGE_CLOSED_POLL_MS);
		const timeout = window.setTimeout(() => {
			fail(new PasskeyBridgeError('TimeoutError', 'The passkey window timed out'));
		}, PASSKEY_BRIDGE_TIMEOUT_MS);
		window.addEventListener('message', handleMessage);
		options.then(
			(value) => {
				resolvedOptions = value;
				sendRequest();
			},
			(error: unknown) => fail(error),
		);
	});
}
