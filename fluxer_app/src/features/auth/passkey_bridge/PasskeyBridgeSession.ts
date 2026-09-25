// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PASSKEY_BRIDGE_READY_TYPE,
	PASSKEY_BRIDGE_RESULT_TYPE,
	PASSKEY_BRIDGE_TIMEOUT_MS,
	PASSKEY_BRIDGE_VERSION,
	type PasskeyBridgeKind,
	type PasskeyBridgeRequest,
	type PasskeyBridgeResponseMap,
	type PasskeyBridgeResult,
	parsePasskeyBridgeRequest,
	readPasskeyBridgeRequestId,
	toPasskeyBridgeErrorPayload,
} from '@app/features/auth/utils/PasskeyBridgeProtocol';
import {startAuthentication, startRegistration} from '@simplewebauthn/browser';

export type PasskeyBridgeViewState =
	| {status: 'unavailable'}
	| {status: 'waiting'}
	| {status: 'ready'; kind: PasskeyBridgeKind}
	| {status: 'running'; kind: PasskeyBridgeKind}
	| {status: 'rejected'}
	| {status: 'timed_out'};

export interface PasskeyBridgeSession {
	continueCeremony(): void;
	dispose(): void;
}

function runCeremony(request: PasskeyBridgeRequest): Promise<PasskeyBridgeResponseMap[PasskeyBridgeKind]> {
	if (request.kind === 'authenticate') {
		return startAuthentication({optionsJSON: request.options});
	}
	return startRegistration({optionsJSON: request.options});
}

function failureResult(id: string, error: unknown): PasskeyBridgeResult {
	return {
		type: PASSKEY_BRIDGE_RESULT_TYPE,
		v: PASSKEY_BRIDGE_VERSION,
		id,
		ok: false,
		error: toPasskeyBridgeErrorPayload(error),
	};
}

export function startPasskeyBridgeSession(
	openerOrigin: string,
	onStateChange: (state: PasskeyBridgeViewState) => void,
): PasskeyBridgeSession {
	const opener = window.opener as Window | null;
	if (opener === null || opener === window) {
		onStateChange({status: 'unavailable'});
		return {continueCeremony() {}, dispose() {}};
	}
	let request: PasskeyBridgeRequest | null = null;
	let running = false;
	let finished = false;
	const post = (message: unknown) => {
		opener.postMessage(message, openerOrigin);
	};
	const finish = (result: PasskeyBridgeResult) => {
		finished = true;
		window.clearTimeout(timeout);
		post(result);
		window.close();
	};
	const handleMessage = (event: MessageEvent) => {
		if (event.origin !== openerOrigin || event.source !== opener) {
			return;
		}
		window.removeEventListener('message', handleMessage);
		const parsed = parsePasskeyBridgeRequest(event.data);
		if (parsed === null) {
			finished = true;
			window.clearTimeout(timeout);
			const id = readPasskeyBridgeRequestId(event.data);
			if (id !== null) {
				post(failureResult(id, new DOMException('The passkey request was rejected', 'SecurityError')));
			}
			onStateChange({status: 'rejected'});
			return;
		}
		request = parsed;
		onStateChange({status: 'ready', kind: parsed.kind});
	};
	const timeout = window.setTimeout(() => {
		if (running || finished) {
			return;
		}
		finished = true;
		window.removeEventListener('message', handleMessage);
		if (request !== null) {
			post(failureResult(request.id, new DOMException('The passkey window timed out', 'TimeoutError')));
			request = null;
		}
		onStateChange({status: 'timed_out'});
	}, PASSKEY_BRIDGE_TIMEOUT_MS);
	window.addEventListener('message', handleMessage);
	onStateChange({status: 'waiting'});
	post({type: PASSKEY_BRIDGE_READY_TYPE, v: PASSKEY_BRIDGE_VERSION});
	return {
		continueCeremony() {
			const current = request;
			if (current === null || running || finished) {
				return;
			}
			running = true;
			onStateChange({status: 'running', kind: current.kind});
			runCeremony(current).then(
				(response) => {
					finish({type: PASSKEY_BRIDGE_RESULT_TYPE, v: PASSKEY_BRIDGE_VERSION, id: current.id, ok: true, response});
				},
				(error: unknown) => {
					finish(failureResult(current.id, error));
				},
			);
		},
		dispose() {
			window.removeEventListener('message', handleMessage);
			window.clearTimeout(timeout);
		},
	};
}
