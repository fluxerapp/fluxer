// SPDX-License-Identifier: AGPL-3.0-or-later

export interface FastConnectState {
	open: boolean;
	url: string;
	messages: Array<MessageEvent>;
	startedAt: number;
}

export interface FastConnectHandoff {
	ws: WebSocket;
	state: FastConnectState;
}

declare global {
	interface Window {
		__FLUXER_FAST_CONNECT__?: FastConnectHandoff | null;
	}
}

function readHandoff(): FastConnectHandoff | null {
	if (typeof window === 'undefined') return null;
	const handoff = window.__FLUXER_FAST_CONNECT__;
	if (handoff == null) return null;
	if (!(handoff.ws instanceof WebSocket)) return null;
	if (!Array.isArray(handoff.state?.messages)) return null;
	return handoff;
}

function discard(handoff: FastConnectHandoff): void {
	handoff.ws.onopen = null;
	handoff.ws.onmessage = null;
	handoff.ws.onclose = null;
	handoff.ws.onerror = null;
	try {
		handoff.ws.close(1000, 'Unused fast connect socket');
	} catch {}
}

export function takeFastConnect(expectedUrl: string): FastConnectHandoff | null {
	const handoff = readHandoff();
	if (handoff == null) return null;
	window.__FLUXER_FAST_CONNECT__ = null;
	if (handoff.state.url !== expectedUrl) {
		discard(handoff);
		return null;
	}
	const readyState = handoff.ws.readyState;
	if (readyState !== WebSocket.CONNECTING && readyState !== WebSocket.OPEN) {
		discard(handoff);
		return null;
	}
	handoff.ws.onopen = null;
	handoff.ws.onmessage = null;
	handoff.ws.onclose = null;
	handoff.ws.onerror = null;
	return handoff;
}
