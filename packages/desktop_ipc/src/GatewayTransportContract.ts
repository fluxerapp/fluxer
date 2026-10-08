// SPDX-License-Identifier: AGPL-3.0-or-later

export const NativeGatewayTransportMode = Object.freeze({
	GATEWAY: 'gateway',
	VOICE: 'voice',
} as const);

export type NativeGatewayTransportMode = (typeof NativeGatewayTransportMode)[keyof typeof NativeGatewayTransportMode];

export const NativeGatewayTransportEventKind = Object.freeze({
	OPEN: 'open',
	MESSAGE: 'message',
	BINARY: 'binary',
	CLOSE: 'close',
	ERROR: 'error',
} as const);

export type NativeGatewayTransportEventKind =
	(typeof NativeGatewayTransportEventKind)[keyof typeof NativeGatewayTransportEventKind];

export interface NativeGatewayTransportEvent {
	readonly connectionId: string;
	readonly kind: NativeGatewayTransportEventKind;
	readonly data: string | null;
	readonly binary: ArrayBuffer | null;
	readonly code: number | null;
	readonly reason: string | null;
	readonly wasClean: boolean | null;
	readonly message: string | null;
}

export interface NativeGatewayTransportCreateRequest {
	readonly connectionId: string;
	readonly url: string;
	readonly mode: NativeGatewayTransportMode;
}

export interface NativeGatewayTransportCreateResult {
	readonly connectionId: string;
}

export interface NativeGatewayTransportSendTextRequest {
	readonly connectionId: string;
	readonly payload: string;
}

export interface NativeGatewayTransportSendBinaryRequest {
	readonly connectionId: string;
	readonly payload: ArrayBuffer;
}

export interface NativeGatewayTransportCloseRequest {
	readonly connectionId: string;
	readonly code: number;
	readonly reason: string;
}

export interface NativeGatewayTransportAPI {
	readonly isAvailable: boolean;
	create: (request: NativeGatewayTransportCreateRequest) => Promise<NativeGatewayTransportCreateResult>;
	sendText: (request: NativeGatewayTransportSendTextRequest) => Promise<void>;
	sendBinary: (request: NativeGatewayTransportSendBinaryRequest) => Promise<void>;
	close: (request: NativeGatewayTransportCloseRequest) => Promise<void>;
	dispose: (connectionId: string) => Promise<void>;
	onEvent: (callback: (event: NativeGatewayTransportEvent) => void) => () => void;
}

export const NATIVE_GATEWAY_TRANSPORT_CHANNELS = Object.freeze({
	create: 'native-gateway-transport:create',
	sendText: 'native-gateway-transport:send-text',
	sendBinary: 'native-gateway-transport:send-binary',
	close: 'native-gateway-transport:close',
	dispose: 'native-gateway-transport:dispose',
	isAvailable: 'native-gateway-transport:is-available',
} as const);

export const NATIVE_GATEWAY_TRANSPORT_PROXIED_MESSAGE =
	'The native gateway transport cannot reach a gateway that resolves through a proxy';

export const NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL = 'native-gateway-transport:event';

export const NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG = '--fluxer-native-gateway-available=1';

export const NATIVE_GATEWAY_TRANSPORT_CONNECTION_ID_MAX_BYTES = 192;
export const NATIVE_GATEWAY_TRANSPORT_URL_MAX_BYTES = 2048;
export const NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES = 8 * 1024 * 1024;
export const NATIVE_GATEWAY_TRANSPORT_ERROR_MAX_BYTES = 16 * 1024;
export const NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES = 123;
export const NATIVE_GATEWAY_TRANSPORT_MAX_CONNECTIONS_PER_DOCUMENT = 8;

const NATIVE_GATEWAY_TRANSPORT_CONNECTION_ID_PATTERN =
	/^gateway-renderer-[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u;

export function isNativeGatewayTransportConnectionId(value: string): boolean {
	return NATIVE_GATEWAY_TRANSPORT_CONNECTION_ID_PATTERN.test(value);
}
