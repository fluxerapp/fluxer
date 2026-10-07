// SPDX-License-Identifier: AGPL-3.0-or-later

export type NativeGatewayEventKind = 'open' | 'message' | 'binary' | 'error' | 'close';

export interface NativeGatewayEvent {
	kind: NativeGatewayEventKind;
	data: string | null;
	binary: Buffer | null;
	code: number | null;
	reason: string | null;
	wasClean: boolean | null;
	message: string | null;
}

export interface NativeGatewayConnectOptions {
	url: string;
	address: string | null;
	mode: 'gateway';
}

export interface NativeGatewayConnection {
	sendText(text: string): void;
	sendBinary(payload: Buffer): void;
	close(code: number, reason: string): void;
	dispose(): void;
}

export declare const connect:
	| ((
			options: NativeGatewayConnectOptions,
			onEvent: (event: NativeGatewayEvent) => void,
			onTerminalEvent: (event: NativeGatewayEvent) => void,
	  ) => NativeGatewayConnection)
	| null;
export declare const warmup: (() => void) | null;
export declare const loadError: Error | null;
