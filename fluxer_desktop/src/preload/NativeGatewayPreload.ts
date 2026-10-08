// SPDX-License-Identifier: AGPL-3.0-or-later

import {reconstructNativeGatewayTransportEvent} from '@electron/preload/PreloadNativeGatewayEvent';
import {
	NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG,
	NATIVE_GATEWAY_TRANSPORT_CHANNELS,
	NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL,
	type NativeGatewayTransportAPI,
	type NativeGatewayTransportCloseRequest,
	type NativeGatewayTransportCreateRequest,
	type NativeGatewayTransportCreateResult,
	type NativeGatewayTransportEvent,
	type NativeGatewayTransportSendBinaryRequest,
	type NativeGatewayTransportSendTextRequest,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';

interface NativeGatewayPreloadRenderer {
	invoke: (channel: string, ...args: Array<unknown>) => Promise<unknown>;
	on: (channel: string, listener: (event: unknown, ...args: Array<unknown>) => void) => void;
	removeListener: (channel: string, listener: (event: unknown, ...args: Array<unknown>) => void) => void;
}

interface NativeGatewayTransportPreloadAPI {
	readonly nativeGatewayTransport: NativeGatewayTransportAPI;
}

class NativeGatewayTransportUnavailableError extends Error {
	public constructor() {
		super('The native gateway transport is unavailable in this desktop shell');
		this.name = 'NativeGatewayTransportUnavailableError';
	}
}

export function createNativeGatewayPreloadAPI(
	renderer: NativeGatewayPreloadRenderer,
	argv: ReadonlyArray<string> = process.argv,
): NativeGatewayTransportPreloadAPI {
	const isAvailable = argv.includes(NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG);
	let availabilityProbe: Promise<boolean> | null = null;
	const probeAvailability = (): Promise<boolean> => {
		availabilityProbe ??= renderer
			.invoke(NATIVE_GATEWAY_TRANSPORT_CHANNELS.isAvailable)
			.then((value) => value === true)
			.catch(() => false);
		return availabilityProbe;
	};
	return Object.freeze({
		nativeGatewayTransport: Object.freeze<NativeGatewayTransportAPI>({
			isAvailable,
			create: async (request: NativeGatewayTransportCreateRequest): Promise<NativeGatewayTransportCreateResult> => {
				if (!(await probeAvailability())) {
					throw new NativeGatewayTransportUnavailableError();
				}
				return (await renderer.invoke(
					NATIVE_GATEWAY_TRANSPORT_CHANNELS.create,
					request,
				)) as NativeGatewayTransportCreateResult;
			},
			sendText: async (request: NativeGatewayTransportSendTextRequest): Promise<void> => {
				await renderer.invoke(NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendText, request);
			},
			sendBinary: async (request: NativeGatewayTransportSendBinaryRequest): Promise<void> => {
				await renderer.invoke(NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendBinary, request);
			},
			close: async (request: NativeGatewayTransportCloseRequest): Promise<void> => {
				await renderer.invoke(NATIVE_GATEWAY_TRANSPORT_CHANNELS.close, request);
			},
			dispose: async (connectionId: string): Promise<void> => {
				await renderer.invoke(NATIVE_GATEWAY_TRANSPORT_CHANNELS.dispose, connectionId);
			},
			onEvent: (callback: (event: NativeGatewayTransportEvent) => void): (() => void) => {
				const listener = (_event: unknown, payload: unknown): void => {
					let reconstructed: NativeGatewayTransportEvent;
					try {
						reconstructed = reconstructNativeGatewayTransportEvent(payload);
					} catch {
						return;
					}
					callback(reconstructed);
				};
				renderer.on(NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL, listener);
				return () => {
					renderer.removeListener(NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL, listener);
				};
			},
		}),
	});
}
