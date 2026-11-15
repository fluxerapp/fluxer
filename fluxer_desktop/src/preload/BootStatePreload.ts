// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DESKTOP_BOOT_STATE_CHANNELS,
	type DesktopBootStateAPI,
	type DesktopBootStateFlushOptions,
	type DesktopBootStateFrameRequest,
	type DesktopBootStateFrameResult,
	type DesktopBootStateReadResult,
	type DesktopBootStateWriteRequest,
	type DesktopBootStateWriteResult,
} from '@fluxer/desktop_ipc/src/BootStateContract';

interface BootStateRenderer {
	invoke(channel: string, ...args: Array<unknown>): Promise<unknown>;
	on(channel: string, listener: (event: unknown, ...args: Array<unknown>) => void): unknown;
}

type FlushHandler = (options: DesktopBootStateFlushOptions) => Promise<void>;

export function createBootStatePreloadAPI(renderer: BootStateRenderer): {readonly bootState: DesktopBootStateAPI} {
	const handlers = new Set<FlushHandler>();
	renderer.on(DESKTOP_BOOT_STATE_CHANNELS.flushRequested, (_event, id, options) => {
		const flush: DesktopBootStateFlushOptions = {
			frame: (options as Partial<DesktopBootStateFlushOptions> | undefined)?.frame !== false,
		};
		void Promise.allSettled([...handlers].map((handler) => Promise.resolve().then(() => handler(flush))))
			.then(() => renderer.invoke(DESKTOP_BOOT_STATE_CHANNELS.flushed, id))
			.catch(() => undefined);
	});
	return Object.freeze({
		bootState: Object.freeze<DesktopBootStateAPI>({
			read: (accountKey: string) =>
				renderer.invoke(DESKTOP_BOOT_STATE_CHANNELS.read, accountKey) as Promise<DesktopBootStateReadResult>,
			issueWriteToken: (accountKey: string) =>
				renderer.invoke(DESKTOP_BOOT_STATE_CHANNELS.issueWriteToken, accountKey) as Promise<string | null>,
			write: (request: DesktopBootStateWriteRequest) =>
				renderer.invoke(DESKTOP_BOOT_STATE_CHANNELS.write, {
					accountKey: request.accountKey,
					writeToken: request.writeToken,
					payload: request.payload,
					frameId: request.frameId,
				}) as Promise<DesktopBootStateWriteResult>,
			clear: async (accountKey: string) => {
				await renderer.invoke(DESKTOP_BOOT_STATE_CHANNELS.clear, accountKey);
			},
			captureFrame: (request: DesktopBootStateFrameRequest) =>
				renderer.invoke(DESKTOP_BOOT_STATE_CHANNELS.captureFrame, {
					accountKey: request.accountKey,
					writeToken: request.writeToken,
					captureAppCheck: request.captureAppCheck,
					systemDark: request.systemDark,
				}) as Promise<DesktopBootStateFrameResult>,
			onFlushRequested: (handler: FlushHandler) => {
				handlers.add(handler);
				return () => {
					handlers.delete(handler);
				};
			},
		}),
	});
}
