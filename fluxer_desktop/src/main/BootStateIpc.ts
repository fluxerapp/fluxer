// SPDX-License-Identifier: AGPL-3.0-or-later

import {readExactPlainRecord} from '@electron/common/PlainRecord';
import {isLocalAppRendererDocumentURL} from '@electron/main/LocalAppURL';
import {
	BOOT_FRAME_ID_PATTERN,
	BOOT_STATE_MAX_PAYLOAD_BYTES,
	DESKTOP_BOOT_STATE_CHANNELS,
	type DesktopBootFrame,
	type DesktopBootStateFrameRequest,
	type DesktopBootStateFrameResult,
	type DesktopBootStateReadResult,
	type DesktopBootStateWriteRequest,
	type DesktopBootStateWriteResult,
} from '@fluxer/desktop_ipc/src/BootStateContract';
import {type IpcMainEvent, type IpcMainInvokeEvent, ipcMain} from 'electron';

const ACCOUNT_KEY_MAX_LENGTH = 512;
const WRITE_TOKEN_MAX_LENGTH = 128;
const WRITE_REQUEST_KEYS: ReadonlyArray<string> = ['accountKey', 'writeToken', 'payload', 'frameId'];
const FRAME_REQUEST_KEYS: ReadonlyArray<string> = ['accountKey', 'writeToken', 'captureAppCheck', 'systemDark'];

interface BootStateIpcRoutes {
	readonly isPrivilegedRendererDocumentSender: (event: IpcMainInvokeEvent | IpcMainEvent) => boolean;
	readonly isAppDocumentWindowContents: (contents: Electron.WebContents) => boolean;
	readonly read: (accountKey: string) => Promise<DesktopBootStateReadResult>;
	readonly issueWriteToken: (accountKey: string) => Promise<string | null>;
	readonly write: (request: DesktopBootStateWriteRequest) => Promise<DesktopBootStateWriteResult>;
	readonly clear: (accountKey: string) => Promise<void>;
	readonly flushed: (sender: Electron.WebContents, id: number) => void;
	readonly captureFrame: (
		sender: Electron.WebContents,
		request: DesktopBootStateFrameRequest,
	) => Promise<DesktopBootStateFrameResult>;
	readonly takeFrame: (sender: Electron.WebContents) => Promise<DesktopBootFrame | null>;
}

class BootStateIpcError extends Error {
	public constructor(channel: string, reason: string) {
		super(`${channel}: ${reason}`);
		this.name = 'BootStateIpcError';
	}
}

let registered = false;

function requireMainAppDocument(
	event: IpcMainInvokeEvent | IpcMainEvent,
	channel: string,
	routes: BootStateIpcRoutes,
): void {
	const frame = event.senderFrame;
	if (
		frame == null ||
		!routes.isPrivilegedRendererDocumentSender(event) ||
		!routes.isAppDocumentWindowContents(event.sender) ||
		!isLocalAppRendererDocumentURL(frame.url)
	) {
		throw new BootStateIpcError(channel, 'only the main window app document may use the boot state');
	}
}

function requireAccountKey(value: unknown, channel: string): string {
	if (typeof value !== 'string' || value.length === 0 || value.length > ACCOUNT_KEY_MAX_LENGTH) {
		throw new BootStateIpcError(channel, 'invalid account key');
	}
	return value;
}

function readFrameRequest(value: unknown): DesktopBootStateFrameRequest {
	const channel = DESKTOP_BOOT_STATE_CHANNELS.captureFrame;
	const record = readExactPlainRecord({value, expectedKeys: FRAME_REQUEST_KEYS});
	if (record === null) {
		throw new BootStateIpcError(channel, 'invalid frame request');
	}
	const {writeToken, captureAppCheck, systemDark} = record;
	if (typeof writeToken !== 'string' || writeToken.length > WRITE_TOKEN_MAX_LENGTH) {
		throw new BootStateIpcError(channel, 'invalid write token');
	}
	if (typeof captureAppCheck !== 'boolean') {
		throw new BootStateIpcError(channel, 'invalid capture app check');
	}
	if (systemDark !== null && typeof systemDark !== 'boolean') {
		throw new BootStateIpcError(channel, 'invalid system appearance');
	}
	return {accountKey: requireAccountKey(record.accountKey, channel), writeToken, captureAppCheck, systemDark};
}

function readWriteRequest(value: unknown): DesktopBootStateWriteRequest | 'too-large' {
	const channel = DESKTOP_BOOT_STATE_CHANNELS.write;
	const record = readExactPlainRecord({value, expectedKeys: WRITE_REQUEST_KEYS});
	if (record === null) {
		throw new BootStateIpcError(channel, 'invalid write request');
	}
	const {writeToken, payload, frameId} = record;
	if (typeof writeToken !== 'string' || writeToken.length > WRITE_TOKEN_MAX_LENGTH) {
		throw new BootStateIpcError(channel, 'invalid write token');
	}
	if (frameId !== null && (typeof frameId !== 'string' || !BOOT_FRAME_ID_PATTERN.test(frameId))) {
		throw new BootStateIpcError(channel, 'invalid frame id');
	}
	if (!(payload instanceof Uint8Array) || payload.byteLength === 0) {
		throw new BootStateIpcError(channel, 'invalid payload');
	}
	if (payload.byteLength > BOOT_STATE_MAX_PAYLOAD_BYTES) {
		return 'too-large';
	}
	return {accountKey: requireAccountKey(record.accountKey, channel), writeToken, payload, frameId};
}

export function registerBootStateIpcRoutes(routes: BootStateIpcRoutes): void {
	if (registered) {
		return;
	}
	registered = true;
	ipcMain.handle(DESKTOP_BOOT_STATE_CHANNELS.read, (event, accountKey: unknown) => {
		requireMainAppDocument(event, DESKTOP_BOOT_STATE_CHANNELS.read, routes);
		return routes.read(requireAccountKey(accountKey, DESKTOP_BOOT_STATE_CHANNELS.read));
	});
	ipcMain.handle(DESKTOP_BOOT_STATE_CHANNELS.issueWriteToken, (event, accountKey: unknown) => {
		requireMainAppDocument(event, DESKTOP_BOOT_STATE_CHANNELS.issueWriteToken, routes);
		return routes.issueWriteToken(requireAccountKey(accountKey, DESKTOP_BOOT_STATE_CHANNELS.issueWriteToken));
	});
	ipcMain.handle(DESKTOP_BOOT_STATE_CHANNELS.write, (event, request: unknown) => {
		requireMainAppDocument(event, DESKTOP_BOOT_STATE_CHANNELS.write, routes);
		const validated = readWriteRequest(request);
		return validated === 'too-large' ? validated : routes.write(validated);
	});
	ipcMain.handle(DESKTOP_BOOT_STATE_CHANNELS.clear, (event, accountKey: unknown) => {
		requireMainAppDocument(event, DESKTOP_BOOT_STATE_CHANNELS.clear, routes);
		return routes.clear(requireAccountKey(accountKey, DESKTOP_BOOT_STATE_CHANNELS.clear));
	});
	ipcMain.handle(DESKTOP_BOOT_STATE_CHANNELS.flushed, (event, id: unknown) => {
		requireMainAppDocument(event, DESKTOP_BOOT_STATE_CHANNELS.flushed, routes);
		if (typeof id !== 'number' || !Number.isSafeInteger(id) || id <= 0) {
			throw new BootStateIpcError(DESKTOP_BOOT_STATE_CHANNELS.flushed, 'invalid flush id');
		}
		routes.flushed(event.sender, id);
	});
	ipcMain.handle(DESKTOP_BOOT_STATE_CHANNELS.captureFrame, (event, request: unknown) => {
		requireMainAppDocument(event, DESKTOP_BOOT_STATE_CHANNELS.captureFrame, routes);
		return routes.captureFrame(event.sender, readFrameRequest(request));
	});
	ipcMain.handle(DESKTOP_BOOT_STATE_CHANNELS.takeFrame, (event) => {
		requireMainAppDocument(event, DESKTOP_BOOT_STATE_CHANNELS.takeFrame, routes);
		return routes.takeFrame(event.sender);
	});
}
