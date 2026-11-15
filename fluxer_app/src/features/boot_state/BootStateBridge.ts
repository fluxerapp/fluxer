// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BootStateFrameRecord} from '@app/features/boot_state/BootStateCodec';
import {type BootStateWebStore, createWebBootStateStore} from '@app/features/boot_state/BootStateWebStore';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	DesktopBootStateAPI,
	DesktopBootStateReadResult,
	DesktopBootStateWriteResult,
} from '@fluxer/desktop_ipc/src/BootStateContract';

const logger = new Logger('BootStateBridge');

const writeTokens = new Map<string, string>();
const pairedFrames = new Map<string, BootStateFrameRecord>();
const clearListeners = new Set<(accountKey: string) => void>();
let clearGeneration = 0;
let webStore: BootStateWebStore | null | undefined;

export type BootStateStore = Pick<DesktopBootStateAPI, 'read' | 'issueWriteToken' | 'write' | 'clear'> &
	Partial<Pick<DesktopBootStateAPI, 'captureFrame' | 'onFlushRequested'>>;

type FrameCapturingStore = BootStateStore & Pick<DesktopBootStateAPI, 'captureFrame'>;

function resolveWebStore(): BootStateWebStore | null {
	if (webStore === undefined) {
		webStore = createWebBootStateStore();
	}
	return webStore;
}

export function resolveBootStateAPI(): BootStateStore | null {
	if (isDesktopLocalAppDocument()) {
		return window.electron?.bootState ?? null;
	}
	return resolveWebStore();
}

export function webBootStateAvailable(): boolean {
	return !isDesktopLocalAppDocument() && resolveWebStore() !== null;
}

export function capturesBootFrames(api: BootStateStore): api is FrameCapturingStore {
	return api.captureFrame !== undefined;
}

export function capturesDomBootFrames(api: BootStateStore): boolean {
	return webStore != null && api === webStore;
}

export interface PendingBootStateRead {
	readonly accountKey: string;
	readonly userId: string;
	readonly startedAt: number;
	readonly result: Promise<DesktopBootStateReadResult>;
}

export function startBootStateRead(account: {storageKey: string; userId: string} | null): PendingBootStateRead | null {
	const api = resolveBootStateAPI();
	if (api === null || account === null) {
		return null;
	}
	return {
		accountKey: account.storageKey,
		userId: account.userId,
		startedAt: performance.now(),
		result: api.read(account.storageKey),
	};
}

export async function acquireBootStateWriteToken(api: BootStateStore, accountKey: string): Promise<string | null> {
	const cached = writeTokens.get(accountKey);
	if (cached !== undefined) {
		return cached;
	}
	const generation = clearGeneration;
	const token = await api.issueWriteToken(accountKey);
	if (token === null || generation !== clearGeneration) {
		return null;
	}
	writeTokens.set(accountKey, token);
	return token;
}

export function onBootStateCleared(listener: (accountKey: string) => void): () => void {
	clearListeners.add(listener);
	return () => {
		clearListeners.delete(listener);
	};
}

export function pairedBootFrame(accountKey: string): BootStateFrameRecord | null {
	return pairedFrames.get(accountKey) ?? null;
}

export function pairBootFrame(accountKey: string, frame: BootStateFrameRecord | null): void {
	if (frame === null) {
		pairedFrames.delete(accountKey);
		return;
	}
	pairedFrames.set(accountKey, frame);
}

export async function writeBootState(
	api: BootStateStore,
	accountKey: string,
	writeToken: string,
	payload: Uint8Array,
	frameId: string | null,
): Promise<DesktopBootStateWriteResult> {
	const result = await api.write({accountKey, writeToken, payload, frameId});
	if (result === 'revoked' && writeTokens.get(accountKey) === writeToken) {
		writeTokens.delete(accountKey);
	}
	return result;
}

export async function captureBootFrame(
	api: FrameCapturingStore,
	accountKey: string,
	writeToken: string,
	options: {readonly captureAppCheck: boolean; readonly systemDark: boolean | null},
): Promise<string | null> {
	try {
		const result = await api.captureFrame({accountKey, writeToken, ...options});
		if (result.status === 'revoked' && writeTokens.get(accountKey) === writeToken) {
			writeTokens.delete(accountKey);
		}
		return result.status === 'captured' ? result.frameId : null;
	} catch (error) {
		logger.warn('Failed to capture the boot frame', {accountKey, error});
		return null;
	}
}

export async function clearBootState(accountKey: string): Promise<void> {
	clearGeneration += 1;
	writeTokens.delete(accountKey);
	pairedFrames.delete(accountKey);
	for (const listener of clearListeners) {
		listener(accountKey);
	}
	const api = resolveBootStateAPI();
	if (api === null) {
		return;
	}
	try {
		await api.clear(accountKey);
	} catch (error) {
		logger.warn('Failed to clear the boot state', {accountKey, error});
	}
}

export async function wipeAllBootState(accountKeys: ReadonlyArray<string>): Promise<void> {
	if (isDesktopLocalAppDocument()) {
		await Promise.all(accountKeys.map((accountKey) => clearBootState(accountKey)));
		return;
	}
	clearGeneration += 1;
	writeTokens.clear();
	pairedFrames.clear();
	for (const accountKey of accountKeys) {
		for (const listener of clearListeners) {
			listener(accountKey);
		}
	}
	try {
		await resolveWebStore()?.wipeAll();
	} catch (error) {
		logger.warn('Failed to wipe the boot state', {error});
	}
}
