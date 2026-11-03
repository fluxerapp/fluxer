// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GlobalKeyHookRegisterOptions} from '@electron/common/Types';
import {hookKeyNameToDomCode} from '@electron/main/GlobalShortcutKeys';
import {GlobalShortcutMatcher, type ShortcutBinding, ShortcutPressPolicy} from '@electron/main/GlobalShortcutMatcher';
import type {
	GlobalShortcutsConsumer,
	GlobalShortcutsEngine,
	HookBackendKind,
	HookEvent,
} from '@electron/main/GlobalShortcutsEngine';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {ipcMain} from 'electron';

const MAX_LEGACY_ID_LENGTH = 512;
const MAX_LEGACY_KEY_NAME_LENGTH = 64;
const MAX_LEGACY_MOUSE_BUTTON = 31;

interface LegacyLinuxEvdevStatus {
	supported: boolean;
	hasAccess: boolean;
	canPrompt: boolean;
	sandboxed: boolean;
	username: string | null;
	totalEventDevices: number;
	readableEventDevices: number;
	inInputGroup: boolean;
}

export interface LegacyHostEnvironment {
	platform: NodeJS.Platform;
	sandboxed: boolean;
	username: string | null;
	hasInputMonitoringAccess: () => boolean;
	watchSender: (sender: Electron.WebContents, onGone: () => void) => void;
}

function readLegacyName(value: unknown): string | null {
	if (typeof value !== 'string' || value.length === 0 || value.length > MAX_LEGACY_KEY_NAME_LENGTH) return null;
	return value;
}

interface LegacyClient {
	send: (channel: string, payload: unknown) => void;
	starts: number;
	matcher: GlobalShortcutMatcher;
	registrations: Map<string, ShortcutBinding>;
}

export class LegacyGlobalKeyHookAdapter implements GlobalShortcutsConsumer {
	private readonly clients = new Map<number, LegacyClient>();

	attach(id: number, send: (channel: string, payload: unknown) => void): boolean {
		if (this.clients.has(id)) return false;
		this.clients.set(id, {
			send,
			starts: 0,
			matcher: new GlobalShortcutMatcher(),
			registrations: new Map(),
		});
		return true;
	}

	detach(id: number): void {
		this.clients.delete(id);
	}

	clear(): void {
		this.clients.clear();
	}

	acquire(id: number): void {
		const client = this.clients.get(id);
		if (client) client.starts += 1;
	}

	release(id: number): void {
		const client = this.clients.get(id);
		if (client && client.starts > 0) client.starts -= 1;
	}

	register(id: number, binding: ShortcutBinding): void {
		const client = this.clients.get(id);
		if (!client) return;
		client.registrations.set(binding.sourceId, binding);
		this.applyRegistrations(client);
	}

	unregister(id: number, registrationId: string): void {
		const client = this.clients.get(id);
		if (!client?.registrations.delete(registrationId)) return;
		this.applyRegistrations(client);
	}

	unregisterAll(id: number): void {
		const client = this.clients.get(id);
		if (!client) return;
		client.registrations.clear();
		this.applyRegistrations(client);
	}

	wantsHooks(): boolean {
		for (const client of this.clients.values()) {
			if (client.starts > 0) return true;
		}
		return false;
	}

	handleHookEvent(event: HookEvent, backend: HookBackendKind): void {
		for (const client of this.clients.values()) {
			const transitions =
				event.native === null
					? client.matcher.handleMouse(event, ShortcutPressPolicy.PRESS_AND_RELEASE)
					: client.matcher.handleKey(event, ShortcutPressPolicy.PRESS_AND_RELEASE);
			for (const transition of transitions) {
				client.send('global-keybind-triggered', {
					id: transition.sourceId,
					type: transition.phase === 'press' ? 'keydown' : 'keyup',
				});
			}
			if (client.starts === 0) {
				continue;
			}
			const modifiers = {
				ctrlKey: event.ctrlKey,
				altKey: event.altKey,
				shiftKey: event.shiftKey,
				metaKey: event.metaKey,
			};
			if (event.native === null) {
				client.send('global-mouse-event', {type: event.type, button: event.button, ...modifiers});
			} else {
				client.send('global-key-event', {
					type: event.type,
					keycode: event.native.keycode,
					keyName: event.native.keyName,
					...modifiers,
					backend: backend === 'evdev' ? 'evdev' : 'native',
				});
			}
		}
	}

	releaseHookPresses(): void {
		for (const client of this.clients.values()) {
			for (const transition of client.matcher.releaseAll()) {
				client.send('global-keybind-triggered', {id: transition.sourceId, type: 'keyup'});
			}
		}
	}

	private applyRegistrations(client: LegacyClient): void {
		for (const transition of client.matcher.setBindings([...client.registrations.values()])) {
			client.send('global-keybind-triggered', {id: transition.sourceId, type: 'keyup'});
		}
	}
}

export function registerLegacyGlobalKeyHookHandlers(
	engine: GlobalShortcutsEngine,
	adapter: LegacyGlobalKeyHookAdapter,
	host: LegacyHostEnvironment,
): void {
	const ensureClient = (sender: Electron.WebContents): void => {
		const senderId = sender.id;
		const send = (channel: string, payload: unknown): void => {
			if (!sender.isDestroyed()) sender.send(channel, payload);
		};
		if (!adapter.attach(senderId, send)) return;
		host.watchSender(sender, () => {
			adapter.detach(senderId);
			void engine.refreshHooks();
		});
	};
	ipcMain.handle('global-key-hook-start', async (event): Promise<boolean> => {
		requirePrivilegedRendererDocumentSender(event, 'global-key-hook-start');
		if (host.platform === 'darwin' && !host.hasInputMonitoringAccess()) return false;
		ensureClient(event.sender);
		adapter.acquire(event.sender.id);
		engine.migrateLegacyDirectInput();
		await engine.refreshHooks();
		if (engine.hooksActive()) return true;
		adapter.release(event.sender.id);
		await engine.refreshHooks();
		return false;
	});
	ipcMain.handle('global-key-hook-stop', async (event): Promise<void> => {
		requirePrivilegedRendererDocumentSender(event, 'global-key-hook-stop');
		adapter.release(event.sender.id);
		await engine.refreshHooks();
	});
	ipcMain.handle('check-input-monitoring-access', (event): boolean => {
		requirePrivilegedRendererDocumentSender(event, 'check-input-monitoring-access');
		return host.hasInputMonitoringAccess();
	});
	ipcMain.handle('global-key-hook-register', (event, options: GlobalKeyHookRegisterOptions): void => {
		requirePrivilegedRendererDocumentSender(event, 'global-key-hook-register');
		if (options === null || typeof options !== 'object') {
			return;
		}
		const record = options;
		const id = record.id;
		if (typeof id !== 'string' || id.length === 0 || id.length > MAX_LEGACY_ID_LENGTH) {
			return;
		}
		const modifiers = {
			ctrl: record.ctrl === true,
			alt: record.alt === true,
			shift: record.shift === true,
			meta: record.meta === true,
		};
		const mouseButton = record.mouseButton;
		let binding: ShortcutBinding;
		if (typeof mouseButton === 'number' && Number.isInteger(mouseButton)) {
			if (mouseButton < 0 || mouseButton > MAX_LEGACY_MOUSE_BUTTON) {
				return;
			}
			binding = {sourceId: id, action: id, trigger: {kind: 'mouse', button: mouseButton}, modifiers};
		} else {
			const keyName = readLegacyName(record.keyName);
			const physicalKeyName = readLegacyName(record.physicalKeyName);
			const physicalSource = physicalKeyName ?? keyName;
			const code = physicalSource === null ? null : hookKeyNameToDomCode(physicalSource);
			if (keyName === null && code === null) {
				return;
			}
			binding = {sourceId: id, action: id, trigger: {kind: 'key', layoutKey: keyName, code}, modifiers};
		}
		ensureClient(event.sender);
		adapter.register(event.sender.id, binding);
		engine.migrateLegacyDirectInput();
	});
	ipcMain.handle('global-key-hook-unregister', (event, id: unknown): void => {
		requirePrivilegedRendererDocumentSender(event, 'global-key-hook-unregister');
		if (typeof id !== 'string') return;
		adapter.unregister(event.sender.id, id);
	});
	ipcMain.handle('global-key-hook-unregister-all', (event): void => {
		requirePrivilegedRendererDocumentSender(event, 'global-key-hook-unregister-all');
		adapter.unregisterAll(event.sender.id);
	});
	ipcMain.handle('linux-evdev-status', (event): LegacyLinuxEvdevStatus => {
		requirePrivilegedRendererDocumentSender(event, 'linux-evdev-status');
		const linux = host.platform === 'linux';
		return {
			supported: linux,
			hasAccess: true,
			canPrompt: false,
			sandboxed: linux && host.sandboxed,
			username: host.username,
			totalEventDevices: 0,
			readableEventDevices: 0,
			inInputGroup: false,
		};
	});
}
