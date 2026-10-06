// SPDX-License-Identifier: AGPL-3.0-or-later

import {watch} from 'node:fs';
import {userInfo} from 'node:os';
import {getGlobalShortcutsSettings, setGlobalShortcutsSettings} from '@electron/common/DesktopConfig';
import {LINUX_DESKTOP_ENTRY_ID, LINUX_PORTAL_SESSION_TOKEN} from '@electron/common/DesktopIdentity';
import {createChildLogger} from '@electron/common/Logger';
import type {GlobalShortcutsStatus} from '@electron/common/Types';
import {
	GlobalShortcutsEngine,
	type GlobalShortcutsEngineDeps,
	type GlobalShortcutsLinuxEnvironment,
	type GlobalShortcutsSettingsStore,
	type InputDeviceWatch,
	portalDefinitionsFromStored,
	sanitizeGlobalShortcutsSyncPayload,
} from '@electron/main/GlobalShortcutsEngine';
import {
	createLinuxSessionStateMonitor,
	createNativeGlobalShortcutsPortal,
	createNativeHookBackend,
	hasMacInputMonitoringAccess,
	isLinuxKeyboardReadable,
} from '@electron/main/GlobalShortcutsNative';
import {getLinuxInputHookMode, getLinuxPortalsMode} from '@electron/main/LaunchOptions';
import {LegacyGlobalKeyHookAdapter, registerLegacyGlobalKeyHookHandlers} from '@electron/main/LegacyGlobalKeyHookIpc';
import {LinuxPortalShortcutsManager} from '@electron/main/LinuxGlobalShortcutsPortal';
import {getFlatpakAppId, isFlatpakRuntime} from '@electron/main/LinuxSandbox';
import {getLinuxDesktop, getLinuxSessionType, isKdePlasma5Session} from '@electron/main/LinuxSession';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {getMainWindow} from '@electron/main/Window';
import {app, ipcMain, powerMonitor} from 'electron';

const logger = createChildLogger('GlobalShortcuts');

let engine: GlobalShortcutsEngine | null = null;
let legacyAdapter: LegacyGlobalKeyHookAdapter | null = null;
const senderWatchers = new Map<number, Set<() => void>>();
let detachPowerListeners: (() => void) | null = null;

function log(message: string, details?: Record<string, unknown>): void {
	logger.info(message, details ?? {});
}

function getShortcutsPlatform(): GlobalShortcutsEngineDeps['platform'] {
	if (process.platform === 'darwin') return 'macos';
	if (process.platform === 'win32') return 'windows';
	return 'linux';
}

function watchSender(sender: Electron.WebContents, onGone: () => void): void {
	const senderId = sender.id;
	const existing = senderWatchers.get(senderId);
	if (existing) {
		existing.add(onGone);
		return;
	}
	const callbacks = new Set([onGone]);
	senderWatchers.set(senderId, callbacks);
	const runCallbacks = (): void => {
		const pending = [...callbacks];
		callbacks.clear();
		for (const callback of pending) callback();
	};
	sender.on('did-navigate', runCallbacks);
	sender.on('render-process-gone', runCallbacks);
	sender.once('destroyed', () => {
		runCallbacks();
		senderWatchers.delete(senderId);
	});
}

function createSettingsStore(): GlobalShortcutsSettingsStore {
	return {
		getDirectInputEnabled: () => getGlobalShortcutsSettings().directInputEnabled,
		setDirectInputEnabled: (enabled) => {
			setGlobalShortcutsSettings({directInputEnabled: enabled});
		},
		isMigrated: () => getGlobalShortcutsSettings().migrated,
		setMigrated: () => {
			setGlobalShortcutsSettings({migrated: true});
		},
		getLastActions: () => getGlobalShortcutsSettings().lastActions,
		setLastActions: (actions) => {
			setGlobalShortcutsSettings({lastActions: actions});
		},
	};
}

function getPortalParentWindow(linux: GlobalShortcutsLinuxEnvironment): string {
	const window = getMainWindow();
	if (window === null || window.isDestroyed()) return '';
	if (linux.sandboxed && linux.session === 'wayland') return '';
	const x11Window = app.commandLine.getSwitchValue('ozone-platform') === 'x11' || linux.session !== 'wayland';
	if (!x11Window) return '';
	const handle = window.getNativeWindowHandle();
	if (handle.length === 8) return `x11:${handle.readBigUInt64LE(0).toString(16)}`;
	if (handle.length === 4) return `x11:${handle.readUInt32LE(0).toString(16)}`;
	return '';
}

function schedule(callback: () => void, delayMs: number): {cancel(): void} {
	const timer = setTimeout(callback, delayMs);
	timer.unref?.();
	return {cancel: () => clearTimeout(timer)};
}

function watchLinuxInputDevices(onChange: () => void): InputDeviceWatch | null {
	try {
		const watcher = watch('/dev/input', {persistent: false}, () => onChange());
		watcher.on('error', (error) => {
			log('Input device watch failed', {message: error.message});
			watcher.close();
		});
		return {close: () => watcher.close()};
	} catch (error) {
		log('Input device watch unavailable', {message: error instanceof Error ? error.message : String(error)});
		return null;
	}
}

function createPortalManager(
	linux: GlobalShortcutsLinuxEnvironment,
	getEngine: () => GlobalShortcutsEngine | null,
): LinuxPortalShortcutsManager | null {
	if (linux.session !== 'wayland' || getLinuxPortalsMode() === 'off') return null;
	const hostPortalAppId = linux.sandboxed ? null : LINUX_DESKTOP_ENTRY_ID;
	return new LinuxPortalShortcutsManager({
		createPortal: (onEvent) =>
			createNativeGlobalShortcutsPortal(onEvent, {
				portalAppId: hostPortalAppId,
				sandboxed: linux.sandboxed,
				sessionToken: LINUX_PORTAL_SESSION_TOKEN,
				desktop: linux.desktop,
			}),
		desktop: linux.desktop,
		plasma5: isKdePlasma5Session(process.env),
		portalAppId: linux.sandboxed ? getFlatpakAppId() : LINUX_DESKTOP_ENTRY_ID,
		getConsent: () => getGlobalShortcutsSettings().portalConsent,
		setConsent: (consent) => {
			setGlobalShortcutsSettings({portalConsent: consent});
		},
		getDefinitions: () => portalDefinitionsFromStored(getGlobalShortcutsSettings().lastActions),
		getParentWindow: () => getPortalParentWindow(linux),
		onShortcut: (action, phase) => getEngine()?.handlePortalShortcut(action, phase),
		onStatusChanged: () => getEngine()?.notifyStatus(),
		schedule,
		now: () => Date.now(),
		log,
	});
}

function attachPowerListeners(target: GlobalShortcutsEngine): () => void {
	const releaseAll = (): void => {
		target.releaseAllPressed();
	};
	powerMonitor.on('suspend', releaseAll);
	powerMonitor.on('lock-screen', releaseAll);
	const sessionMonitor = createLinuxSessionStateMonitor((event) => {
		if (event.type === 'screen-locked') releaseAll();
	});
	return () => {
		powerMonitor.off('suspend', releaseAll);
		powerMonitor.off('lock-screen', releaseAll);
		sessionMonitor?.close();
	};
}

function logDesktopIdMismatch(linux: GlobalShortcutsLinuxEnvironment): void {
	if (linux.sandboxed) return;
	const expected = `${LINUX_DESKTOP_ENTRY_ID}.desktop`;
	const actual = process.env.CHROME_DESKTOP ?? null;
	if (actual !== expected) {
		logger.warn('CHROME_DESKTOP does not match the portal app id', {expected, actual});
	}
}

function readUsername(): string | null {
	try {
		return userInfo().username;
	} catch {
		return null;
	}
}

function requireEngine(): GlobalShortcutsEngine {
	if (engine === null) throw new Error('Global shortcuts are not initialised');
	return engine;
}

function ensureClient(event: Electron.IpcMainInvokeEvent, channel: string): number {
	requirePrivilegedRendererDocumentSender(event, channel);
	const current = requireEngine();
	const sender = event.sender;
	const senderId = sender.id;
	if (!current.hasClient(senderId)) {
		current.attachClient(senderId, (eventChannel, payload) => {
			if (!sender.isDestroyed()) sender.send(eventChannel, payload);
		});
		watchSender(sender, () => engine?.detachClient(senderId));
	}
	return senderId;
}

function registerGlobalShortcutsHandlers(): void {
	ipcMain.handle('global-shortcuts:sync', (event, payload: unknown): void => {
		const senderId = ensureClient(event, 'global-shortcuts:sync');
		const sanitized = sanitizeGlobalShortcutsSyncPayload(payload);
		if (sanitized === null) throw new Error('Invalid global shortcuts sync payload');
		requireEngine().sync(senderId, sanitized);
	});
	ipcMain.handle('global-shortcuts:set-paused', (event, paused: unknown): void => {
		const senderId = ensureClient(event, 'global-shortcuts:set-paused');
		if (typeof paused !== 'boolean') throw new Error('Invalid paused flag');
		requireEngine().setPaused(senderId, paused);
	});
	ipcMain.handle('global-shortcuts:get-status', (event): GlobalShortcutsStatus => {
		ensureClient(event, 'global-shortcuts:get-status');
		return requireEngine().getStatus();
	});
	ipcMain.handle('global-shortcuts:set-up', async (event): Promise<GlobalShortcutsStatus> => {
		ensureClient(event, 'global-shortcuts:set-up');
		await requireEngine().setUp();
		return requireEngine().getStatus();
	});
	ipcMain.handle('global-shortcuts:recheck', async (event): Promise<GlobalShortcutsStatus> => {
		ensureClient(event, 'global-shortcuts:recheck');
		await requireEngine().recheck();
		return requireEngine().getStatus();
	});
	ipcMain.handle('global-shortcuts:start-capture', (event): Promise<number | null> => {
		const senderId = ensureClient(event, 'global-shortcuts:start-capture');
		return requireEngine().startCapture(senderId);
	});
	ipcMain.handle('global-shortcuts:stop-capture', (event, captureId: unknown): void => {
		const senderId = ensureClient(event, 'global-shortcuts:stop-capture');
		if (typeof captureId !== 'number' || !Number.isSafeInteger(captureId)) throw new Error('Invalid capture id');
		requireEngine().stopCapture(senderId, captureId);
	});
	ipcMain.handle('global-shortcuts:configure', async (event): Promise<void> => {
		ensureClient(event, 'global-shortcuts:configure');
		await requireEngine().configure();
	});
	ipcMain.handle(
		'global-shortcuts:set-direct-input-enabled',
		async (event, enabled: unknown): Promise<GlobalShortcutsStatus> => {
			ensureClient(event, 'global-shortcuts:set-direct-input-enabled');
			if (typeof enabled !== 'boolean') throw new Error('Invalid direct input flag');
			const current = requireEngine();
			current.setDirectInputEnabled(enabled);
			await current.refreshHooks();
			return current.getStatus();
		},
	);
}

export function initializeGlobalShortcuts(): void {
	if (engine !== null) return;
	const platform = getShortcutsPlatform();
	const linux: GlobalShortcutsLinuxEnvironment | null =
		platform === 'linux'
			? {
					session: getLinuxSessionType(),
					sandboxed: isFlatpakRuntime(),
					desktop: getLinuxDesktop(),
					inputHookMode: getLinuxInputHookMode(),
				}
			: null;
	if (linux !== null) logDesktopIdMismatch(linux);
	const portal = linux === null ? null : createPortalManager(linux, () => engine);
	const created = new GlobalShortcutsEngine({
		platform,
		linux,
		settings: createSettingsStore(),
		portal,
		createHookBackend: createNativeHookBackend,
		isDirectInputAvailable: isLinuxKeyboardReadable,
		watchInputDevices: watchLinuxInputDevices,
		schedule,
		log,
	});
	engine = created;
	const adapter = new LegacyGlobalKeyHookAdapter();
	legacyAdapter = adapter;
	created.addConsumer(adapter);
	registerGlobalShortcutsHandlers();
	registerLegacyGlobalKeyHookHandlers(created, adapter, {
		platform: process.platform,
		sandboxed: linux?.sandboxed === true,
		username: readUsername(),
		hasInputMonitoringAccess: hasMacInputMonitoringAccess,
		watchSender,
	});
	detachPowerListeners = attachPowerListeners(created);
	log('Global shortcuts initialised', {platform, linux, backend: created.getBackendSelection()});
	created.start();
}

export function cleanupGlobalShortcuts(): void {
	detachPowerListeners?.();
	detachPowerListeners = null;
	legacyAdapter?.clear();
	legacyAdapter = null;
	engine?.dispose();
	engine = null;
}
