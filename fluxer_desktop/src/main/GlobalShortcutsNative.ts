// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import {
	evdevKeyToDomCode,
	macosKeycodeToDomCode,
	modifierKindForCode,
	windowsKeyToDomCode,
	windowsScanCodeKey,
	x11KeycodeToDomCode,
} from '@electron/main/GlobalShortcutKeys';
import type {HookBackend, HookBackendKind, HookEvent, HookStartResult} from '@electron/main/GlobalShortcutsEngine';
import {getLinuxPortalsMode} from '@electron/main/LaunchOptions';
import type {PortalClient, PortalClientOptions, PortalEvent} from '@electron/main/LinuxGlobalShortcutsPortal';
import {getTccStatus} from '@electron/main/MacTcc';

const logger = createChildLogger('GlobalShortcutsNative');
const requireModule = createRequire(import.meta.url);

const WINDOWS_VK_RAW_PREFIX = 0x10000;
const WINDOWS_PACKET_RAW_PREFIX = 0x20000;
const WINDOWS_VK_PACKET = 0xe7;
const WINDOWS_PACKET_SPACE = 0x20;
const UNMAPPED_KEY_NAME = /^Key\d+$/;
const X11_UNFLAGGED_MODIFIER_CODES: ReadonlyArray<string> = ['AltRight'];

interface NativeModifierFlags {
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

interface NativeKeyEvent extends NativeModifierFlags {
	type: 'keydown' | 'keyup';
	keycode: number;
	keyName: string;
	x11Keycode?: number;
	scanCode?: number;
	extended?: boolean;
}

interface NativeMouseEvent extends NativeModifierFlags {
	type: 'mousedown' | 'mouseup';
	button: number;
}

type NativeHookEvent = NativeKeyEvent | NativeMouseEvent;

interface NativeInputHookInstance {
	start(): boolean | undefined | Promise<void>;
	stop(): void;
}

interface NativeInputHookModule {
	InputHook: (new (callback: (event: NativeHookEvent) => void) => NativeInputHookInstance) | null;
	hasAccessibilityPermission?: () => boolean;
	loadError: Error | null;
}

interface NativeEvdevModule {
	EvdevHook: (new (callback: (event: NativeHookEvent) => void) => NativeInputHookInstance) | null;
	isKeyboardReadable: (() => boolean) | null;
	loadError: Error | null;
}

interface NativeGlobalShortcutsPortalCtor {
	new (onEvent: (event: PortalEvent) => void, options: PortalClientOptions): PortalClient;
}

export type SessionStateEvent = {type: 'screen-locked'} | {type: 'screen-unlocked'};

export interface SessionStateMonitorHandle {
	close(): void;
}

interface NativeSessionStateMonitorCtor {
	new (onEvent: (event: SessionStateEvent) => void): SessionStateMonitorHandle;
}

interface NativePortalsModule {
	GlobalShortcutsPortal: NativeGlobalShortcutsPortalCtor | null;
	SessionStateMonitor?: NativeSessionStateMonitorCtor | null;
	loadError: Error | null;
}

const NATIVE_HOOK_MODULES: Readonly<Record<Exclude<HookBackendKind, 'evdev'>, string>> = {
	x11: '@fluxer/linux-input-hook',
	windows: '@fluxer/windows-input-hook',
	macos: '@fluxer/macos-input-hook',
};

const moduleCache = new Map<string, unknown>();

function loadModule<T extends {loadError: Error | null}>(name: string): T {
	const cached = moduleCache.get(name);
	if (cached !== undefined) return cached as T;
	let loaded: T;
	try {
		loaded = requireModule(name) as T;
	} catch (error) {
		throw new Error(
			`${name} failed to load; this is a packaging bug, not a runtime fallback case. ` +
				`Original error: ${error instanceof Error ? error.message : String(error)}`,
		);
	}
	moduleCache.set(name, loaded);
	return loaded;
}

function keyCodeIdentity(
	kind: HookBackendKind,
	event: NativeKeyEvent,
): {code: string | null; key: string | null; raw: number} {
	switch (kind) {
		case 'x11': {
			const x11Keycode = event.x11Keycode ?? 0;
			return {code: x11KeycodeToDomCode(x11Keycode), key: event.keyName || null, raw: x11Keycode};
		}
		case 'evdev':
			return {code: evdevKeyToDomCode(event.keycode), key: null, raw: event.keycode};
		case 'windows': {
			const scanCode = event.scanCode ?? 0;
			const extended = event.extended === true;
			if (event.keycode === WINDOWS_VK_PACKET) {
				return {
					code: scanCode === WINDOWS_PACKET_SPACE ? 'Space' : null,
					key: windowsPacketKey(scanCode),
					raw: WINDOWS_PACKET_RAW_PREFIX | scanCode,
				};
			}
			return {
				code: windowsKeyToDomCode(scanCode, extended, event.keycode),
				key: event.keyName && !UNMAPPED_KEY_NAME.test(event.keyName) ? event.keyName : null,
				raw: scanCode === 0 ? WINDOWS_VK_RAW_PREFIX | event.keycode : windowsScanCodeKey(scanCode, extended),
			};
		}
		case 'macos':
			return {code: macosKeycodeToDomCode(event.keycode), key: null, raw: event.keycode};
	}
}

function windowsPacketKey(character: number): string | null {
	const upper = String.fromCharCode(character).toUpperCase();
	return /^[A-Z0-9]$/.test(upper) ? upper : null;
}

function isNativeKeyEvent(event: NativeHookEvent): event is NativeKeyEvent {
	return event.type === 'keydown' || event.type === 'keyup';
}

function isNativeMouseEvent(event: NativeHookEvent): event is NativeMouseEvent {
	return event.type === 'mousedown' || event.type === 'mouseup';
}

export function translateNativeHookEvent(kind: HookBackendKind, event: NativeHookEvent): HookEvent | null {
	const unflaggedModifierCodes = kind === 'x11' ? X11_UNFLAGGED_MODIFIER_CODES : undefined;
	if (isNativeMouseEvent(event)) {
		return {
			type: event.type,
			button: event.button,
			ctrlKey: event.ctrlKey,
			altKey: event.altKey,
			shiftKey: event.shiftKey,
			metaKey: event.metaKey,
			unflaggedModifierCodes,
			native: null,
		};
	}
	if (!isNativeKeyEvent(event)) return null;
	const identity = keyCodeIdentity(kind, event);
	if (kind === 'evdev' && identity.code === null) return null;
	const own = kind === 'windows' && event.type === 'keydown' ? modifierKindForCode(identity.code) : null;
	return {
		type: event.type,
		code: identity.code,
		key: identity.key,
		rawKeycode: identity.raw,
		ctrlKey: event.ctrlKey || own === 'ctrl',
		altKey: event.altKey || own === 'alt',
		shiftKey: event.shiftKey || own === 'shift',
		metaKey: event.metaKey || own === 'meta',
		unflaggedModifierCodes,
		native: {keycode: event.keycode, keyName: event.keyName},
	};
}

export function hasMacInputMonitoringAccess(): boolean {
	if (process.platform !== 'darwin') return true;
	try {
		const status = getTccStatus('input-monitoring');
		if (status !== 'not-determined') return status === 'granted';
	} catch (error) {
		logger.warn('Input Monitoring status could not be read', {error});
	}
	try {
		const module = loadModule<NativeInputHookModule>(NATIVE_HOOK_MODULES.macos);
		return module.hasAccessibilityPermission?.() === true;
	} catch (error) {
		logger.warn('Input Monitoring preflight failed', {error});
		return false;
	}
}

class NativeHookBackend implements HookBackend {
	private instance: NativeInputHookInstance | null = null;

	constructor(public readonly kind: HookBackendKind) {}

	async start(onEvent: (event: HookEvent) => void): Promise<HookStartResult> {
		if (this.kind === 'macos' && !hasMacInputMonitoringAccess()) return 'permission';
		const Ctor =
			this.kind === 'evdev'
				? loadModule<NativeEvdevModule>('@fluxer/linux-evdev').EvdevHook
				: loadModule<NativeInputHookModule>(NATIVE_HOOK_MODULES[this.kind]).InputHook;
		if (Ctor === null) return 'start-failed';
		const kind = this.kind;
		const instance = new Ctor((event) => {
			const translated = translateNativeHookEvent(kind, event);
			if (translated !== null) onEvent(translated);
		});
		let started: boolean | undefined;
		try {
			started = (await instance.start()) ?? undefined;
		} catch (error) {
			instance.stop();
			throw error;
		}
		if (started === false) {
			instance.stop();
			return 'start-failed';
		}
		this.instance = instance;
		logger.info('Global shortcut hook running', {backend: kind});
		return 'ok';
	}

	stop(): void {
		const instance = this.instance;
		this.instance = null;
		instance?.stop();
	}
}

export function createNativeHookBackend(kind: HookBackendKind): HookBackend {
	return new NativeHookBackend(kind);
}

export function isLinuxKeyboardReadable(): boolean {
	if (process.platform !== 'linux') return false;
	try {
		const module = loadModule<NativeEvdevModule>('@fluxer/linux-evdev');
		return module.isKeyboardReadable?.() === true;
	} catch (error) {
		logger.warn('Keyboard readability probe failed', {error});
		return false;
	}
}

export function createNativeGlobalShortcutsPortal(
	onEvent: (event: PortalEvent) => void,
	options: PortalClientOptions,
): PortalClient {
	const module = loadModule<NativePortalsModule>('@fluxer/linux-portals');
	if (module.GlobalShortcutsPortal === null) {
		throw new Error(
			`@fluxer/linux-portals exports no GlobalShortcutsPortal: ${module.loadError?.message ?? 'unknown'}`,
		);
	}
	return new module.GlobalShortcutsPortal(onEvent, options);
}

export function createLinuxSessionStateMonitor(
	onEvent: (event: SessionStateEvent) => void,
): SessionStateMonitorHandle | null {
	if (process.platform !== 'linux' || getLinuxPortalsMode(process.argv) === 'off') return null;
	try {
		const Monitor = loadModule<NativePortalsModule>('@fluxer/linux-portals').SessionStateMonitor;
		return Monitor ? new Monitor(onEvent) : null;
	} catch (error) {
		logger.info('Session state monitor unavailable', {error});
		return null;
	}
}
