// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawn} from 'node:child_process';
import {accessSync, constants as fsConstants} from 'node:fs';
import {readdir, readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {userInfo} from 'node:os';
import path from 'node:path';
import {createChildLogger} from '@electron/common/Logger';
import {type EvdevKeyEvent, type EvdevMouseEvent, getEvdevHook, nameToEvdevKeycode} from '@electron/main/EvdevHook';
import {GlobalKeyHookLifecycle} from '@electron/main/GlobalKeyHookLifecycle';
import {getLinuxInputHookMode} from '@electron/main/LaunchOptions';
import {isFlatpakRuntime} from '@electron/main/LinuxSandbox';
import {getTccStatus} from '@electron/main/MacTcc';
import {getMainWindow} from '@electron/main/Window';
import {ipcMain} from 'electron';

const logger = createChildLogger('GlobalKeyHook');
const requireModule = createRequire(import.meta.url);

interface NativeInputEvent {
	type: 'keydown' | 'keyup' | 'mousedown' | 'mouseup' | 'mousemove' | 'wheel';
	keycode?: number;
	keyName?: string;
	button?: number;
	deltaX?: number;
	deltaY?: number;
	x?: number;
	y?: number;
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

interface NativeInputHookCtor {
	new (
		callback: (event: NativeInputEvent) => void,
	): {
		start(): void;
		stop(): void;
	};
}

interface EvdevHookController {
	start(): Promise<boolean>;
	on(event: 'key', listener: (event: EvdevKeyEvent) => void): this;
	on(event: 'mouse', listener: (event: EvdevMouseEvent) => void): this;
}

interface NativeInputHookModule {
	InputHook: NativeInputHookCtor | null;
	isAvailable: () => boolean;
	hasAccessibilityPermission?: () => boolean;
	loadError: Error | null;
}

interface KeybindRegistration {
	id: string;
	description: string | null;
	keycode: number;
	keyName: string | null;
	mouseButton?: number;
	modifiers: {
		ctrl: boolean;
		alt: boolean;
		shift: boolean;
		meta: boolean;
	};
}

type Backend = 'evdev' | 'native' | null;

const registeredKeybinds = new Map<string, KeybindRegistration>();
const activeKeybindPresses = new Set<string>();

let activeBackend: Backend = null;
let activeNativeInstance: {start(): void; stop(): void} | null = null;

function nativeModuleNameForPlatform(): string | null {
	switch (process.platform) {
		case 'linux':
			return '@fluxer/linux-input-hook';
		case 'darwin':
			return '@fluxer/macos-input-hook';
		case 'win32':
			return '@fluxer/windows-input-hook';
		default:
			return null;
	}
}

function loadNativeInputHookModule(): NativeInputHookModule | null {
	const moduleName = nativeModuleNameForPlatform();
	if (moduleName === null) return null;
	if (process.platform === 'linux') {
		const mode = getLinuxInputHookMode(process.argv);
		if (mode === 'off' || mode === 'evdev') return null;
	}
	let required: NativeInputHookModule;
	try {
		required = requireModule(moduleName) as NativeInputHookModule;
	} catch (error) {
		throw new Error(
			`${moduleName} failed to load — this is a packaging bug, not a runtime fallback case. ` +
				`Original error: ${error instanceof Error ? error.message : String(error)}`,
		);
	}
	if (!required.InputHook) {
		throw new Error(
			`${moduleName} loaded but exports no InputHook — native binary did not register module. ` +
				`Underlying loadError: ${required.loadError ? required.loadError.message : '<none>'}`,
		);
	}
	return required;
}

function keycodeToKeyName(keycode: number): string {
	return `Key${keycode}`;
}

const EVDEV_CHARACTER_KEY_NAMES: Record<string, string> = {
	' ': 'Space',
	'-': 'Minus',
	_: 'Minus',
	'=': 'Equal',
	'+': 'Equal',
	',': 'Comma',
	'<': 'Comma',
	'.': 'Period',
	'>': 'Period',
	';': 'Semicolon',
	':': 'Semicolon',
	"'": 'Quote',
	'"': 'Quote',
	'/': 'Slash',
	'?': 'Slash',
	'\\': 'Backslash',
	'|': 'Backslash',
	'[': 'BracketLeft',
	'{': 'BracketLeft',
	']': 'BracketRight',
	'}': 'BracketRight',
	'`': 'Backquote',
	'~': 'Backquote',
};

function normalizeEvdevKeyName(name: string | null): string | null {
	if (!name) return null;
	const mappedCharacter = EVDEV_CHARACTER_KEY_NAMES[name];
	if (mappedCharacter) return mappedCharacter;
	const keyCodeMatch = /^Key([A-Z])$/.exec(name);
	if (keyCodeMatch) return keyCodeMatch[1] ?? null;
	const digitCodeMatch = /^Digit([0-9])$/.exec(name);
	if (digitCodeMatch) return digitCodeMatch[1] ?? null;
	if (name === 'Esc') return 'Escape';
	if (name === 'Spacebar') return 'Space';
	if (name === 'Break') return 'Pause';
	if (/^[a-z]$/.test(name)) return name.toUpperCase();
	return name;
}

function dispatchKeyEvent(event: {
	type: 'keydown' | 'keyup';
	keycode: number;
	keyName: string;
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}): void {
	const mainWindow = getMainWindow();
	if (!mainWindow) return;
	const eventWithBackend = {...event, backend: activeBackend};
	mainWindow.webContents.send('global-key-event', eventWithBackend);
	for (const [id, keybind] of registeredKeybinds) {
		if (!keyEventMatchesRegistration(keybind, eventWithBackend)) continue;
		if (event.type === 'keyup') {
			if (!activeKeybindPresses.delete(id)) continue;
			dispatchGlobalKeybindTriggered(id, event.type);
			continue;
		}
		if (activeKeybindPresses.has(id)) continue;
		const modifiersMatch =
			keybind.modifiers.ctrl === event.ctrlKey &&
			keybind.modifiers.alt === event.altKey &&
			keybind.modifiers.shift === event.shiftKey &&
			keybind.modifiers.meta === event.metaKey;
		if (modifiersMatch || !Object.values(keybind.modifiers).some(Boolean)) {
			activeKeybindPresses.add(id);
			dispatchGlobalKeybindTriggered(id, event.type);
		}
	}
}

function dispatchGlobalKeybindTriggered(id: string, type: 'keydown' | 'keyup'): void {
	const mainWindow = getMainWindow();
	if (!mainWindow) return;
	mainWindow.webContents.send('global-keybind-triggered', {id, type});
}

function shouldPreferPhysicalKeyNameForRegistration(): boolean {
	return activeBackend === 'evdev' || (activeBackend === 'native' && process.platform === 'darwin');
}

const observedMouseButtons = new Set<number>();

function keyEventMatchesRegistration(
	keybind: Pick<KeybindRegistration, 'keycode' | 'keyName'>,
	event: Pick<NativeInputEvent, 'keycode' | 'keyName'>,
): boolean {
	if (keybind.keyName !== null && event.keyName !== null && keybind.keyName === event.keyName) {
		return true;
	}
	return keybind.keycode !== 0 && keybind.keycode === event.keycode;
}

function registrationModifiersMatch(
	expected: KeybindRegistration['modifiers'],
	event: {
		ctrlKey: boolean;
		altKey: boolean;
		shiftKey: boolean;
		metaKey: boolean;
	},
): boolean {
	const exact =
		expected.ctrl === event.ctrlKey &&
		expected.alt === event.altKey &&
		expected.shift === event.shiftKey &&
		expected.meta === event.metaKey;
	return exact || !Object.values(expected).some(Boolean);
}

function dispatchMouseEvent(event: {
	type: 'mousedown' | 'mouseup';
	button: number;
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}): void {
	if (event.type === 'mousedown' && !observedMouseButtons.has(event.button)) {
		observedMouseButtons.add(event.button);
	}
	const mainWindow = getMainWindow();
	if (!mainWindow) return;
	mainWindow.webContents.send('global-mouse-event', event);
	for (const [id, keybind] of registeredKeybinds) {
		if (keybind.mouseButton !== event.button) continue;
		if (event.type === 'mouseup') {
			if (!activeKeybindPresses.delete(id)) continue;
			dispatchGlobalKeybindTriggered(id, 'keyup');
			continue;
		}
		if (activeKeybindPresses.has(id)) continue;
		if (registrationModifiersMatch(keybind.modifiers, event)) {
			activeKeybindPresses.add(id);
			dispatchGlobalKeybindTriggered(id, 'keydown');
		}
	}
}

function handleNativeEvent(event: NativeInputEvent): void {
	switch (event.type) {
		case 'keydown':
		case 'keyup': {
			dispatchKeyEvent({
				type: event.type,
				keycode: event.keycode ?? 0,
				keyName: event.keyName ?? keycodeToKeyName(event.keycode ?? 0),
				ctrlKey: event.ctrlKey,
				altKey: event.altKey,
				shiftKey: event.shiftKey,
				metaKey: event.metaKey,
			});
			return;
		}
		case 'mousedown':
		case 'mouseup': {
			if (event.button === undefined) return;
			dispatchMouseEvent({
				type: event.type,
				button: event.button,
				ctrlKey: event.ctrlKey,
				altKey: event.altKey,
				shiftKey: event.shiftKey,
				metaKey: event.metaKey,
			});
			return;
		}
		case 'mousemove':
		case 'wheel':
			return;
	}
}

async function startEvdevBackend(): Promise<boolean> {
	if (process.platform !== 'linux') return false;
	let evdev: EvdevHookController | null = null;
	let ok = false;
	try {
		evdev = getEvdevHook();
		ok = await evdev.start();
	} catch (error) {
		logger.warn('Failed to start evdev input backend', {error});
		return false;
	}
	if (!ok || !evdev) {
		return false;
	}
	evdev.on('key', (event: EvdevKeyEvent) => dispatchKeyEvent(event));
	evdev.on('mouse', (event: EvdevMouseEvent) => dispatchMouseEvent(event));
	logger.info('Global key hook running on evdev backend (Linux)');
	return true;
}

function startNativeBackend(): boolean {
	const moduleName = nativeModuleNameForPlatform();
	if (moduleName === null) return false;
	let module: NativeInputHookModule;
	try {
		const loaded = loadNativeInputHookModule();
		if (!loaded || !loaded.InputHook) return false;
		module = loaded;
	} catch (error) {
		logger.error('Failed to load native input hook module:', error);
		return false;
	}
	try {
		const Ctor = module.InputHook;
		if (!Ctor) return false;
		const instance = new Ctor((event: NativeInputEvent) => {
			try {
				handleNativeEvent(event);
			} catch (err) {
				logger.error('handleNativeEvent threw:', err);
			}
		});
		instance.start();
		activeNativeInstance = instance;
		logger.info(`Global key hook running on ${moduleName}`);
		return true;
	} catch (error) {
		logger.error('Failed to start native input hook:', error);
		return false;
	}
}

async function startHook(): Promise<boolean> {
	if (activeBackend !== null) return true;
	if (process.platform === 'linux') {
		const mode = getLinuxInputHookMode(process.argv);
		const wayland = isWaylandSession();
		if (mode === 'off') {
			logger.warn('Global key hook disabled by launch diagnostics');
			return false;
		}
		if (mode === 'evdev') {
			if (await startEvdevBackend()) {
				activeBackend = 'evdev';
				return true;
			}
			return false;
		}
		if (mode === 'native') {
			if (wayland) {
				logger.warn('Refusing X11 native input hook on Wayland; evdev input access is required');
				return false;
			}
			if (startNativeBackend()) {
				activeBackend = 'native';
				return true;
			}
			return false;
		}
		if (await startEvdevBackend()) {
			activeBackend = 'evdev';
			return true;
		}
		if (wayland) {
			logger.warn('No evdev input devices are readable; system-wide shortcuts are unavailable on Wayland');
			return false;
		}
	}
	if (startNativeBackend()) {
		activeBackend = 'native';
		return true;
	}
	return false;
}

function isWaylandSession(): boolean {
	if (process.platform !== 'linux') return false;
	return Boolean(process.env.WAYLAND_DISPLAY) || process.env.XDG_SESSION_TYPE === 'wayland';
}

const hookLifecycle = new GlobalKeyHookLifecycle({
	start: startHook,
	stop: stopHook,
});

const trackedSenderIds = new Set<number>();

function releaseSenderState(senderId: number): void {
	registeredKeybinds.clear();
	activeKeybindPresses.clear();
	void hookLifecycle.releaseAllForOwner(senderId);
}

function trackSender(sender: Electron.WebContents): void {
	if (trackedSenderIds.has(sender.id)) return;
	trackedSenderIds.add(sender.id);
	const senderId = sender.id;
	sender.once('destroyed', () => {
		trackedSenderIds.delete(senderId);
		releaseSenderState(senderId);
	});
	sender.on('did-navigate', () => {
		releaseSenderState(senderId);
	});
	sender.on('render-process-gone', () => {
		releaseSenderState(senderId);
	});
}

function stopHook(): void {
	observedMouseButtons.clear();
	activeKeybindPresses.clear();
	if (activeBackend === 'evdev') {
		const evdev = getEvdevHook();
		evdev.removeAllListeners('key');
		evdev.removeAllListeners('mouse');
		evdev.stop();
	} else if (activeBackend === 'native' && activeNativeInstance) {
		try {
			activeNativeInstance.stop();
		} catch (error) {
			logger.error('Failed to stop native input hook:', error);
		}
		activeNativeInstance = null;
	}
	activeBackend = null;
}

function getInputMonitoringStatus(): string | null {
	try {
		return getTccStatus('input-monitoring');
	} catch (error) {
		logger.error('Failed to query Input Monitoring auth status:', error);
		return null;
	}
}

function preflightInputMonitoringAccess(): boolean | null {
	let module: NativeInputHookModule | null = null;
	try {
		module = loadNativeInputHookModule();
	} catch (error) {
		logger.warn('Failed to load native input hook module for Input Monitoring preflight:', error);
		return null;
	}
	if (!module?.hasAccessibilityPermission) return null;
	try {
		return module.hasAccessibilityPermission();
	} catch (error) {
		logger.warn('Input Monitoring preflight threw:', error);
		return null;
	}
}

async function checkInputMonitoringAccess(): Promise<boolean> {
	if (process.platform !== 'darwin') {
		return true;
	}
	if (activeBackend !== null) {
		return true;
	}
	const preflight = preflightInputMonitoringAccess();
	if (preflight !== null) {
		if (!preflight) {
			logger.warn('Input Monitoring listen-event access is not granted (CGPreflightListenEventAccess)');
		}
		return preflight;
	}
	const status = getInputMonitoringStatus();
	if (status === null) {
		logger.warn('Input Monitoring access could not be checked; refusing to start macOS native input hook');
		return false;
	}
	if (status === 'granted') {
		return true;
	}
	logger.warn('Input Monitoring access is not granted, status:', status);
	return false;
}

interface LinuxEvdevAccessProbe {
	totalEventDevices: number;
	readableEventDevices: number;
	inInputGroup: boolean;
}

async function probeLinuxEvdevAccess(): Promise<LinuxEvdevAccessProbe> {
	if (process.platform !== 'linux') {
		return {totalEventDevices: 0, readableEventDevices: 0, inInputGroup: true};
	}
	const inInputGroup = isFlatpakRuntime() ? false : await isUserInInputGroup();
	let entries: Array<string>;
	try {
		entries = await readdir('/dev/input');
	} catch {
		return {totalEventDevices: 0, readableEventDevices: 0, inInputGroup};
	}
	let total = 0;
	let readable = 0;
	for (const name of entries) {
		if (!name.startsWith('event')) continue;
		total += 1;
		try {
			accessSync(path.join('/dev/input', name), fsConstants.R_OK);
			readable += 1;
		} catch {}
	}
	return {totalEventDevices: total, readableEventDevices: readable, inInputGroup};
}

async function isUserInInputGroup(): Promise<boolean> {
	if (process.platform !== 'linux') return false;
	if (typeof process.getgroups !== 'function') return false;
	let groupFile: string;
	try {
		groupFile = await readFile('/etc/group', 'utf8');
	} catch {
		return false;
	}
	let inputGid: number | null = null;
	for (const line of groupFile.split('\n')) {
		if (!line.startsWith('input:')) continue;
		const parts = line.split(':');
		const gid = Number.parseInt(parts[2] ?? '', 10);
		if (Number.isFinite(gid)) inputGid = gid;
		break;
	}
	if (inputGid === null) return false;
	let groups: Array<number>;
	try {
		groups = process.getgroups();
	} catch {
		return false;
	}
	return groups.includes(inputGid);
}

async function _collectLinuxEvdevDiagnostics(): Promise<Record<string, unknown>> {
	if (process.platform !== 'linux') {
		return {platform: process.platform};
	}
	const probe = await probeLinuxEvdevAccess();
	return {
		platform: process.platform,
		waylandSession: isWaylandSession(),
		flatpak: isFlatpakRuntime(),
		totalEventDevices: probe.totalEventDevices,
		readableEventDevices: probe.readableEventDevices,
		inInputGroup: probe.inInputGroup,
	};
}

const PKEXEC_CANDIDATE_PATHS = ['/usr/bin/pkexec', '/usr/local/bin/pkexec', '/bin/pkexec'];

function findPkexec(): string | null {
	for (const candidate of PKEXEC_CANDIDATE_PATHS) {
		try {
			accessSync(candidate, fsConstants.X_OK);
			return candidate;
		} catch {}
	}
	return null;
}

interface LinuxEvdevStatus {
	supported: boolean;
	hasAccess: boolean;
	canPrompt: boolean;
	sandboxed: boolean;
	username: string | null;
	totalEventDevices: number;
	readableEventDevices: number;
	inInputGroup: boolean;
}

async function getLinuxEvdevStatus(): Promise<LinuxEvdevStatus> {
	if (process.platform !== 'linux') {
		return {
			supported: false,
			hasAccess: true,
			canPrompt: false,
			sandboxed: false,
			username: null,
			totalEventDevices: 0,
			readableEventDevices: 0,
			inInputGroup: true,
		};
	}
	const probe = await probeLinuxEvdevAccess();
	const hasAccess = probe.inInputGroup || probe.readableEventDevices > 0;
	let username: string | null = null;
	try {
		username = userInfo().username;
	} catch {
		username = process.env.USER ?? null;
	}
	return {
		supported: true,
		hasAccess,
		canPrompt: !isFlatpakRuntime() && !probe.inInputGroup && findPkexec() !== null && !!username,
		sandboxed: isFlatpakRuntime(),
		username,
		totalEventDevices: probe.totalEventDevices,
		readableEventDevices: probe.readableEventDevices,
		inInputGroup: probe.inInputGroup,
	};
}

interface LinuxEvdevGrantResult {
	success: boolean;
	needsRelogin: boolean;
	error?: string;
}

function pkexecFailureReason(code: number | null, signal: NodeJS.Signals | null): string {
	return code === 126 || code === 127
		? 'Authorization was cancelled or no polkit agent is running.'
		: signal
			? `pkexec was terminated by ${signal}`
			: `pkexec exited with code ${code}`;
}

function runPkexec(args: Array<string>): Promise<{success: boolean; error?: string}> {
	const pkexec = findPkexec();
	if (!pkexec) {
		return Promise.resolve({success: false, error: 'Authorization helper is not installed.'});
	}
	return new Promise((resolve) => {
		const child = spawn(pkexec, args, {stdio: 'ignore'});
		child.on('error', (error) => {
			logger.error('Failed to spawn pkexec', error);
			resolve({success: false, error: error.message});
		});
		child.on('exit', (code, signal) => {
			if (code === 0) {
				resolve({success: true});
				return;
			}
			resolve({success: false, error: pkexecFailureReason(code, signal)});
		});
	});
}

async function waitForLinuxEvdevAccessRefresh(): Promise<LinuxEvdevAccessProbe> {
	await new Promise((resolve) => setTimeout(resolve, 700));
	return probeLinuxEvdevAccess();
}

function linuxUaccessGrantScript(): string {
	return [
		'set -eu',
		'target_user="$1"',
		'install -d -m 0755 /etc/udev/rules.d',
		"cat > /etc/udev/rules.d/70-fluxer-input.rules <<'EOF'",
		'# Grants the active local desktop user access to input devices for Fluxer system-wide shortcuts.',
		'KERNEL=="event*", SUBSYSTEM=="input", TAG+="uaccess"',
		'EOF',
		'if command -v udevadm >/dev/null 2>&1; then',
		'  udevadm control --reload-rules || true',
		'  udevadm trigger --subsystem-match=input --action=change || true',
		'  udevadm settle --timeout=3 || true',
		'fi',
		'if command -v setfacl >/dev/null 2>&1; then',
		'  for device in /dev/input/event*; do',
		'    [ -e "$device" ] || continue',
		`    setfacl -m "u:\${target_user}:r" "$device" || true`,
		'  done',
		'fi',
	].join('\n');
}

async function grantLinuxEvdevAccess(): Promise<LinuxEvdevGrantResult> {
	if (process.platform !== 'linux') {
		return {success: false, needsRelogin: false, error: 'Not a Linux session'};
	}
	if (isFlatpakRuntime()) {
		return {success: false, needsRelogin: false, error: 'Enable Flatpak input device access, then restart Fluxer.'};
	}
	if (!findPkexec()) {
		return {
			success: false,
			needsRelogin: false,
			error: 'Authorization helper is not installed.',
		};
	}
	const before = await probeLinuxEvdevAccess();
	if (before.inInputGroup || before.readableEventDevices > 0) {
		return {success: true, needsRelogin: false};
	}
	const username = (() => {
		try {
			return userInfo().username;
		} catch {
			return process.env.USER ?? '';
		}
	})();
	if (!username) {
		return {success: false, needsRelogin: false, error: 'Could not resolve current username'};
	}
	const uaccess = await runPkexec(['/bin/sh', '-c', linuxUaccessGrantScript(), 'fluxer-input-access', username]);
	if (!uaccess.success) {
		return {success: false, needsRelogin: false, error: uaccess.error};
	}
	const afterUaccess = await waitForLinuxEvdevAccessRefresh();
	if (afterUaccess.inInputGroup || afterUaccess.readableEventDevices > 0) {
		return {success: true, needsRelogin: false};
	}
	const groupFallback = await runPkexec(['usermod', '-aG', 'input', username]);
	if (groupFallback.success) {
		return {success: true, needsRelogin: true};
	}
	return {
		success: false,
		needsRelogin: false,
		error: groupFallback.error ?? 'Input access could not be enabled automatically.',
	};
}

export function registerGlobalKeyHookHandlers(): void {
	ipcMain.handle('global-key-hook-start', async (event): Promise<boolean> => {
		if (!(await checkInputMonitoringAccess())) {
			return false;
		}
		trackSender(event.sender);
		return hookLifecycle.acquire(event.sender.id);
	});
	ipcMain.handle('global-key-hook-stop', async (event): Promise<void> => {
		await hookLifecycle.release(event.sender.id);
	});
	ipcMain.handle('global-key-hook-is-running', (): boolean => {
		return hookLifecycle.isRunning();
	});
	ipcMain.handle('check-input-monitoring-access', async (): Promise<boolean> => {
		return checkInputMonitoringAccess();
	});
	ipcMain.handle(
		'global-key-hook-register',
		(
			_event,
			options: {
				id: string;
				keycode?: number;
				keyName?: string;
				physicalKeyName?: string;
				mouseButton?: number;
				description?: string;
				ctrl?: boolean;
				alt?: boolean;
				shift?: boolean;
				meta?: boolean;
			},
		): void => {
			let keyName: string | null = options.keyName ?? null;
			if (shouldPreferPhysicalKeyNameForRegistration() && options.physicalKeyName) {
				keyName = options.physicalKeyName;
			}
			if (!keyName && options.keycode != null && options.keycode !== 0) {
				keyName = keycodeToKeyName(options.keycode);
			}
			if (activeBackend === 'evdev') {
				keyName = normalizeEvdevKeyName(keyName);
			}
			let evdevKeycode = 0;
			if (keyName) evdevKeycode = nameToEvdevKeycode(keyName);
			registeredKeybinds.set(options.id, {
				id: options.id,
				description: options.description ?? null,
				keycode: activeBackend === 'evdev' ? evdevKeycode : (options.keycode ?? 0),
				keyName,
				mouseButton: options.mouseButton,
				modifiers: {
					ctrl: options.ctrl ?? false,
					alt: options.alt ?? false,
					shift: options.shift ?? false,
					meta: options.meta ?? false,
				},
			});
		},
	);
	ipcMain.handle('global-key-hook-unregister', (_event, id: string): void => {
		registeredKeybinds.delete(id);
		activeKeybindPresses.delete(id);
	});
	ipcMain.handle('global-key-hook-unregister-all', (): void => {
		registeredKeybinds.clear();
		activeKeybindPresses.clear();
	});
	ipcMain.handle('linux-evdev-status', async (): Promise<LinuxEvdevStatus> => {
		return getLinuxEvdevStatus();
	});
	ipcMain.handle('linux-evdev-grant-access', async (): Promise<LinuxEvdevGrantResult> => {
		return grantLinuxEvdevAccess();
	});
}

export function cleanupGlobalKeyHook(): void {
	void hookLifecycle.forceStop();
	registeredKeybinds.clear();
	activeKeybindPresses.clear();
}
