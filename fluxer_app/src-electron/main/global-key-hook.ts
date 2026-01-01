/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createRequire} from 'node:module';
import {ipcMain} from 'electron';
import type {UiohookKeyboardEvent, UiohookMouseEvent} from 'uiohook-napi';
import {getMainWindow} from './window.js';

let uIOhook: typeof import('uiohook-napi').uIOhook | null = null;
let UiohookKey: typeof import('uiohook-napi').UiohookKey | null = null;

interface KeybindRegistration {
	id: string;
	keycode: number;
	mouseButton?: number;
	modifiers: {
		ctrl: boolean;
		alt: boolean;
		shift: boolean;
		meta: boolean;
	};
}

const registeredKeybinds = new Map<string, KeybindRegistration>();
const activeKeys = new Set<number>();
let hookStarted = false;
const requireModule = createRequire(import.meta.url);

function keycodeToKeyName(keycode: number): string {
	if (!UiohookKey) return `Key${keycode}`;

	const keyMap: Record<number, string> = {
		[UiohookKey.Escape]: 'Escape',
		[UiohookKey.F1]: 'F1',
		[UiohookKey.F2]: 'F2',
		[UiohookKey.F3]: 'F3',
		[UiohookKey.F4]: 'F4',
		[UiohookKey.F5]: 'F5',
		[UiohookKey.F6]: 'F6',
		[UiohookKey.F7]: 'F7',
		[UiohookKey.F8]: 'F8',
		[UiohookKey.F9]: 'F9',
		[UiohookKey.F10]: 'F10',
		[UiohookKey.F11]: 'F11',
		[UiohookKey.F12]: 'F12',
		[UiohookKey.Backquote]: 'Backquote',
		[UiohookKey['1']]: '1',
		[UiohookKey['2']]: '2',
		[UiohookKey['3']]: '3',
		[UiohookKey['4']]: '4',
		[UiohookKey['5']]: '5',
		[UiohookKey['6']]: '6',
		[UiohookKey['7']]: '7',
		[UiohookKey['8']]: '8',
		[UiohookKey['9']]: '9',
		[UiohookKey['0']]: '0',
		[UiohookKey.Minus]: 'Minus',
		[UiohookKey.Equal]: 'Equal',
		[UiohookKey.Backspace]: 'Backspace',
		[UiohookKey.Tab]: 'Tab',
		[UiohookKey.Q]: 'Q',
		[UiohookKey.W]: 'W',
		[UiohookKey.E]: 'E',
		[UiohookKey.R]: 'R',
		[UiohookKey.T]: 'T',
		[UiohookKey.Y]: 'Y',
		[UiohookKey.U]: 'U',
		[UiohookKey.I]: 'I',
		[UiohookKey.O]: 'O',
		[UiohookKey.P]: 'P',
		[UiohookKey.BracketLeft]: 'BracketLeft',
		[UiohookKey.BracketRight]: 'BracketRight',
		[UiohookKey.Backslash]: 'Backslash',
		[UiohookKey.CapsLock]: 'CapsLock',
		[UiohookKey.A]: 'A',
		[UiohookKey.S]: 'S',
		[UiohookKey.D]: 'D',
		[UiohookKey.F]: 'F',
		[UiohookKey.G]: 'G',
		[UiohookKey.H]: 'H',
		[UiohookKey.J]: 'J',
		[UiohookKey.K]: 'K',
		[UiohookKey.L]: 'L',
		[UiohookKey.Semicolon]: 'Semicolon',
		[UiohookKey.Quote]: 'Quote',
		[UiohookKey.Enter]: 'Enter',
		[UiohookKey.Shift]: 'ShiftLeft',
		[UiohookKey.Z]: 'Z',
		[UiohookKey.X]: 'X',
		[UiohookKey.C]: 'C',
		[UiohookKey.V]: 'V',
		[UiohookKey.B]: 'B',
		[UiohookKey.N]: 'N',
		[UiohookKey.M]: 'M',
		[UiohookKey.Comma]: 'Comma',
		[UiohookKey.Period]: 'Period',
		[UiohookKey.Slash]: 'Slash',
		[UiohookKey.ShiftRight]: 'ShiftRight',
		[UiohookKey.Ctrl]: 'ControlLeft',
		[UiohookKey.Meta]: 'MetaLeft',
		[UiohookKey.Alt]: 'AltLeft',
		[UiohookKey.Space]: 'Space',
		[UiohookKey.AltRight]: 'AltRight',
		[UiohookKey.MetaRight]: 'MetaRight',
		[UiohookKey.CtrlRight]: 'ControlRight',
		[UiohookKey.ArrowLeft]: 'ArrowLeft',
		[UiohookKey.ArrowUp]: 'ArrowUp',
		[UiohookKey.ArrowRight]: 'ArrowRight',
		[UiohookKey.ArrowDown]: 'ArrowDown',
		[UiohookKey.Insert]: 'Insert',
		[UiohookKey.Delete]: 'Delete',
		[UiohookKey.Home]: 'Home',
		[UiohookKey.End]: 'End',
		[UiohookKey.PageUp]: 'PageUp',
		[UiohookKey.PageDown]: 'PageDown',
	};

	return keyMap[keycode] ?? `Key${keycode}`;
}

function handleKeyEvent(event: UiohookKeyboardEvent, type: 'keydown' | 'keyup') {
	const mainWindow = getMainWindow();
	if (!mainWindow) return;

	const keycode = event.keycode;
	const keyName = keycodeToKeyName(keycode);

	if (type === 'keydown') {
		activeKeys.add(keycode);
	} else {
		activeKeys.delete(keycode);
	}

	mainWindow.webContents.send('global-key-event', {
		type,
		keycode,
		keyName,
		altKey: event.altKey,
		ctrlKey: event.ctrlKey,
		shiftKey: event.shiftKey,
		metaKey: event.metaKey,
	});

	for (const [id, keybind] of registeredKeybinds) {
		if (keybind.keycode === keycode) {
			const modifiersMatch =
				keybind.modifiers.ctrl === event.ctrlKey &&
				keybind.modifiers.alt === event.altKey &&
				keybind.modifiers.shift === event.shiftKey &&
				keybind.modifiers.meta === event.metaKey;

			if (modifiersMatch || !Object.values(keybind.modifiers).some(Boolean)) {
				mainWindow.webContents.send('global-keybind-triggered', {
					id,
					type,
				});
			}
		}
	}
}

function handleMouseEvent(event: UiohookMouseEvent, type: 'mousedown' | 'mouseup') {
	const mainWindow = getMainWindow();
	if (!mainWindow) return;

	const button = event.button;

	mainWindow.webContents.send('global-mouse-event', {
		type,
		button,
	});

	for (const [id, keybind] of registeredKeybinds) {
		if (keybind.mouseButton === button) {
			mainWindow.webContents.send('global-keybind-triggered', {
				id,
				type: type === 'mousedown' ? 'keydown' : 'keyup',
			});
		}
	}
}

async function startHook(): Promise<boolean> {
	if (hookStarted) return true;

	try {
		const uiohookModule = await import('uiohook-napi');
		uIOhook = uiohookModule.uIOhook;
		UiohookKey = uiohookModule.UiohookKey;

		uIOhook.on('keydown', (event) => handleKeyEvent(event, 'keydown'));
		uIOhook.on('keyup', (event) => handleKeyEvent(event, 'keyup'));
		uIOhook.on('mousedown', (event) => handleMouseEvent(event, 'mousedown'));
		uIOhook.on('mouseup', (event) => handleMouseEvent(event, 'mouseup'));

		uIOhook.start();
		hookStarted = true;
		return true;
	} catch (error) {
		console.error('[GlobalKeyHook] Failed to start:', error);
		return false;
	}
}

const INPUT_MONITORING_PERMISSION = 'input-monitoring';
const INPUT_MONITORING_STATUS_ALLOWLIST = new Set(['authorized', 'not-determined']);

function getInputMonitoringStatus(): string | null {
	const permissionsModule = (() => {
		try {
			return requireModule('node-mac-permissions');
		} catch (error) {
			console.error('[GlobalKeyHook] Failed to load node-mac-permissions:', error);
			return null;
		}
	})();

	if (!permissionsModule || typeof permissionsModule.getAuthStatus !== 'function') {
		return null;
	}

	try {
		return permissionsModule.getAuthStatus(INPUT_MONITORING_PERMISSION);
	} catch (error) {
		console.error('[GlobalKeyHook] Failed to query Input Monitoring auth status:', error);
		return null;
	}
}

async function checkInputMonitoringAccess(): Promise<boolean> {
	if (process.platform !== 'darwin') {
		return true;
	}

	if (hookStarted) {
		return true;
	}

	const status = getInputMonitoringStatus();
	if (status === null) {
		return true;
	}

	if (INPUT_MONITORING_STATUS_ALLOWLIST.has(status)) {
		return true;
	}

	console.warn('[GlobalKeyHook] Input Monitoring access denied, status:', status);
	return false;
}

function stopHook(): void {
	if (!hookStarted || !uIOhook) return;

	try {
		uIOhook.stop();
		hookStarted = false;
	} catch (error) {
		console.error('[GlobalKeyHook] Failed to stop:', error);
	}
}

export function registerGlobalKeyHookHandlers(): void {
	ipcMain.handle('global-key-hook-start', async (): Promise<boolean> => {
		return startHook();
	});

	ipcMain.handle('global-key-hook-stop', (): void => {
		stopHook();
	});

	ipcMain.handle('global-key-hook-is-running', (): boolean => {
		return hookStarted;
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
				mouseButton?: number;
				ctrl?: boolean;
				alt?: boolean;
				shift?: boolean;
				meta?: boolean;
			},
		): void => {
			registeredKeybinds.set(options.id, {
				id: options.id,
				keycode: options.keycode ?? 0,
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
	});

	ipcMain.handle('global-key-hook-unregister-all', (): void => {
		registeredKeybinds.clear();
	});
}

export function cleanupGlobalKeyHook(): void {
	stopHook();
	registeredKeybinds.clear();
	activeKeys.clear();
}
