// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import {createRequire} from 'node:module';
import {getLinuxInputHookMode} from '@electron/main/LaunchOptions';

const requireModule = createRequire(import.meta.url);

export interface EvdevKeyEvent {
	type: 'keydown' | 'keyup';
	keycode: number;
	keyName: string;
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

export interface EvdevMouseEvent {
	type: 'mousedown' | 'mouseup';
	button: number;
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

type NativeEvent = EvdevKeyEvent | EvdevMouseEvent;
type NativeEvdevHookCtor = new (
	onEvent: (event: NativeEvent) => void,
) => {
	start(): boolean;
	stop(): void;
};
interface NativeEvdevHookInstance {
	start(): boolean;
	stop(): void;
}
type NativeEvdevModule = {
	EvdevHook: NativeEvdevHookCtor | null;
	nameToEvdevKeycode: ((name: string) => number) | null;
	loadError: Error | null;
};

function loadNativeModule(): NativeEvdevModule | null {
	if (process.platform !== 'linux') return null;
	if (getLinuxInputHookMode(process.argv) === 'off' || getLinuxInputHookMode(process.argv) === 'native') return null;
	let required: NativeEvdevModule;
	try {
		required = requireModule('@fluxer/linux-evdev') as NativeEvdevModule;
	} catch (error) {
		throw new Error(
			`@fluxer/linux-evdev failed to load on Linux — this is a packaging bug, not a runtime fallback case. ` +
				`Original error: ${error instanceof Error ? error.message : String(error)}`,
		);
	}
	if (!required.EvdevHook || !required.nameToEvdevKeycode) {
		throw new Error(
			`@fluxer/linux-evdev loaded but exports are missing — native binary did not register module. ` +
				`Underlying loadError: ${required.loadError ? required.loadError.message : '<none>'}`,
		);
	}
	return required;
}

let nativeModuleCache: NativeEvdevModule | null | undefined;

function getNativeModule(): NativeEvdevModule | null {
	if (nativeModuleCache !== undefined) return nativeModuleCache;
	nativeModuleCache = loadNativeModule();
	return nativeModuleCache;
}

class EvdevHook extends EventEmitter {
	private nativeInstance: NativeEvdevHookInstance | null = null;
	private started = false;

	async start(): Promise<boolean> {
		if (this.started) return true;
		const nativeModule = getNativeModule();
		if (!nativeModule?.EvdevHook) {
			return false;
		}
		const Native = nativeModule.EvdevHook;
		const instance = new Native((event) => this.dispatch(event));
		const opened = instance.start();
		if (!opened) {
			instance.stop();
			return false;
		}
		this.nativeInstance = instance;
		this.started = true;
		return true;
	}

	stop(): void {
		if (!this.started) return;
		this.started = false;
		const instance = this.nativeInstance;
		this.nativeInstance = null;
		if (instance) {
			try {
				instance.stop();
			} catch {}
		}
	}

	private dispatch(event: NativeEvent): void {
		if (event.type === 'keydown' || event.type === 'keyup') {
			this.emit('key', event);
		} else {
			this.emit('mouse', event);
		}
	}
}

let singleton: EvdevHook | null = null;

export function getEvdevHook(): EvdevHook {
	if (!singleton) singleton = new EvdevHook();
	return singleton;
}

export function nameToEvdevKeycode(name: string | undefined | null): number {
	if (!name) return 0;
	const nativeModule = getNativeModule();
	if (nativeModule?.nameToEvdevKeycode) {
		return nativeModule.nameToEvdevKeycode(name);
	}
	return 0;
}
