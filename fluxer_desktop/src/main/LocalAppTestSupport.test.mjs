// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerHooks} from 'node:module';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const DESKTOP_SRC = new URL('../', import.meta.url);
const PACKAGES = new URL('../../../packages/', import.meta.url);
const APP_SRC = new URL('../../../fluxer_app/src/', import.meta.url);

export const REPOSITORY_ROOT = fileURLToPath(new URL('../../../', import.meta.url));

const DESKTOP_DIRECTORY = fileURLToPath(new URL('../../', import.meta.url));

const ELECTRON_EXPORT_NAMES = Object.freeze([
	'app',
	'autoUpdater',
	'BaseWindow',
	'BrowserWindow',
	'clipboard',
	'contextBridge',
	'crashReporter',
	'desktopCapturer',
	'dialog',
	'globalShortcut',
	'inAppPurchase',
	'ipcMain',
	'ipcRenderer',
	'Menu',
	'MenuItem',
	'nativeImage',
	'nativeTheme',
	'net',
	'netLog',
	'Notification',
	'powerMonitor',
	'powerSaveBlocker',
	'protocol',
	'safeStorage',
	'screen',
	'session',
	'shell',
	'systemPreferences',
	'Tray',
	'utilityProcess',
	'webContents',
	'webFrame',
	'webFrameMain',
]);

const LIVE_STATE_KEY = '__fluxerDesktopLocalAppTestState__';

const state = {electron: {}, desktopAppStorage: null};

globalThis[LIVE_STATE_KEY] = state;

function dataModule(source) {
	return `data:text/javascript,${encodeURIComponent(source)}`;
}

const ELECTRON_STUB = dataModule(`const state = globalThis[${JSON.stringify(LIVE_STATE_KEY)}];
const live = (name) =>
	new Proxy(function electronStub() {}, {
		get: (_target, property) => Reflect.get(state.electron[name], property),
		set: (_target, property, value) => Reflect.set(state.electron[name], property, value),
		apply: (_target, thisArgument, args) => Reflect.apply(state.electron[name], thisArgument, args),
		construct: (_target, args) => Reflect.construct(state.electron[name], args),
	});
export default new Proxy({}, {get: (_target, property) => state.electron[property]});
${ELECTRON_EXPORT_NAMES.map((name) => `export const ${name} = live(${JSON.stringify(name)});`).join('\n')}`);

const ELECTRON_LOG_STUB = dataModule(`const noop = () => undefined;
const log = {debug: noop, info: noop, warn: noop, error: noop, transports: {file: {}, console: {}}};
export default log;
export const {debug, info, warn, error, transports} = log;`);

const DESKTOP_APP_STORAGE_STUB = dataModule(`const state = globalThis[${JSON.stringify(LIVE_STATE_KEY)}];
export const getDesktopAppStorage = () => state.desktopAppStorage;`);

const WINDOW_STUB = dataModule(`const state = globalThis[${JSON.stringify(LIVE_STATE_KEY)}];
export const isAppDocumentWindowContents = (contents) => state.mainWindow?.webContents === contents;
export const isTrustedOrigin = () => true;`);

const APP_LOGGER_STUB = dataModule(`const noop = () => undefined;
export class Logger {
	debug = noop;
	info = noop;
	warn = noop;
	error = noop;
	log = noop;
}`);

const APP_REST_TRANSPORT_STUB = dataModule(`const unavailable = () => {
	throw new Error('The renderer REST transport is not available in a main-process test');
};
export const http = new Proxy({}, {get: () => unavailable});`);

const APP_NATIVE_UTILS_STUB = dataModule('export const getElectronAPI = () => null;');

const STUB_MODULES = new Map([
	['electron', ELECTRON_STUB],
	['electron-log', ELECTRON_LOG_STUB],
	['@app/features/platform/transport/RestTransport', APP_REST_TRANSPORT_STUB],
	['@app/features/platform/utils/AppLogger', APP_LOGGER_STUB],
	['@app/features/ui/utils/NativeUtils', APP_NATIVE_UTILS_STUB],
]);

registerHooks({
	resolve(specifier, context, nextResolve) {
		const stub = STUB_MODULES.get(specifier);
		if (stub != null) {
			return {shortCircuit: true, url: stub};
		}
		if (specifier.startsWith('@app/')) {
			return {shortCircuit: true, url: new URL(`${specifier.slice('@app/'.length)}.ts`, APP_SRC).href};
		}
		if (specifier.startsWith('@fluxer/')) {
			return {shortCircuit: true, url: new URL(`${specifier.slice('@fluxer/'.length)}.ts`, PACKAGES).href};
		}
		if (specifier.startsWith('@electron/')) {
			return {shortCircuit: true, url: new URL(`${specifier.slice('@electron/'.length)}.ts`, DESKTOP_SRC).href};
		}
		try {
			return nextResolve(specifier, context);
		} catch (error) {
			if (error?.code !== 'ERR_MODULE_NOT_FOUND' || /\.[a-z]+$/u.test(specifier)) {
				throw error;
			}
			return nextResolve(`${specifier}.ts`, context);
		}
	},
});

const APP_STUB_DEFAULTS = {
	isReady: () => true,
	getAppPath: () => DESKTOP_DIRECTORY,
	getPath: () => path.join(DESKTOP_DIRECTORY, '.test-userdata'),
};

export function installElectronStub(extra = {}) {
	const registry = {contents: new Map(), frames: new Map()};
	const webContents = {
		fromId: (id) => registry.contents.get(id) ?? null,
		fromFrame: (frame) => (frame == null ? null : (frame.owner ?? null)),
	};
	const webFrameMain = {
		fromId: (processId, routingId) => registry.frames.get(`${processId}:${routingId}`) ?? null,
	};
	const app = extra.app ?? {};
	for (const [key, value] of Object.entries(APP_STUB_DEFAULTS)) {
		if (!(key in app)) {
			app[key] = value;
		}
	}
	const nativeTheme = {themeSource: 'system'};
	const session = {defaultSession: {resolveProxy: async () => 'DIRECT'}};
	state.electron = {nativeTheme, session, webContents, webFrameMain, ...extra, app};
	return registry;
}

export function installTestModuleStub(specifier, source) {
	STUB_MODULES.set(specifier, dataModule(source));
}

export function installDesktopAppStorageStub(storage) {
	STUB_MODULES.set('@electron/main/DesktopAppStorage', DESKTOP_APP_STORAGE_STUB);
	state.desktopAppStorage = storage;
}

export function installWindowStub(mainWindow) {
	STUB_MODULES.set('@electron/main/Window', WINDOW_STUB);
	state.mainWindow = mainWindow;
}
