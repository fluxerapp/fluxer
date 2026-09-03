// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {createRequire} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {
	DESKTOP_APP_NAME,
	LINUX_DESKTOP_ENTRY_ID,
	WINDOWS_APP_USER_MODEL_ID,
	WINDOWS_LEGACY_APP_USER_MODEL_IDS,
} from '@electron/common/DesktopIdentity';
import {isPortableMode} from '@electron/common/UserDataPath';
import {getStableLinuxLaunchPath} from '@electron/main/LinuxLaunchPath';
import {isFlatpakRuntime} from '@electron/main/LinuxSandbox';
import {t} from '@electron/main/MainI18n';
import {app, ipcMain} from 'electron';
import log from 'electron-log';

const requireModule = createRequire(import.meta.url);
const AUTOSTART_INITIALIZED_FILE = 'autostart-initialized-v2';
const FLATPAK_AUTOSTART_STATE_FILE = 'flatpak-autostart-v1.json';

function getInitializedFilePath(): string {
	return path.join(app.getPath('userData'), AUTOSTART_INITIALIZED_FILE);
}

function isInitialized(): boolean {
	try {
		return fs.existsSync(getInitializedFilePath());
	} catch {
		return false;
	}
}

function markInitialized(): void {
	try {
		fs.writeFileSync(getInitializedFilePath(), '1', 'utf8');
	} catch (error) {
		log.error('[Autostart] Failed to mark initialized:', error);
	}
}

function getFlatpakAutostartStatePath(): string {
	return path.join(app.getPath('userData'), FLATPAK_AUTOSTART_STATE_FILE);
}

function readFlatpakAutostartState(): boolean {
	try {
		const raw = fs.readFileSync(getFlatpakAutostartStatePath(), 'utf8');
		const parsed = JSON.parse(raw) as {enabled?: unknown};
		return parsed.enabled === true;
	} catch {
		return false;
	}
}

async function writeFlatpakAutostartState(enabled: boolean): Promise<void> {
	try {
		const statePath = getFlatpakAutostartStatePath();
		await fs.promises.mkdir(path.dirname(statePath), {recursive: true});
		await fs.promises.writeFile(statePath, JSON.stringify({version: 1, enabled, updatedAt: new Date().toISOString()}), {
			encoding: 'utf8',
			mode: 0o600,
		});
	} catch (error) {
		log.warn('[Autostart] Failed to persist Flatpak autostart state:', error);
	}
}

const isMac = process.platform === 'darwin';
const isWindows = process.platform === 'win32';
const isLinux = process.platform === 'linux';
const APP_NAME = DESKTOP_APP_NAME;
const LINUX_DESKTOP_FILE_BASENAME = `${LINUX_DESKTOP_ENTRY_ID}.desktop`;
const LINUX_STARTUP_WM_CLASS = LINUX_DESKTOP_ENTRY_ID;
const AUTOSTART_LAUNCH_ARG = '--autostart';

interface AutoLaunchConfig {
	name: string;
	path: string;
	args: Array<string>;
}

interface WinShellBinding {
	setCurrentUserRunValue: ((options: {name: string; command: string}) => Promise<void>) | null;
	deleteCurrentUserRunValue: ((name: string) => Promise<void>) | null;
	getCurrentUserRunValue: ((name: string) => Promise<string | null>) | null;
	loadError: Error | null;
}

interface LinuxPortalsBinding {
	requestBackground:
		| ((options: {
				reason?: string;
				autostart: boolean;
				commandline?: Array<string>;
				dbusActivatable?: boolean;
		  }) => Promise<{response: number; cancelled: boolean; background: boolean; autostart: boolean}>)
		| null;
	loadError: Error | null;
}

let cachedWinShellBinding: WinShellBinding | null | undefined;
let cachedLinuxPortalsBinding: LinuxPortalsBinding | null | undefined;

function loadWinShell(): WinShellBinding | null {
	if (!isWindows) return null;
	if (cachedWinShellBinding !== undefined) return cachedWinShellBinding;
	try {
		cachedWinShellBinding = requireModule('@fluxer/win-shell') as WinShellBinding;
	} catch (error) {
		log.warn('[Autostart] Failed to load @fluxer/win-shell:', error);
		cachedWinShellBinding = null;
	}
	if (cachedWinShellBinding?.loadError) {
		log.warn('[Autostart] @fluxer/win-shell load error:', cachedWinShellBinding.loadError);
	}
	return cachedWinShellBinding;
}

function loadLinuxPortals(): LinuxPortalsBinding | null {
	if (!isLinux) return null;
	if (cachedLinuxPortalsBinding !== undefined) return cachedLinuxPortalsBinding;
	try {
		cachedLinuxPortalsBinding = requireModule('@fluxer/linux-portals') as LinuxPortalsBinding;
	} catch (error) {
		log.warn('[Autostart] Failed to load @fluxer/linux-portals:', error);
		cachedLinuxPortalsBinding = null;
	}
	if (cachedLinuxPortalsBinding?.loadError) {
		log.warn('[Autostart] @fluxer/linux-portals load error:', cachedLinuxPortalsBinding.loadError);
	}
	return cachedLinuxPortalsBinding;
}

function getWindowsLoginItemPath(): string {
	if (!isWindows) {
		return process.execPath;
	}
	const appFolder = path.dirname(process.execPath);
	const exeName = path.basename(process.execPath);
	const stubLauncher = path.resolve(appFolder, '..', exeName);
	if (app.isPackaged && stubLauncher !== process.execPath && fs.existsSync(stubLauncher)) {
		return stubLauncher;
	}
	return process.execPath;
}

function getAutoLaunchConfig(): AutoLaunchConfig {
	return {
		name: isWindows ? WINDOWS_APP_USER_MODEL_ID : APP_NAME,
		path: isWindows ? getWindowsLoginItemPath() : isLinux ? getStableLinuxLaunchPath() : process.execPath,
		args: [AUTOSTART_LAUNCH_ARG],
	};
}

function getWindowsLoginItemNames(): Array<string> {
	const names = [WINDOWS_APP_USER_MODEL_ID, ...WINDOWS_LEGACY_APP_USER_MODEL_IDS, APP_NAME];
	return [...new Set(names)];
}

function getWindowsAutoLaunchConfigs(): Array<AutoLaunchConfig> {
	if (!isWindows) {
		return [];
	}
	const primary = getAutoLaunchConfig();
	const launchPaths = new Set([primary.path, process.execPath]);
	const configs: Array<AutoLaunchConfig> = [];
	for (const name of getWindowsLoginItemNames()) {
		for (const launchPath of launchPaths) {
			configs.push({
				name,
				path: launchPath,
				args: [AUTOSTART_LAUNCH_ARG],
			});
			configs.push({
				name,
				path: launchPath,
				args: [],
			});
		}
	}
	return configs;
}

function quoteWindowsCommandArg(value: string): string {
	if (value.length > 0 && !/[\s"]/.test(value)) return value;
	let result = '"';
	let backslashes = 0;
	for (const char of value) {
		if (char === '\\') {
			backslashes++;
			continue;
		}
		if (char === '"') {
			result += '\\'.repeat(backslashes * 2 + 1);
			result += '"';
			backslashes = 0;
			continue;
		}
		result += '\\'.repeat(backslashes);
		backslashes = 0;
		result += char;
	}
	result += '\\'.repeat(backslashes * 2);
	result += '"';
	return result;
}

function buildWindowsRunCommand(config: AutoLaunchConfig): string {
	return [config.path, ...config.args].map(quoteWindowsCommandArg).join(' ');
}

function normalizeWindowsLaunchPath(value: string): string {
	return path.normalize(value).toLowerCase();
}

function windowsArgsEqual(left: Array<string>, right: Array<string>): boolean {
	if (left.length !== right.length) return false;
	return left.every((value, index) => value === right[index]);
}

function windowsLaunchItemMatchesConfig(item: Electron.LaunchItems, config: AutoLaunchConfig): boolean {
	return (
		item.name === config.name &&
		normalizeWindowsLaunchPath(item.path) === normalizeWindowsLaunchPath(config.path) &&
		windowsArgsEqual(item.args ?? [], config.args)
	);
}

type WindowsAutostartState = 'enabled' | 'disabled' | 'missing';

function getWindowsConfigState(config: AutoLaunchConfig): WindowsAutostartState {
	const settings = app.getLoginItemSettings({
		path: config.path,
		args: config.args,
	});
	const matchingItem = settings.launchItems?.find((item) => windowsLaunchItemMatchesConfig(item, config));
	if (matchingItem) return matchingItem.enabled ? 'enabled' : 'disabled';
	if (!settings.openAtLogin && !settings.executableWillLaunchAtLogin) return 'missing';
	return 'enabled';
}

async function getWindowsRunValue(name: string): Promise<string | null> {
	const binding = loadWinShell();
	if (!binding?.getCurrentUserRunValue) return null;
	try {
		return await binding.getCurrentUserRunValue(name);
	} catch (error) {
		log.warn('[Autostart] Failed to read Windows Run key:', {name, error});
		return null;
	}
}

async function setWindowsRunValue(config: AutoLaunchConfig): Promise<void> {
	const binding = loadWinShell();
	if (!binding?.setCurrentUserRunValue) return;
	await binding.setCurrentUserRunValue({
		name: config.name,
		command: buildWindowsRunCommand(config),
	});
}

async function deleteWindowsRunValue(name: string): Promise<void> {
	const binding = loadWinShell();
	if (!binding?.deleteCurrentUserRunValue) return;
	await binding.deleteCurrentUserRunValue(name);
}

async function isWindowsAutostartEnabled(): Promise<boolean> {
	for (const config of getWindowsAutoLaunchConfigs()) {
		const state = getWindowsConfigState(config);
		if (state === 'enabled') return true;
		if (state === 'disabled') continue;
		const runValue = await getWindowsRunValue(config.name);
		if (runValue !== null && runValue === buildWindowsRunCommand(config)) return true;
	}
	return false;
}

export function isAutostartLaunch(): boolean {
	if (process.argv.includes(AUTOSTART_LAUNCH_ARG)) {
		return true;
	}
	if (isMac) {
		const settings = app.getLoginItemSettings();
		return Boolean(settings.wasOpenedAtLogin);
	}
	return false;
}

function getLinuxAutostartDir(): string {
	const xdgConfigHome = process.env.XDG_CONFIG_HOME;
	const base = xdgConfigHome && xdgConfigHome.length > 0 ? xdgConfigHome : path.join(os.homedir(), '.config');
	return path.join(base, 'autostart');
}

function getLinuxDesktopFilePath(): string {
	return path.join(getLinuxAutostartDir(), LINUX_DESKTOP_FILE_BASENAME);
}

function escapeDesktopEntry(value: string): string {
	return value.replace(/\\/g, '\\\\').replace(/\n/g, '\\n').replace(/\t/g, '\\t').replace(/\r/g, '\\r');
}

function quoteDesktopExecArg(value: string): string {
	return `"${value.replace(/(["\\$`])/g, '\\$1')}"`;
}

function tryParseDesktopEntry(contents: string): Map<string, string> {
	const entry = new Map<string, string>();
	let inDesktopEntry = false;
	for (const rawLine of contents.split(/\r?\n/)) {
		const line = rawLine.trim();
		if (line.length === 0 || line.startsWith('#')) continue;
		if (line.startsWith('[') && line.endsWith(']')) {
			if (inDesktopEntry) break;
			inDesktopEntry = line === '[Desktop Entry]';
			continue;
		}
		if (!inDesktopEntry) continue;
		const separator = line.indexOf('=');
		if (separator <= 0) continue;
		entry.set(line.slice(0, separator), line.slice(separator + 1));
	}
	return entry;
}

function desktopEntryBoolean(value: string | undefined): boolean {
	return value?.trim().toLowerCase() === 'true';
}

function parseDesktopExecCommand(value: string | undefined): string | null {
	if (!value) return null;
	const input = value.trimStart();
	if (input.length === 0) return null;
	if (input[0] !== '"') {
		const match = /^\S+/.exec(input);
		return match ? match[0] : null;
	}
	let result = '';
	let escaping = false;
	for (let index = 1; index < input.length; index++) {
		const char = input[index];
		if (escaping) {
			result += char;
			escaping = false;
			continue;
		}
		if (char === '\\') {
			escaping = true;
			continue;
		}
		if (char === '"') return result;
		result += char;
	}
	return null;
}

function commandExists(command: string): boolean {
	if (path.isAbsolute(command)) {
		try {
			fs.accessSync(command, fs.constants.X_OK);
			return true;
		} catch {
			return false;
		}
	}
	const pathEnv = process.env.PATH ?? '';
	for (const searchDir of pathEnv.split(path.delimiter)) {
		if (searchDir.length === 0) continue;
		try {
			fs.accessSync(path.join(searchDir, command), fs.constants.X_OK);
			return true;
		} catch {}
	}
	return false;
}

function linuxDesktopEntryTargetsExistingCommand(entry: Map<string, string>): boolean {
	const tryExec = entry.get('TryExec')?.trim();
	if (tryExec) return commandExists(tryExec);
	const execCommand = parseDesktopExecCommand(entry.get('Exec'));
	if (!execCommand) return false;
	return commandExists(execCommand);
}

function buildLinuxDesktopFileContents(): string {
	const execPath = getStableLinuxLaunchPath();
	const execLine = `${quoteDesktopExecArg(execPath)} ${AUTOSTART_LAUNCH_ARG}`;
	return [
		'[Desktop Entry]',
		'Type=Application',
		`Name=${escapeDesktopEntry(APP_NAME)}`,
		'Comment=Fluxer',
		`Exec=${escapeDesktopEntry(execLine)}`,
		`TryExec=${escapeDesktopEntry(execPath)}`,
		'Terminal=false',
		'Categories=Network;InstantMessaging;',
		`StartupWMClass=${LINUX_STARTUP_WM_CLASS}`,
		'SingleMainWindow=true',
		'X-GNOME-Autostart-enabled=true',
		'StartupNotify=false',
		'',
	].join('\n');
}

async function enableLinuxAutostart(): Promise<void> {
	const dir = getLinuxAutostartDir();
	const filePath = getLinuxDesktopFilePath();
	const tempPath = `${filePath}.${process.pid}.tmp`;
	try {
		await fs.promises.mkdir(dir, {recursive: true, mode: 0o700});
		await fs.promises.writeFile(tempPath, buildLinuxDesktopFileContents(), {encoding: 'utf8', mode: 0o644});
		await fs.promises.rename(tempPath, filePath);
		log.info('[Autostart] Wrote Linux autostart entry', {filePath});
	} catch (error) {
		await fs.promises.rm(tempPath, {force: true}).catch(() => {});
		log.error('[Autostart] Failed to write Linux autostart entry:', error);
		throw error;
	}
}

async function disableLinuxAutostart(): Promise<void> {
	const filePath = getLinuxDesktopFilePath();
	try {
		await fs.promises.rm(filePath, {force: true});
		log.info('[Autostart] Removed Linux autostart entry', {filePath});
	} catch (error) {
		log.error('[Autostart] Failed to remove Linux autostart entry:', error);
		throw error;
	}
}

async function removeSandboxLocalLinuxAutostartEntry(): Promise<void> {
	const filePath = getLinuxDesktopFilePath();
	try {
		await fs.promises.rm(filePath, {force: true});
	} catch (error) {
		log.warn('[Autostart] Failed to remove sandbox-local Linux autostart entry:', {filePath, error});
	}
}

function getFlatpakAutostartCommandline(): Array<string> {
	const command = process.env.FLUXER_FLATPAK_COMMAND?.trim() || 'fluxer';
	return [command, AUTOSTART_LAUNCH_ARG];
}

async function setFlatpakAutostart(enabled: boolean): Promise<void> {
	const portals = loadLinuxPortals();
	if (!portals?.requestBackground) {
		throw new Error('XDG Background portal is unavailable.');
	}
	const result = await portals.requestBackground({
		autostart: enabled,
		...(enabled
			? {
					reason: t('desktop.autostart.portalReason', {appName: APP_NAME}),
					commandline: getFlatpakAutostartCommandline(),
				}
			: {}),
	});
	if (result.cancelled) {
		throw new Error('XDG Background portal request was cancelled.');
	}
	if (result.autostart !== enabled) {
		throw new Error(`XDG Background portal did not ${enabled ? 'enable' : 'disable'} autostart.`);
	}
	await removeSandboxLocalLinuxAutostartEntry();
	await writeFlatpakAutostartState(result.autostart);
	log.info('[Autostart] Updated Flatpak autostart through XDG Background portal', {
		enabled: result.autostart,
		background: result.background,
		response: result.response,
	});
}

function isLinuxAutostartEnabled(): boolean {
	if (isFlatpakRuntime()) return readFlatpakAutostartState();
	let contents: string;
	try {
		contents = fs.readFileSync(getLinuxDesktopFilePath(), 'utf8');
	} catch {
		return false;
	}
	const entry = tryParseDesktopEntry(contents);
	if (desktopEntryBoolean(entry.get('Hidden'))) return false;
	return linuxDesktopEntryTargetsExistingCommand(entry);
}

async function enableAutostart(): Promise<void> {
	if (isPortableMode()) {
		log.info('[Autostart] Skipping enable: portable mode is active');
		return;
	}
	const config = getAutoLaunchConfig();
	if (isMac) {
		app.setLoginItemSettings({
			openAtLogin: true,
		});
		return;
	}
	if (isWindows) {
		await setWindowsRunValue(config).catch((error) => {
			log.warn('[Autostart] Native Windows Run-key write failed; falling back to Electron:', error);
		});
		app.setLoginItemSettings({
			openAtLogin: true,
			enabled: true,
			name: config.name,
			path: config.path,
			args: config.args,
		});
		for (const legacyName of getWindowsLoginItemNames().filter((name) => name !== config.name)) {
			app.setLoginItemSettings({
				openAtLogin: false,
				name: legacyName,
			});
			await deleteWindowsRunValue(legacyName).catch((error) => {
				log.warn('[Autostart] Failed to remove legacy Windows Run key:', {legacyName, error});
			});
		}
		return;
	}
	if (isLinux) {
		if (isFlatpakRuntime()) {
			await setFlatpakAutostart(true);
			return;
		}
		await enableLinuxAutostart();
		return;
	}
}

async function disableAutostart(): Promise<void> {
	if (isMac) {
		app.setLoginItemSettings({
			openAtLogin: false,
		});
		return;
	}
	if (isWindows) {
		const seenNames = new Set<string>();
		for (const windowsConfig of getWindowsAutoLaunchConfigs()) {
			if (seenNames.has(windowsConfig.name)) continue;
			seenNames.add(windowsConfig.name);
			app.setLoginItemSettings({
				openAtLogin: false,
				name: windowsConfig.name,
			});
			await deleteWindowsRunValue(windowsConfig.name).catch((error) => {
				log.warn('[Autostart] Failed to remove Windows Run key:', {
					name: windowsConfig.name,
					error,
				});
			});
		}
		return;
	}
	if (isLinux) {
		if (isFlatpakRuntime()) {
			await setFlatpakAutostart(false);
			return;
		}
		await disableLinuxAutostart();
		return;
	}
}

async function isAutostartEnabled(): Promise<boolean> {
	if (isPortableMode()) return false;
	if (isMac) {
		const settings = app.getLoginItemSettings();
		return settings.openAtLogin;
	}
	if (isWindows) {
		return isWindowsAutostartEnabled();
	}
	if (isLinux) {
		return isLinuxAutostartEnabled();
	}
	return false;
}

export function registerAutostartHandlers(): void {
	ipcMain.handle('autostart-enable', async (): Promise<void> => {
		await enableAutostart();
	});
	ipcMain.handle('autostart-disable', async (): Promise<void> => {
		await disableAutostart();
	});
	ipcMain.handle('autostart-is-enabled', async (): Promise<boolean> => {
		return isAutostartEnabled();
	});
	ipcMain.handle('autostart-is-initialized', (): boolean => {
		return isInitialized();
	});
	ipcMain.handle('autostart-mark-initialized', (): void => {
		markInitialized();
	});
}
