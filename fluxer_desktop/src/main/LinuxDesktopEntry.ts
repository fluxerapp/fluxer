// SPDX-License-Identifier: AGPL-3.0-or-later

import child_process from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {APP_PROTOCOL} from '@electron/common/Constants';
import {
	DESKTOP_APP_NAME,
	LEGACY_LINUX_DESKTOP_ENTRY_ID,
	LINUX_DESKTOP_ENTRY_ID,
	LINUX_ICON_NAME,
} from '@electron/common/DesktopIdentity';
import {createChildLogger} from '@electron/common/Logger';
import {TASK_ARG_PREFIX} from '@electron/main/JumpList';
import {getStableLinuxLaunchPath} from '@electron/main/LinuxLaunchPath';
import {isFlatpakRuntime} from '@electron/main/LinuxSandbox';
import {getNativeLocale, t} from '@electron/main/MainI18n';
import {app} from 'electron';

const logger = createChildLogger('LinuxDesktopEntry');
const APP_NAME = DESKTOP_APP_NAME;
const APP_ID = LINUX_DESKTOP_ENTRY_ID;
const WM_CLASS = APP_ID;
const DESKTOP_FILE_BASENAME = `${APP_ID}.desktop`;
const LEGACY_DESKTOP_FILE_BASENAME = `${LEGACY_LINUX_DESKTOP_ENTRY_ID}.desktop`;
const GENERIC_NAME_DEFAULT = 'Instant Messenger';
const COMMENT_DEFAULT = 'Instant messaging and VoIP';
export const GENERATED_MARKER = '# X-Generated-By=fluxer-desktop';
const DESKTOP_ACTIONS = [
	{
		id: 'open-settings',
		nameKey: 'desktop.jumpList.openSettings',
	},
	{
		id: 'new-dm',
		nameKey: 'desktop.jumpList.newDirectMessage',
	},
] as const;
const HICOLOR_ICON_SIZES = [16, 24, 32, 48, 64, 128, 256, 512] as const;

function getXdgDataHome(): string {
	const override = process.env.XDG_DATA_HOME;
	if (override && override.length > 0) return override;
	return path.join(os.homedir(), '.local', 'share');
}

function getXdgDataDirs(): Array<string> {
	const override = process.env.XDG_DATA_DIRS;
	const raw = override && override.length > 0 ? override : '/usr/local/share:/usr/share';
	return raw.split(':').filter((entry) => entry.length > 0);
}

function getUserApplicationsDir(): string {
	return path.join(getXdgDataHome(), 'applications');
}

function getDesktopFilePath(): string {
	return path.join(getUserApplicationsDir(), DESKTOP_FILE_BASENAME);
}

function findSystemDesktopEntry(basename = DESKTOP_FILE_BASENAME): string | null {
	for (const dataDir of getXdgDataDirs()) {
		const candidate = path.join(dataDir, 'applications', basename);
		try {
			if (fs.existsSync(candidate)) return candidate;
		} catch {}
	}
	return null;
}

function findThirdPartyDesktopEntry(execPath: string): string | null {
	const applicationsDir = getUserApplicationsDir();
	let entries: Array<string>;
	try {
		entries = fs.readdirSync(applicationsDir);
	} catch {
		return null;
	}
	for (const entry of entries) {
		if (!entry.endsWith('.desktop') || entry === DESKTOP_FILE_BASENAME) continue;
		const candidate = path.join(applicationsDir, entry);
		try {
			if (fs.readFileSync(candidate, 'utf8').includes(execPath)) return candidate;
		} catch {}
	}
	return null;
}

function escapeDesktopValue(value: string): string {
	return value.replace(/\\/g, '\\\\').replace(/\n/g, '\\n').replace(/\t/g, '\\t').replace(/\r/g, '\\r');
}

function quoteExecArg(value: string): string {
	return `"${value.replace(/(["`$\\])/g, '\\$1')}"`;
}

function installHicolorIcons(): void {
	const iconRoot = path.join(process.resourcesPath, 'icons');
	for (const size of HICOLOR_ICON_SIZES) {
		const source = path.join(iconRoot, `${size}x${size}.png`);
		const target = path.join(getXdgDataHome(), 'icons', 'hicolor', `${size}x${size}`, 'apps', `${LINUX_ICON_NAME}.png`);
		try {
			if (!fs.existsSync(source)) continue;
			fs.mkdirSync(path.dirname(target), {recursive: true});
			fs.copyFileSync(source, target);
		} catch (error) {
			logger.debug('Failed to install Linux hicolor icon', {source, target, error});
		}
	}
}

function buildDesktopActionExecLine(execPath: string, taskId: (typeof DESKTOP_ACTIONS)[number]['id']): string {
	return `${quoteExecArg(execPath)} ${TASK_ARG_PREFIX}${taskId} %U`;
}

function buildDesktopActionEntries(execPath: string): Array<string> {
	const entries = [`Actions=${DESKTOP_ACTIONS.map((action) => action.id).join(';')};`];
	for (const action of DESKTOP_ACTIONS) {
		entries.push(
			'',
			`[Desktop Action ${action.id}]`,
			`Name=${escapeDesktopValue(t(action.nameKey))}`,
			`Exec=${escapeDesktopValue(buildDesktopActionExecLine(execPath, action.id))}`,
		);
	}
	return entries;
}

function buildLocalizedEntryLines(key: string, defaultValue: string, localizedValue: string): Array<string> {
	const lines = [`${key}=${escapeDesktopValue(defaultValue)}`];
	if (localizedValue !== defaultValue) {
		const entryLocale = getNativeLocale().replace(/-/g, '_');
		lines.push(`${key}[${entryLocale}]=${escapeDesktopValue(localizedValue)}`);
	}
	return lines;
}

export function buildDesktopFileContents(execPath: string, hidden: boolean): string {
	const execLine = `${quoteExecArg(execPath)} %U`;
	return [
		'[Desktop Entry]',
		GENERATED_MARKER,
		'Type=Application',
		'Version=1.5',
		`Name=${escapeDesktopValue(APP_NAME)}`,
		...buildLocalizedEntryLines('GenericName', GENERIC_NAME_DEFAULT, t('desktop.linuxEntry.genericName')),
		...buildLocalizedEntryLines('Comment', COMMENT_DEFAULT, t('desktop.linuxEntry.comment')),
		`Exec=${escapeDesktopValue(execLine)}`,
		`TryExec=${escapeDesktopValue(execPath)}`,
		`Icon=${escapeDesktopValue(LINUX_ICON_NAME)}`,
		'Terminal=false',
		'Categories=Network;InstantMessaging;Chat;',
		`MimeType=x-scheme-handler/${APP_PROTOCOL};`,
		`StartupWMClass=${WM_CLASS}`,
		'SingleMainWindow=true',
		'StartupNotify=true',
		...(hidden ? ['NoDisplay=true'] : []),
		...buildDesktopActionEntries(execPath),
		'',
	].join('\n');
}

function readDesktopEntryValue(contents: string, key: string): string | null {
	for (const line of contents.split('\n')) {
		const trimmed = line.trim();
		if (trimmed.startsWith('[Desktop Action ')) break;
		if (trimmed.startsWith(`${key}=`)) return trimmed.slice(key.length + 1).trim();
	}
	return null;
}

function isExecutableFile(candidate: string): boolean {
	try {
		if (!fs.statSync(candidate).isFile()) return false;
		fs.accessSync(candidate, fs.constants.X_OK);
		return true;
	} catch {
		return false;
	}
}

function isStaleDesktopFile(contents: string): boolean {
	const tryExec = readDesktopEntryValue(contents, 'TryExec');
	if (tryExec === null || !tryExec.startsWith('/')) return false;
	return !isExecutableFile(tryExec);
}

function readExistingDesktopFile(filePath: string): string | null {
	try {
		return fs.readFileSync(filePath, 'utf8');
	} catch {
		return null;
	}
}

function runUpdateDesktopDatabase(applicationsDir: string): void {
	child_process.execFile('update-desktop-database', [applicationsDir], {timeout: 5000}, (error) => {
		if (error) {
			logger.debug('update-desktop-database returned non-zero or was not found', {
				message: error.message,
			});
		}
	});
}

function registerProtocolClient(): void {
	try {
		app.setAsDefaultProtocolClient(APP_PROTOCOL);
	} catch (error) {
		logger.warn('Failed to register protocol client', {error});
	}
}

function removeFile(filePath: string, reason: string): boolean {
	try {
		fs.unlinkSync(filePath);
		logger.info(reason, {filePath});
		return true;
	} catch (error) {
		logger.warn('Failed to remove .desktop entry', {filePath, error});
		return false;
	}
}

function removeLegacyGeneratedDesktopEntry(): boolean {
	const legacyPath = path.join(getUserApplicationsDir(), LEGACY_DESKTOP_FILE_BASENAME);
	const contents = readExistingDesktopFile(legacyPath);
	if (contents === null || !contents.includes(GENERATED_MARKER)) return false;
	return removeFile(legacyPath, 'Removed the generated .desktop entry for the previous desktop id');
}

function writeDesktopFileAtomically(filePath: string, contents: string): void {
	const tempPath = `${filePath}.${process.pid}.tmp`;
	try {
		fs.mkdirSync(path.dirname(filePath), {recursive: true});
		fs.writeFileSync(tempPath, contents, {encoding: 'utf8', mode: 0o644});
		fs.renameSync(tempPath, filePath);
	} catch (error) {
		try {
			fs.rmSync(tempPath, {force: true});
		} catch {}
		throw error;
	}
}

function syncUserDesktopEntry(): boolean {
	const execPath = getStableLinuxLaunchPath();
	const filePath = getDesktopFilePath();
	const existing = readExistingDesktopFile(filePath);
	const systemEntry = findSystemDesktopEntry();
	if (systemEntry) {
		if (existing !== null && (existing.includes(GENERATED_MARKER) || isStaleDesktopFile(existing))) {
			return removeFile(filePath, 'Removed a user .desktop entry shadowing the system entry');
		}
		logger.debug('System-wide .desktop entry detected; skipping user-local copy', {systemEntry});
		return false;
	}
	if (existing !== null && !existing.includes(GENERATED_MARKER) && !isStaleDesktopFile(existing)) {
		logger.debug('Linux .desktop entry was hand-edited; leaving untouched', {filePath});
		return false;
	}
	installHicolorIcons();
	const thirdPartyEntry = findThirdPartyDesktopEntry(execPath) ?? findSystemDesktopEntry(LEGACY_DESKTOP_FILE_BASENAME);
	if (thirdPartyEntry) {
		logger.debug('Another .desktop entry manages the app menu entry; keeping ours hidden', {thirdPartyEntry});
	}
	const desired = buildDesktopFileContents(execPath, thirdPartyEntry !== null);
	if (existing === desired) {
		logger.debug('Linux .desktop entry already up to date', {filePath});
		return false;
	}
	writeDesktopFileAtomically(filePath, desired);
	logger.info('Wrote Linux .desktop entry', {filePath, execPath});
	return true;
}

function desktopEntryResolves(): boolean {
	return findSystemDesktopEntry() !== null || readExistingDesktopFile(getDesktopFilePath()) !== null;
}

export function ensureLinuxDesktopEntry(): boolean {
	if (process.platform !== 'linux') return false;
	if (isFlatpakRuntime()) {
		logger.debug('Skipping .desktop entry creation in Flatpak; package export owns launcher/protocol integration');
		registerProtocolClient();
		return false;
	}
	if (process.env.FLUXER_DISABLE_DESKTOP_FILE === '1') {
		logger.debug('Skipping .desktop entry management; FLUXER_DISABLE_DESKTOP_FILE=1');
		if (findSystemDesktopEntry() !== null) registerProtocolClient();
		return desktopEntryResolves();
	}
	let changed = removeLegacyGeneratedDesktopEntry();
	try {
		changed = syncUserDesktopEntry() || changed;
	} catch (error) {
		logger.warn('Failed to write Linux .desktop entry; deep links and portals may not resolve the app', {
			filePath: getDesktopFilePath(),
			error,
		});
	}
	if (changed) runUpdateDesktopDatabase(getUserApplicationsDir());
	const resolves = desktopEntryResolves();
	if (resolves) registerProtocolClient();
	return resolves;
}
