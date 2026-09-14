// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {createRequire} from 'node:module';
import path from 'node:path';
import {
	DESKTOP_APP_NAME,
	WINDOWS_APP_USER_MODEL_ID,
	WINDOWS_LEGACY_APP_USER_MODEL_IDS,
	WINDOWS_SHORTCUT_AUTHOR,
	WINDOWS_TOAST_ACTIVATOR_CLSID,
} from '@electron/common/DesktopIdentity';

const requireModule = createRequire(import.meta.url);

interface WindowsShortcutRepairPaths {
	authorShortcut: string;
	currentDir: string;
	currentExe: string;
	rootAppDir: string;
	rootShortcut: string;
}

interface CreateShortcutOptions {
	lnkPath: string;
	target: string;
	args?: string;
	appUserModelId?: string;
	toastActivatorClsid?: string;
	iconPath?: string;
	iconIndex?: number;
	workingDir?: string;
	description?: string;
}

function getProgramsDir(): string | null {
	const appData = process.env.APPDATA;
	if (!appData) return null;
	return path.join(appData, 'Microsoft', 'Windows', 'Start Menu', 'Programs');
}

function getWindowsShortcutRepairPaths(): WindowsShortcutRepairPaths | null {
	const currentExe = process.execPath;
	const currentDir = path.dirname(currentExe);
	if (path.basename(currentDir).toLowerCase() !== 'current') {
		return null;
	}
	const rootAppDir = path.dirname(currentDir);
	if (!fs.existsSync(path.join(rootAppDir, 'Update.exe'))) {
		return null;
	}
	const programsDir = getProgramsDir();
	if (!programsDir) return null;
	const shortcutName = `${DESKTOP_APP_NAME}.lnk`;
	return {
		authorShortcut: path.join(programsDir, WINDOWS_SHORTCUT_AUTHOR, shortcutName),
		currentDir,
		currentExe,
		rootAppDir,
		rootShortcut: path.join(programsDir, shortcutName),
	};
}

function isInFluxerRoot(candidate: string, rootDir: string): boolean {
	if (!candidate) return false;
	let fullPath: string;
	try {
		fullPath = path.resolve(candidate);
	} catch {
		return false;
	}
	const normalized = fullPath.replace(/\\+$/, '');
	const normalizedRoot = rootDir.replace(/\\+$/, '');
	return (
		normalized.toLowerCase() === normalizedRoot.toLowerCase() ||
		normalized.toLowerCase().startsWith(normalizedRoot.toLowerCase() + path.sep)
	);
}

interface ShellLinkBinding {
	createShortcut: ((opts: CreateShortcutOptions) => Promise<void>) | null;
	loadError: Error | null;
}

let cachedBinding: ShellLinkBinding | null = null;

function loadWinShell(): ShellLinkBinding | null {
	if (cachedBinding) return cachedBinding;
	try {
		cachedBinding = requireModule('@fluxer/win-shell') as ShellLinkBinding;
	} catch (error) {
		console.warn('[WindowsShortcuts] Failed to load @fluxer/win-shell', error);
		return null;
	}
	if (cachedBinding.loadError) {
		console.warn('[WindowsShortcuts] @fluxer/win-shell load error', cachedBinding.loadError);
	}
	return cachedBinding;
}

function lnkContainsString(lnkPath: string, needle: string): boolean {
	let buf: Buffer;
	try {
		buf = fs.readFileSync(lnkPath);
	} catch {
		return false;
	}
	const utf16 = Buffer.from(needle, 'utf16le');
	if (buf.includes(utf16)) return true;
	return buf.includes(Buffer.from(needle, 'utf8'));
}

async function repairOneShortcut(
	shortcutPath: string,
	repair: WindowsShortcutRepairPaths,
	createShortcut: (opts: CreateShortcutOptions) => Promise<void>,
): Promise<void> {
	if (!fs.existsSync(shortcutPath)) return;
	const rootDir = path.resolve(repair.rootAppDir);
	const alreadyCurrent = lnkContainsString(shortcutPath, repair.currentExe);
	const pointsAtFluxer = lnkContainsString(shortcutPath, rootDir) || isInFluxerRoot(shortcutPath, rootDir);
	const hasLegacyAumid = WINDOWS_LEGACY_APP_USER_MODEL_IDS.some((legacyAumid) =>
		lnkContainsString(shortcutPath, legacyAumid),
	);
	if (!pointsAtFluxer) return;
	if (alreadyCurrent && !hasLegacyAumid) return;
	try {
		await createShortcut({
			lnkPath: shortcutPath,
			target: repair.currentExe,
			appUserModelId: WINDOWS_APP_USER_MODEL_ID,
			toastActivatorClsid: WINDOWS_TOAST_ACTIVATOR_CLSID,
			workingDir: repair.currentDir,
			iconPath: repair.currentExe,
			iconIndex: 0,
		});
	} catch (error) {
		console.warn('[WindowsShortcuts] Failed to rewrite shortcut', {shortcutPath, error});
	}
}

async function repairWindowsShortcutsAsync(repairPaths: WindowsShortcutRepairPaths): Promise<void> {
	const binding = loadWinShell();
	if (!binding || !binding.createShortcut) {
		console.warn('[WindowsShortcuts] @fluxer/win-shell unavailable; skipping shortcut repair');
		return;
	}
	const {createShortcut} = binding;
	const shortcutPaths: Array<string> = [repairPaths.authorShortcut, repairPaths.rootShortcut];
	const desktopDir = process.env.USERPROFILE ? path.join(process.env.USERPROFILE, 'Desktop') : '';
	if (desktopDir) shortcutPaths.push(path.join(desktopDir, `${DESKTOP_APP_NAME}.lnk`));
	if (process.env.APPDATA) {
		const pinnedDir = path.join(
			process.env.APPDATA,
			'Microsoft',
			'Internet Explorer',
			'Quick Launch',
			'User Pinned',
			'TaskBar',
		);
		shortcutPaths.push(path.join(pinnedDir, `${DESKTOP_APP_NAME}.lnk`));
	}
	for (const shortcutPath of shortcutPaths) {
		await repairOneShortcut(shortcutPath, repairPaths, createShortcut);
	}
}

export function repairWindowsShortcuts(): void {
	if (process.platform !== 'win32') return;
	const repairPaths = getWindowsShortcutRepairPaths();
	if (!repairPaths) return;
	repairWindowsShortcutsAsync(repairPaths).catch((error) => {
		console.warn('[WindowsShortcuts] Failed to repair Fluxer shortcuts', error);
	});
}
