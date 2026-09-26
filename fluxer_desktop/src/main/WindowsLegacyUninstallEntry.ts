// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import {WINDOWS_LEGACY_SQUIRREL_ID, WINDOWS_VELOPACK_ID} from '@electron/common/DesktopIdentity';
import log from 'electron-log';

const UNINSTALL_REGISTRY_KEY = 'HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall';

interface RegResult {
	status: number;
	stdout: string;
}

function runReg(args: Array<string>): Promise<RegResult> {
	return new Promise((resolve, reject) => {
		execFile('reg.exe', args, {encoding: 'utf8', windowsHide: true}, (error, stdout) => {
			if (!error) {
				resolve({status: 0, stdout});
				return;
			}
			if (typeof error.code === 'number') {
				resolve({status: error.code, stdout});
				return;
			}
			reject(error);
		});
	});
}

function normalizeWindowsPath(target: string): string {
	return path.resolve(target).replace(/\\+$/, '').toLowerCase();
}

function isRunningFromMigratedSquirrelRoot(): boolean {
	const localAppData = process.env.LOCALAPPDATA;
	if (!localAppData) return false;
	const currentDir = path.dirname(process.execPath);
	if (path.basename(currentDir).toLowerCase() !== 'current') return false;
	const rootAppDir = path.dirname(currentDir);
	if (normalizeWindowsPath(rootAppDir) !== normalizeWindowsPath(path.join(localAppData, WINDOWS_LEGACY_SQUIRREL_ID))) {
		return false;
	}
	return fs.existsSync(path.join(rootAppDir, 'Update.exe'));
}

function uninstallKey(appId: string): string {
	return `${UNINSTALL_REGISTRY_KEY}\\${appId}`;
}

function parseInstallLocation(stdout: string): string | null {
	const match = stdout.match(/^\s*InstallLocation\s+REG_(?:EXPAND_)?SZ\s+(.*?)\s*$/im);
	return match ? match[1] : null;
}

async function velopackEntryDescribesLegacyRoot(): Promise<boolean> {
	const {status, stdout} = await runReg(['query', uninstallKey(WINDOWS_VELOPACK_ID), '/v', 'InstallLocation']);
	if (status !== 0) return false;
	const installLocation = parseInstallLocation(stdout);
	if (!installLocation) return false;
	return path.basename(installLocation.replace(/\\+$/, '')).toLowerCase() === WINDOWS_LEGACY_SQUIRREL_ID;
}

async function removeLegacySquirrelUninstallEntryAsync(): Promise<void> {
	const legacyKey = uninstallKey(WINDOWS_LEGACY_SQUIRREL_ID);
	const legacyEntry = await runReg(['query', legacyKey]);
	if (legacyEntry.status !== 0) return;
	if (!(await velopackEntryDescribesLegacyRoot())) {
		log.info('[LegacyUninstallEntry] Keeping the Squirrel uninstall entry, no Velopack entry describes this install');
		return;
	}
	const removal = await runReg(['delete', legacyKey, '/f']);
	if (removal.status !== 0) {
		log.warn('[LegacyUninstallEntry] Failed to remove the Squirrel uninstall entry', {status: removal.status});
		return;
	}
	log.info('[LegacyUninstallEntry] Removed the Squirrel uninstall entry', {key: legacyKey});
}

export function removeLegacySquirrelUninstallEntry(): void {
	if (process.platform !== 'win32') return;
	if (!isRunningFromMigratedSquirrelRoot()) return;
	removeLegacySquirrelUninstallEntryAsync().catch((error: unknown) => {
		log.warn('[LegacyUninstallEntry] Failed to clean up the Squirrel uninstall entry', error);
	});
}
