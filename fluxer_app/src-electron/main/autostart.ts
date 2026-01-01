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

import fs from 'node:fs';
import path from 'node:path';
import {app, ipcMain} from 'electron';
import {BUILD_CHANNEL} from '../common/build-channel.js';

const AUTOSTART_INITIALIZED_FILE = 'autostart-initialized';

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
	} catch {}
}

const isMac = process.platform === 'darwin';

interface AutoLaunchConfig {
	name: string;
	path: string;
	isHidden: boolean;
	args?: Array<string>;
}

function getAutoLaunchConfig(): AutoLaunchConfig {
	const isCanary = BUILD_CHANNEL === 'canary';
	const appName = isCanary ? 'Fluxer Canary' : 'Fluxer';

	return {
		name: appName,
		path: process.execPath,
		isHidden: true,
		args: [],
	};
}

async function enableAutostart(): Promise<void> {
	if (!isMac) return;

	const config = getAutoLaunchConfig();
	app.setLoginItemSettings({
		openAtLogin: true,
		openAsHidden: config.isHidden,
		name: config.name,
	});
}

async function disableAutostart(): Promise<void> {
	if (!isMac) return;

	const config = getAutoLaunchConfig();
	app.setLoginItemSettings({
		openAtLogin: false,
		name: config.name,
		path: config.path,
		args: config.args,
	});
}

async function isAutostartEnabled(): Promise<boolean> {
	if (!isMac) return false;

	const config = getAutoLaunchConfig();
	const settings = app.getLoginItemSettings({
		path: config.path,
		args: config.args,
	});
	return settings.openAtLogin;
}

export function registerAutostartHandlers(): void {
	if (!isMac) return;

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
