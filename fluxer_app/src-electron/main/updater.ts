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

import {autoUpdater, type BrowserWindow, ipcMain} from 'electron';
import log from 'electron-log';
import {UpdateSourceType, updateElectronApp} from 'update-electron-app';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import {setQuitting} from './window.js';

export type UpdaterContext = 'user' | 'background' | 'focus';

export type UpdaterEvent =
	| {type: 'checking'; context: UpdaterContext}
	| {type: 'available'; context: UpdaterContext; version?: string | null}
	| {type: 'not-available'; context: UpdaterContext}
	| {type: 'downloaded'; context: UpdaterContext; version?: string | null}
	| {type: 'error'; context: UpdaterContext; message: string};

let lastContext: UpdaterContext = 'background';

function send(win: BrowserWindow | null, event: UpdaterEvent) {
	win?.webContents.send('updater-event', event);
}

export function registerUpdater(getMainWindow: () => BrowserWindow | null) {
	updateElectronApp({
		updateSource: {
			type: UpdateSourceType.StaticStorage,
			baseUrl: `https://api.fluxer.app/dl/desktop/${BUILD_CHANNEL}/${process.platform}/${process.arch}`,
		},
		updateInterval: '12 hours',
		logger: log,
		notifyUser: false,
	});

	autoUpdater.on('checking-for-update', () => {
		send(getMainWindow(), {type: 'checking', context: lastContext});
	});

	autoUpdater.on('update-available', () => {
		send(getMainWindow(), {type: 'available', context: lastContext, version: null});
	});

	autoUpdater.on('update-not-available', () => {
		send(getMainWindow(), {type: 'not-available', context: lastContext});
	});

	autoUpdater.on('update-downloaded', (_event, _releaseNotes, releaseName) => {
		send(getMainWindow(), {type: 'downloaded', context: lastContext, version: releaseName ?? null});
	});

	autoUpdater.on('error', (err: Error) => {
		send(getMainWindow(), {type: 'error', context: lastContext, message: err?.message ?? String(err)});
	});

	ipcMain.handle('updater-check', async (_e, context: UpdaterContext) => {
		lastContext = context;
		autoUpdater.checkForUpdates();
	});

	ipcMain.handle('updater-install', async () => {
		setQuitting(true);
		autoUpdater.quitAndInstall();
	});
}
