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
import {app, globalShortcut} from 'electron';
import log from 'electron-log';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import {configureUserDataPath} from '../common/user-data-path.js';
import {startApiProxyServer, stopApiProxyServer} from './api-proxy-server.js';
import {registerAutostartHandlers} from './autostart.js';
import {handleOpenUrl, handleSecondInstance, initializeDeepLinks} from './deep-links.js';
import {cleanupGlobalKeyHook, registerGlobalKeyHookHandlers} from './global-key-hook.js';
import {cleanupIpcHandlers, registerIpcHandlers} from './ipc-handlers.js';
import {startMediaProxyServer, stopMediaProxyServer} from './media-proxy-server.js';
import {createApplicationMenu} from './menu.js';
import {startRpcServer, stopRpcServer} from './rpc-server.js';
import {registerUpdater} from './updater.js';
import {createWindow, getMainWindow, registerDisplayMediaHandlers, setQuitting, showWindow} from './window.js';
import {startWsProxyServer, stopWsProxyServer} from './ws-proxy-server.js';

log.transports.file.level = 'info';
log.transports.console.level = 'debug';

const requireModule = createRequire(import.meta.url);

const userDataConfig = configureUserDataPath();
log.info('Configured user data storage', {
	channel: userDataConfig.channel,
	directory: userDataConfig.directoryName,
	path: userDataConfig.base,
});

const isCanary = BUILD_CHANNEL === 'canary';

if (process.platform === 'win32') {
	const handledSquirrelEvent = requireModule('electron-squirrel-startup');
	if (handledSquirrelEvent) {
		app.quit();
	}
}

app.commandLine.appendSwitch('autoplay-policy', 'no-user-gesture-required');
if (process.platform === 'win32') {
	app.commandLine.appendSwitch('disable-background-timer-throttling');
	app.commandLine.appendSwitch('disable-renderer-backgrounding');
}

if (process.env.NODE_ENV === 'development') {
	log.error('Electron desktop does not support development mode; exiting.');
	app.quit();
	process.exit(1);
}

if (process.platform === 'win32') {
	const appId = isCanary ? 'app.fluxer.canary' : 'app.fluxer';
	app.setAppUserModelId(appId);
}

if (process.platform === 'linux') {
	const linuxName = isCanary ? 'Fluxer Canary' : 'Fluxer';
	app.setName(linuxName);
}

const gotTheLock = app.requestSingleInstanceLock();

if (!gotTheLock) {
	app.quit();
} else {
	app.on('second-instance', (_event, argv, _workingDirectory) => {
		handleSecondInstance(argv);
	});

	app.on('open-url', (event, url) => {
		event.preventDefault();
		handleOpenUrl(url);
	});

	app.whenReady().then(async () => {
		log.info('App ready, initializing...');

		try {
			initializeDeepLinks();
		} catch (error) {
			log.error('[Init] Failed to initialize deep links:', error);
		}

		try {
			registerIpcHandlers();
		} catch (error) {
			log.error('[Init] Failed to register IPC handlers:', error);
		}

		try {
			registerAutostartHandlers();
		} catch (error) {
			log.error('[Init] Failed to register autostart handlers:', error);
		}

		try {
			registerGlobalKeyHookHandlers();
		} catch (error) {
			log.error('[Init] Failed to register global key hook handlers:', error);
		}

		try {
			registerDisplayMediaHandlers();
		} catch (error: unknown) {
			log.error('[Init] Failed to register display media handlers:', error);
		}

		try {
			createApplicationMenu();
		} catch (error: unknown) {
			log.error('[Init] Failed to create application menu:', error);
		}

		createWindow();
		registerUpdater(getMainWindow);

		app.on('activate', () => {
			const mainWindow = getMainWindow();
			if (mainWindow === null || mainWindow.isDestroyed()) {
				createWindow();
			} else {
				showWindow();
			}
		});

		void startRpcServer().catch((error: unknown) => {
			log.error('[RPC] Failed to start RPC server:', error);
		});
		void startWsProxyServer().catch((error: unknown) => {
			log.error('[WS Proxy] Failed to start WS proxy server:', error);
		});
		void startApiProxyServer().catch((error: unknown) => {
			log.error('[API Proxy] Failed to start API proxy server:', error);
		});
		void startMediaProxyServer().catch((error: unknown) => {
			log.error('[Media Proxy] Failed to start media proxy server:', error);
		});

		log.info('App initialized successfully');
	});

	app.on('window-all-closed', () => {
		if (process.platform !== 'darwin') {
			app.quit();
		}
	});

	app.on('before-quit', () => {
		setQuitting(true);
	});

	app.on('will-quit', () => {
		cleanupIpcHandlers();
		cleanupGlobalKeyHook();
		globalShortcut.unregisterAll();
		void stopRpcServer();
		void stopWsProxyServer();
		void stopApiProxyServer();
		void stopMediaProxyServer();
	});

	process.on('uncaughtException', (error: unknown) => {
		log.error('Uncaught exception:', error);
	});

	process.on('unhandledRejection', (reason: unknown, promise: Promise<unknown>) => {
		log.error('Unhandled rejection at:', promise, 'reason:', reason);
	});
}
