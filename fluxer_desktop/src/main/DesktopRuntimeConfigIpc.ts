// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {desktopRuntimeInitialInput} from '@electron/main/DesktopRuntimeDiscovery';
import {DesktopRuntimeResolutionRegistry} from '@electron/main/DesktopRuntimeResolutionRegistry';
import {DesktopRuntimeTransactionRegistry} from '@electron/main/DesktopRuntimeTransactionRegistry';
import {
	addLocalAppUploadProgressSubscriber,
	clearLocalAppUploadProgressSubscribers,
} from '@electron/main/LocalAppProxyClient';
import {createPrivilegedRendererDocumentOwners} from '@electron/main/PrivilegedRendererDocuments';
import {LOCAL_APP_UPLOAD_PROGRESS_CHANNELS} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';
import {DESKTOP_RUNTIME_CONFIG_CHANNELS} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import {ipcMain} from 'electron';

const log = createChildLogger('DesktopRuntimeConfig');

const rendererDocuments = createPrivilegedRendererDocumentOwners('DesktopRuntimeConfig');

const resolutions = new DesktopRuntimeResolutionRegistry(rendererDocuments);
const transactions = new DesktopRuntimeTransactionRegistry(rendererDocuments);
let registered = false;

export function registerDesktopRuntimeConfigHandlers(): void {
	if (registered) {
		return;
	}
	transactions.initialize();
	registered = true;
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.initialInput, (event) => {
		rendererDocuments.requireExecutingCurrentMainFrameForRead(event, 'Desktop runtime config initial input');
		return desktopRuntimeInitialInput();
	});
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.resolve, (event, request: unknown) =>
		resolutions.resolve(event, request),
	);
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.cancelResolution, (event, request: unknown) =>
		resolutions.cancel(event, request),
	);
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.prepare, (event, request: unknown) =>
		transactions.prepare(event, request),
	);
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.commit, (event, request: unknown) =>
		transactions.commit(event, request),
	);
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.abort, (event, request: unknown) =>
		transactions.abort(event, request),
	);
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.finalize, (event, request: unknown) =>
		transactions.finalize(event, request),
	);
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.rollback, (event, request: unknown) =>
		transactions.rollback(event, request),
	);
	ipcMain.handle(DESKTOP_RUNTIME_CONFIG_CHANNELS.deactivate, (event, request: unknown) =>
		transactions.deactivate(event, request),
	);
	ipcMain.on(LOCAL_APP_UPLOAD_PROGRESS_CHANNELS.subscribe, (event) => {
		try {
			rendererDocuments.capture(event, 'Local app upload progress subscription');
		} catch (error) {
			log.warn('Rejected a local app upload progress subscription', {error});
			return;
		}
		addLocalAppUploadProgressSubscriber(event.sender);
	});
}

export function cleanupDesktopRuntimeConfigHandlers(): void {
	if (!registered) {
		return;
	}
	registered = false;
	for (const channel of Object.values(DESKTOP_RUNTIME_CONFIG_CHANNELS)) {
		ipcMain.removeHandler(channel);
	}
	ipcMain.removeAllListeners(LOCAL_APP_UPLOAD_PROGRESS_CHANNELS.subscribe);
	resolutions.cleanup();
	transactions.cleanup();
	clearLocalAppUploadProgressSubscribers();
}
