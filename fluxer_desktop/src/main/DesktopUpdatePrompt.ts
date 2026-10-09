// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_APP_NAME} from '@electron/common/DesktopIdentity';
import {createChildLogger} from '@electron/common/Logger';
import {
	checkDesktopUpdateNow,
	desktopUpdateReplacesShell,
	getDesktopUpdateState,
	startDesktopUpdate,
} from '@electron/main/DesktopUpdateGate';
import {t} from '@electron/main/MainI18n';
import {openExternalDeduped} from '@electron/main/OpenExternal';
import {findNewerManualShell} from '@electron/main/Updater';
import {DOWNLOAD_PAGE_URL} from '@electron/main/UpdaterDownloads';
import {app, BrowserWindow, dialog, type MessageBoxOptions} from 'electron';

const logger = createChildLogger('DesktopUpdatePrompt');

export interface DesktopUpdateFailure {
	readonly reason: string;
	readonly detail: string | null;
}

const UpdatePromptChoice = Object.freeze({
	PRIMARY: 0,
	SECONDARY: 1,
} as const);

let checking: Promise<void> | null = null;
let failurePrompt: Promise<void> | null = null;

async function ask(options: MessageBoxOptions): Promise<number> {
	const parent = BrowserWindow.getFocusedWindow();
	const usable = parent != null && !parent.isDestroyed() && parent.isVisible() ? parent : null;
	const result = usable == null ? await dialog.showMessageBox(options) : await dialog.showMessageBox(usable, options);
	return result.response;
}

function describeError(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

async function runUpdateCheck(): Promise<void> {
	for (;;) {
		let manualVersion: string | null = null;
		try {
			const state = await checkDesktopUpdateNow();
			if (state.updating === true) {
				logger.info('A desktop update is already running, leaving it to finish');
				return;
			}
			if (state.available) {
				await offerDesktopUpdate();
				return;
			}
			manualVersion = (await findNewerManualShell())?.version ?? null;
		} catch (error) {
			logger.warn('The update check from a native menu failed', error);
			const retry = await ask({
				type: 'warning',
				message: t('desktop.update.checkFailedMessage'),
				detail: `${t('desktop.update.checkFailedDetail')}\n\n${describeError(error)}`,
				buttons: [t('desktop.appLoad.retry'), t('desktop.update.later')],
				defaultId: UpdatePromptChoice.PRIMARY,
				cancelId: UpdatePromptChoice.SECONDARY,
			});
			if (retry === UpdatePromptChoice.PRIMARY) {
				continue;
			}
			return;
		}
		if (manualVersion != null) {
			const download = await ask({
				type: 'info',
				message: t('desktop.update.manualMessage', {version: manualVersion}),
				detail: t('desktop.update.manualDetail'),
				buttons: [t('desktop.update.download'), t('desktop.update.later')],
				defaultId: UpdatePromptChoice.PRIMARY,
				cancelId: UpdatePromptChoice.SECONDARY,
			});
			if (download === UpdatePromptChoice.PRIMARY) {
				await openExternalDeduped(DOWNLOAD_PAGE_URL);
			}
			return;
		}
		await ask({
			type: 'info',
			message: t('desktop.update.upToDateMessage'),
			detail: t('desktop.update.upToDateDetail', {version: app.getVersion()}),
			buttons: [t('desktop.update.ok')],
			defaultId: UpdatePromptChoice.PRIMARY,
		});
		return;
	}
}

async function offerDesktopUpdate(): Promise<void> {
	const install = await ask({
		type: 'info',
		message: t('desktop.update.availableMessage'),
		detail: t(desktopUpdateReplacesShell() ? 'desktop.update.availableDetail' : 'desktop.update.reloadDetail'),
		buttons: [t('desktop.update.install'), t('desktop.update.later')],
		defaultId: UpdatePromptChoice.PRIMARY,
		cancelId: UpdatePromptChoice.SECONDARY,
	});
	if (install === UpdatePromptChoice.PRIMARY) {
		startDesktopUpdateFromShell();
	}
}

export function checkForUpdatesFromShell(): Promise<void> {
	checking ??= runUpdateCheck()
		.catch((error: unknown) => {
			logger.error('The native update prompt failed', error);
		})
		.finally(() => {
			checking = null;
		});
	return checking;
}

export function startDesktopUpdateFromShell(): void {
	if (startDesktopUpdate()) {
		logger.info('Started the desktop update from a native menu');
		return;
	}
	if (getDesktopUpdateState().available) {
		return;
	}
	void checkForUpdatesFromShell();
}

export async function updateFromCommandLine(): Promise<void> {
	try {
		const state = await checkDesktopUpdateNow();
		if (!state.available) {
			logger.info('The command line asked for an update and none is waiting');
			return;
		}
		logger.info('The command line asked for an update, starting it', {started: startDesktopUpdate()});
	} catch (error) {
		logger.warn('The command line asked for an update and the check failed', error);
	}
}

export function reportDesktopUpdateFailure(failure: DesktopUpdateFailure): Promise<void> {
	failurePrompt ??= (async () => {
		logger.warn('Telling the user the desktop update did not finish', failure);
		const stillDownloading = failure.reason === 'timed-out';
		const choice = await ask({
			type: 'warning',
			title: DESKTOP_APP_NAME,
			message: t('desktop.update.failedMessage'),
			detail: stillDownloading ? t('desktop.update.stillDownloadingDetail') : t('desktop.update.failedDetail'),
			buttons: [t('desktop.appLoad.retry'), t('desktop.update.download'), t('desktop.update.later')],
			defaultId: 0,
			cancelId: 2,
		});
		if (choice === 0) {
			startDesktopUpdateFromShell();
		} else if (choice === 1) {
			await openExternalDeduped(DOWNLOAD_PAGE_URL);
		}
	})()
		.catch((error: unknown) => {
			logger.error('The update failure prompt failed', error);
		})
		.finally(() => {
			failurePrompt = null;
		});
	return failurePrompt;
}
