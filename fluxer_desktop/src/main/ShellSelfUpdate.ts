// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import {
	type AppImageTarget,
	applyStagedAppImageUpdate,
	discardStagedAppImageUpdate,
	type StagedAppImageUpdate,
	stageAppImageUpdate,
} from '@electron/main/AppImageUpdate';
import {relaunchStableLaunchPath} from '@electron/main/LinuxLaunchPath';
import {moduleNetworkFetch} from '@electron/main/ModuleNetworkFetch';
import {compareModuleVersions, parseModuleVersion} from '@electron/main/ModuleVersion';
import {getUpdateBaseUrl} from '@electron/main/ShellDownloadFormats';
import type {ShellUpdateCapability, ShellUpdatePlan} from '@electron/main/ShellUpdateCapability';
import {
	clearVelopackApplyAttempt,
	readVelopackApplyAttempt,
	recordVelopackApplyAttempt,
} from '@electron/main/UpdaterApplyState';
import {app, autoUpdater, net} from 'electron';
import type {UpdateInfo, VelopackAsset} from 'velopack';

const requireModule = createRequire(import.meta.url);
const logger = createChildLogger('ShellSelfUpdate');

export const SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS = 30_000;
export const SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS = 600_000;

export function getElectronUpdateFeedUrl(): string {
	return `${getUpdateBaseUrl()}/RELEASES.json`;
}

type ShellSelfUpdateReason = 'no-update' | 'check-failed' | 'download-failed' | 'install-failed' | 'timed-out';

interface ShellSelfUpdateFailure {
	readonly reason: ShellSelfUpdateReason;
	readonly detail: string | null;
}

interface ShellSelfUpdateHooks {
	readonly onDownloading: (progress: number | null) => void;
	readonly onRestarting: () => void;
}

interface SelfUpdateControl {
	readonly settle: (failure: ShellSelfUpdateFailure) => void;
	readonly isSettled: () => boolean;
	readonly clearCheckDeadline: () => void;
	readonly addCleanup: (cleanup: () => void) => void;
}

function errorDetail(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function compareShellVersions(left: string, right: string): number {
	return compareModuleVersions(parseModuleVersion(left, 'shell version'), parseModuleVersion(right, 'shell version'));
}

function feedUserAgent(): string {
	return `Fluxer/${app.getVersion()} (${process.platform}: ${process.arch})`;
}

type VelopackUpdateManager = InstanceType<typeof import('velopack').UpdateManager>;

function readInstalledVelopackVersion(manager: VelopackUpdateManager): string {
	try {
		const version = manager.getCurrentVersion();
		return typeof version === 'string' && version.length > 0 ? version : app.getVersion();
	} catch {
		return app.getVersion();
	}
}

function hasUnlandedVelopackApply(manager: VelopackUpdateManager): boolean {
	const attempt = readVelopackApplyAttempt();
	if (attempt == null) return false;
	if (compareShellVersions(readInstalledVelopackVersion(manager), attempt.version) >= 0) {
		clearVelopackApplyAttempt();
		return false;
	}
	return true;
}

function getStagedVelopackVersion(staged: UpdateInfo | VelopackAsset): string | null {
	const asset = 'TargetFullRelease' in staged ? staged.TargetFullRelease : staged;
	return asset.Version ?? null;
}

async function runVelopackSelfUpdate(control: SelfUpdateControl, hooks: ShellSelfUpdateHooks): Promise<void> {
	const {UpdateManager} = requireModule('velopack') as typeof import('velopack');
	const manager = new UpdateManager(getUpdateBaseUrl());
	if (hasUnlandedVelopackApply(manager)) {
		control.settle({reason: 'install-failed', detail: 'the last downloaded update was never installed'});
		return;
	}
	let staged: UpdateInfo | VelopackAsset | null;
	try {
		staged = manager.getUpdatePendingRestart();
	} catch (error) {
		control.settle({reason: 'check-failed', detail: errorDetail(error)});
		return;
	}
	if (control.isSettled()) return;
	if (staged == null) {
		let available: UpdateInfo | null;
		try {
			available = await manager.checkForUpdatesAsync();
		} catch (error) {
			control.settle({reason: 'check-failed', detail: errorDetail(error)});
			return;
		}
		if (control.isSettled()) return;
		if (available == null) {
			control.settle({reason: 'no-update', detail: null});
			return;
		}
		control.clearCheckDeadline();
		hooks.onDownloading(0);
		try {
			await manager.downloadUpdateAsync(available, (percent) => {
				if (control.isSettled()) return;
				hooks.onDownloading(percent);
			});
		} catch (error) {
			control.settle({reason: 'download-failed', detail: errorDetail(error)});
			return;
		}
		if (control.isSettled()) return;
		staged = available;
	}
	control.clearCheckDeadline();
	hooks.onRestarting();
	const stagedVersion = getStagedVelopackVersion(staged);
	if (stagedVersion != null) {
		recordVelopackApplyAttempt(stagedVersion);
	}
	try {
		manager.waitExitThenApplyUpdate(staged, true, true);
	} catch (error) {
		control.settle({reason: 'install-failed', detail: errorDetail(error)});
		return;
	}
	setImmediate(() => {
		app.exit(0);
	});
}

async function fetchPublishedAppImage(): Promise<{version: string; sha256: string | null}> {
	const response = await net.fetch(`${getUpdateBaseUrl()}/latest`, {
		cache: 'no-store',
		headers: {Accept: 'application/json', 'Cache-Control': 'no-cache'},
	});
	if (!response.ok) {
		throw new Error(`Latest version request failed: ${response.status}`);
	}
	const payload = (await response.json()) as {version?: unknown; files?: {appimage?: {sha256?: unknown}}};
	if (typeof payload.version !== 'string' || payload.version.length === 0) {
		throw new Error('Latest version response missing version string');
	}
	const sha256 = payload.files?.appimage?.sha256;
	return {version: payload.version, sha256: typeof sha256 === 'string' ? sha256 : null};
}

async function runAppImageSelfUpdate(
	control: SelfUpdateControl,
	hooks: ShellSelfUpdateHooks,
	target: AppImageTarget,
): Promise<void> {
	let published: {version: string; sha256: string | null};
	try {
		published = await fetchPublishedAppImage();
	} catch (error) {
		control.settle({reason: 'check-failed', detail: errorDetail(error)});
		return;
	}
	if (control.isSettled()) return;
	let newer: boolean;
	try {
		newer = compareShellVersions(published.version, app.getVersion()) > 0;
	} catch (error) {
		control.settle({reason: 'check-failed', detail: errorDetail(error)});
		return;
	}
	if (!newer) {
		control.settle({reason: 'no-update', detail: null});
		return;
	}
	if (published.sha256 == null) {
		control.settle({reason: 'check-failed', detail: 'the published AppImage has no checksum'});
		return;
	}
	control.clearCheckDeadline();
	hooks.onDownloading(0);
	let staged: StagedAppImageUpdate;
	try {
		staged = await stageAppImageUpdate({
			target,
			url: `${getUpdateBaseUrl()}/${published.version}/appimage`,
			expectedSha256: published.sha256,
			fetchImpl: moduleNetworkFetch,
			onProgress: ({transferred, total}) => {
				if (control.isSettled() || total <= 0) return;
				hooks.onDownloading(Math.min(100, (transferred / total) * 100));
			},
		});
	} catch (error) {
		control.settle({reason: 'download-failed', detail: errorDetail(error)});
		return;
	}
	if (control.isSettled()) {
		discardStagedAppImageUpdate(staged);
		return;
	}
	hooks.onRestarting();
	try {
		applyStagedAppImageUpdate(target, staged);
	} catch (error) {
		control.settle({reason: 'install-failed', detail: errorDetail(error)});
		return;
	}
	relaunchStableLaunchPath();
	setImmediate(() => {
		app.exit(0);
	});
}

function runElectronSelfUpdate(control: SelfUpdateControl, hooks: ShellSelfUpdateHooks): void {
	let downloading = false;
	const onUpdateAvailable = (): void => {
		downloading = true;
		control.clearCheckDeadline();
		hooks.onDownloading(null);
	};
	const onUpdateNotAvailable = (): void => {
		control.settle({reason: 'no-update', detail: null});
	};
	const onUpdateDownloaded = (): void => {
		control.clearCheckDeadline();
		hooks.onRestarting();
		try {
			autoUpdater.quitAndInstall();
		} catch (error) {
			control.settle({reason: 'install-failed', detail: errorDetail(error)});
		}
	};
	const onError = (error: Error): void => {
		control.settle({reason: downloading ? 'download-failed' : 'check-failed', detail: errorDetail(error)});
	};
	autoUpdater.on('update-available', onUpdateAvailable);
	autoUpdater.on('update-not-available', onUpdateNotAvailable);
	autoUpdater.on('update-downloaded', onUpdateDownloaded);
	autoUpdater.on('error', onError);
	control.addCleanup(() => {
		autoUpdater.removeListener('update-available', onUpdateAvailable);
		autoUpdater.removeListener('update-not-available', onUpdateNotAvailable);
		autoUpdater.removeListener('update-downloaded', onUpdateDownloaded);
		autoUpdater.removeListener('error', onError);
	});
	try {
		autoUpdater.setFeedURL({
			url: getElectronUpdateFeedUrl(),
			serverType: 'json',
			headers: {'User-Agent': feedUserAgent()},
		});
		autoUpdater.checkForUpdates();
	} catch (error) {
		control.settle({reason: 'check-failed', detail: errorDetail(error)});
	}
}

type ShellSelfUpdatePlan = Extract<ShellUpdatePlan, {capability: typeof ShellUpdateCapability.SELF_UPDATE}>;

export function runShellSelfUpdate(
	plan: ShellSelfUpdatePlan,
	hooks: ShellSelfUpdateHooks,
): Promise<ShellSelfUpdateFailure> {
	return new Promise<ShellSelfUpdateFailure>((resolve) => {
		const cleanups: Array<() => void> = [];
		let settled = false;
		let checkTimer: NodeJS.Timeout | null = null;
		let totalTimer: NodeJS.Timeout | null = null;

		const clearCheckDeadline = (): void => {
			if (checkTimer == null) return;
			clearTimeout(checkTimer);
			checkTimer = null;
		};

		const settle = (failure: ShellSelfUpdateFailure): void => {
			if (settled) return;
			settled = true;
			clearCheckDeadline();
			if (totalTimer != null) {
				clearTimeout(totalTimer);
				totalTimer = null;
			}
			for (const cleanup of cleanups.splice(0)) {
				try {
					cleanup();
				} catch (error) {
					logger.error('A shell self update cleanup threw', error);
				}
			}
			logger.warn('The shell self update did not complete', {updater: plan.updater, ...failure});
			resolve(failure);
		};

		const control: SelfUpdateControl = {
			settle,
			isSettled: () => settled,
			clearCheckDeadline,
			addCleanup: (cleanup) => {
				cleanups.push(cleanup);
			},
		};

		checkTimer = setTimeout(() => {
			checkTimer = null;
			settle({reason: 'timed-out', detail: 'the update check produced no answer'});
		}, SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS);
		checkTimer.unref();
		totalTimer = setTimeout(() => {
			totalTimer = null;
			settle({reason: 'timed-out', detail: 'the update did not finish'});
		}, SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS);
		totalTimer.unref();

		if (plan.updater === 'appimage') {
			runAppImageSelfUpdate(control, hooks, plan.target).catch((error: unknown) => {
				settle({reason: 'install-failed', detail: errorDetail(error)});
			});
			return;
		}
		if (plan.updater === 'velopack') {
			runVelopackSelfUpdate(control, hooks).catch((error: unknown) => {
				settle({reason: 'install-failed', detail: errorDetail(error)});
			});
			return;
		}
		try {
			runElectronSelfUpdate(control, hooks);
		} catch (error) {
			settle({reason: 'check-failed', detail: errorDetail(error)});
		}
	});
}
