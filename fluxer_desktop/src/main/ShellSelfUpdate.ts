// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import {
	type AppImageTarget,
	applyStagedAppImageUpdate,
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

let velopackDownload: {
	readonly version: string;
	readonly settled: Promise<void>;
	readonly observers: Set<(percent: number) => void>;
} | null = null;

function downloadVelopackUpdateOnce(
	manager: VelopackUpdateManager,
	available: UpdateInfo,
	onProgress: (percent: number) => void,
): Promise<void> {
	const version = getStagedVelopackVersion(available);
	if (version == null) {
		return manager.downloadUpdateAsync(available, onProgress);
	}
	if (velopackDownload != null && velopackDownload.version === version) {
		velopackDownload.observers.add(onProgress);
		return velopackDownload.settled;
	}
	const observers = new Set([onProgress]);
	const settled = manager
		.downloadUpdateAsync(available, (percent) => {
			for (const observer of Array.from(observers)) observer(percent);
		})
		.finally(() => {
			if (velopackDownload?.settled === settled) velopackDownload = null;
		});
	velopackDownload = {version, settled, observers};
	return settled;
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
			await downloadVelopackUpdateOnce(manager, available, (percent) => {
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
	app.quit();
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

let appImageStaging: {
	readonly version: string;
	readonly staged: Promise<StagedAppImageUpdate>;
	readonly observers: Set<(percent: number) => void>;
} | null = null;

async function stageAppImageUpdateOnce(
	target: AppImageTarget,
	version: string,
	expectedSha256: string,
	onProgress: (percent: number) => void,
): Promise<StagedAppImageUpdate> {
	if (appImageStaging == null || appImageStaging.version !== version) {
		const observers = new Set<(percent: number) => void>();
		const staged = stageAppImageUpdate({
			target,
			url: `${getUpdateBaseUrl()}/${version}/appimage`,
			expectedSha256,
			fetchImpl: moduleNetworkFetch,
			onProgress: ({transferred, total}) => {
				if (total <= 0) return;
				const percent = Math.min(100, (transferred / total) * 100);
				for (const observer of Array.from(observers)) observer(percent);
			},
		});
		const staging = {version, staged, observers};
		appImageStaging = staging;
		staged.catch(() => {
			if (appImageStaging === staging) appImageStaging = null;
		});
	}
	const staging = appImageStaging;
	staging.observers.add(onProgress);
	try {
		return await staging.staged;
	} finally {
		staging.observers.delete(onProgress);
	}
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
		staged = await stageAppImageUpdateOnce(target, published.version, published.sha256, (percent) => {
			if (control.isSettled()) return;
			hooks.onDownloading(percent);
		});
	} catch (error) {
		control.settle({reason: 'download-failed', detail: errorDetail(error)});
		return;
	}
	if (control.isSettled()) {
		return;
	}
	hooks.onRestarting();
	appImageStaging = null;
	try {
		applyStagedAppImageUpdate(target, staged);
	} catch (error) {
		control.settle({reason: 'install-failed', detail: errorDetail(error)});
		return;
	}
	relaunchStableLaunchPath();
	app.quit();
}

const ElectronUpdatePhase = Object.freeze({
	IDLE: 'idle',
	CHECKING: 'checking',
	DOWNLOADING: 'downloading',
	DOWNLOADED: 'downloaded',
} as const);

type ElectronUpdatePhase = (typeof ElectronUpdatePhase)[keyof typeof ElectronUpdatePhase];

type ElectronUpdateEvent =
	| {readonly type: 'available'}
	| {readonly type: 'not-available'}
	| {readonly type: 'downloaded'}
	| {readonly type: 'error'; readonly error: Error; readonly phase: ElectronUpdatePhase};

const electronUpdateSession: {
	phase: ElectronUpdatePhase;
	armed: boolean;
	readonly observers: Set<(event: ElectronUpdateEvent) => void>;
} = {
	phase: ElectronUpdatePhase.IDLE,
	armed: false,
	observers: new Set(),
};

function publishElectronUpdateEvent(event: ElectronUpdateEvent): void {
	for (const observer of Array.from(electronUpdateSession.observers)) {
		try {
			observer(event);
		} catch (error) {
			logger.error('A shell self update observer threw', error);
		}
	}
}

function armElectronUpdateSession(): void {
	if (electronUpdateSession.armed) return;
	electronUpdateSession.armed = true;
	autoUpdater.on('update-available', () => {
		electronUpdateSession.phase = ElectronUpdatePhase.DOWNLOADING;
		publishElectronUpdateEvent({type: 'available'});
	});
	autoUpdater.on('update-not-available', () => {
		electronUpdateSession.phase = ElectronUpdatePhase.IDLE;
		publishElectronUpdateEvent({type: 'not-available'});
	});
	autoUpdater.on('update-downloaded', () => {
		electronUpdateSession.phase = ElectronUpdatePhase.DOWNLOADED;
		logger.info('The shell update finished downloading');
		publishElectronUpdateEvent({type: 'downloaded'});
	});
	autoUpdater.on('error', (error: Error) => {
		const phase = electronUpdateSession.phase;
		if (phase !== ElectronUpdatePhase.DOWNLOADED) {
			electronUpdateSession.phase = ElectronUpdatePhase.IDLE;
		}
		publishElectronUpdateEvent({type: 'error', error, phase});
	});
}

export function resetElectronUpdateSessionForTests(): void {
	electronUpdateSession.phase = ElectronUpdatePhase.IDLE;
	electronUpdateSession.armed = false;
	electronUpdateSession.observers.clear();
}

function runElectronSelfUpdate(control: SelfUpdateControl, hooks: ShellSelfUpdateHooks): void {
	armElectronUpdateSession();
	const install = (): void => {
		control.clearCheckDeadline();
		hooks.onRestarting();
		try {
			autoUpdater.quitAndInstall();
		} catch (error) {
			control.settle({reason: 'install-failed', detail: errorDetail(error)});
		}
	};
	const observer = (event: ElectronUpdateEvent): void => {
		switch (event.type) {
			case 'available':
				control.clearCheckDeadline();
				hooks.onDownloading(null);
				return;
			case 'not-available':
				control.settle({reason: 'no-update', detail: null});
				return;
			case 'downloaded':
				install();
				return;
			case 'error':
				control.settle({
					reason: event.phase === ElectronUpdatePhase.DOWNLOADING ? 'download-failed' : 'check-failed',
					detail: errorDetail(event.error),
				});
				return;
		}
	};
	electronUpdateSession.observers.add(observer);
	control.addCleanup(() => {
		electronUpdateSession.observers.delete(observer);
	});
	switch (electronUpdateSession.phase) {
		case ElectronUpdatePhase.DOWNLOADED:
			logger.info('The shell update was already downloaded this session, installing it');
			install();
			return;
		case ElectronUpdatePhase.DOWNLOADING:
			logger.info('The shell update is already downloading this session, waiting for it');
			control.clearCheckDeadline();
			hooks.onDownloading(null);
			return;
		case ElectronUpdatePhase.CHECKING:
			logger.info('The shell update check is already running this session, waiting for it');
			return;
		case ElectronUpdatePhase.IDLE:
			break;
	}
	try {
		electronUpdateSession.phase = ElectronUpdatePhase.CHECKING;
		autoUpdater.setFeedURL({
			url: getElectronUpdateFeedUrl(),
			serverType: 'json',
			headers: {'User-Agent': feedUserAgent()},
		});
		autoUpdater.checkForUpdates();
	} catch (error) {
		electronUpdateSession.phase = ElectronUpdatePhase.IDLE;
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
