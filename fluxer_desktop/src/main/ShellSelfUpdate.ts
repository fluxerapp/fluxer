// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {errorMessage as errorDetail} from '@electron/common/ErrorMessage';
import {createChildLogger} from '@electron/common/Logger';
import {
	type AppImageTarget,
	applyStagedAppImageUpdate,
	type StagedAppImageUpdate,
	stageAppImageUpdate,
} from '@electron/main/AppImageUpdate';
import {relaunchStableLaunchPath} from '@electron/main/LinuxLaunchPath';
import {moduleNetworkFetch} from '@electron/main/ModuleNetworkFetch';
import {compareShellVersions} from '@electron/main/ModuleVersion';
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

const SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS = 30_000;
const SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS = 600_000;

function getElectronUpdateFeedUrl(): string {
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

type ShellStageResult =
	| {readonly status: 'staged'}
	| {readonly status: 'failed'; readonly failure: ShellSelfUpdateFailure};

interface StageControl {
	readonly succeed: () => void;
	readonly settle: (failure: ShellSelfUpdateFailure) => void;
	readonly isSettled: () => boolean;
	readonly clearCheckDeadline: () => void;
	readonly addCleanup: (cleanup: () => void) => void;
}

type VelopackUpdateManager = InstanceType<typeof import('velopack').UpdateManager>;

type StagedShellUpdate =
	| {
			readonly updater: 'velopack';
			readonly manager: VelopackUpdateManager;
			readonly update: UpdateInfo | VelopackAsset;
	  }
	| {readonly updater: 'appimage'; readonly target: AppImageTarget; readonly update: StagedAppImageUpdate}
	| {readonly updater: 'electron'};

let stagedShell: StagedShellUpdate | null = null;
let applyingShell = false;

function feedUserAgent(): string {
	return `Fluxer/${app.getVersion()} (${process.platform}: ${process.arch})`;
}

function readInstalledVelopackVersion(manager: VelopackUpdateManager): string {
	try {
		const version = manager.getCurrentVersion();
		return typeof version === 'string' && version.length > 0 ? version : app.getVersion();
	} catch {
		return app.getVersion();
	}
}

const UNLANDED_APPLY_RETRY_AFTER_MS = 30 * 60_000;

type UnlandedApply = 'none' | 'recent' | 'stale';

function classifyUnlandedVelopackApply(manager: VelopackUpdateManager): UnlandedApply {
	const attempt = readVelopackApplyAttempt();
	if (attempt == null) return 'none';
	if (compareShellVersions(readInstalledVelopackVersion(manager), attempt.version) >= 0) {
		clearVelopackApplyAttempt();
		return 'none';
	}
	if (Date.now() - attempt.attemptedAt < UNLANDED_APPLY_RETRY_AFTER_MS) {
		return 'recent';
	}
	logger.warn('An earlier update never installed, retrying from the feed', {version: attempt.version});
	clearVelopackApplyAttempt();
	return 'stale';
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

function launchVelopackApply(
	manager: VelopackUpdateManager,
	update: UpdateInfo | VelopackAsset,
	restart: boolean,
): void {
	const version = getStagedVelopackVersion(update);
	if (version != null) {
		recordVelopackApplyAttempt(version);
	}
	manager.waitExitThenApplyUpdate(update, true, restart);
}

function armVelopackApplyOnQuit(): void {
	app.once('will-quit', () => {
		const staged = stagedShell;
		if (applyingShell || staged?.updater !== 'velopack') return;
		applyingShell = true;
		try {
			launchVelopackApply(staged.manager, staged.update, false);
			logger.info('Applying the staged shell update while the app quits');
		} catch (error) {
			logger.warn('Failed to apply the staged shell update on quit', error);
		}
	});
}

async function stageVelopackUpdate(
	control: StageControl,
	onDownloading: (progress: number | null) => void,
): Promise<void> {
	const {UpdateManager} = requireModule('velopack') as typeof import('velopack');
	const manager = new UpdateManager(getUpdateBaseUrl());
	const unlanded = classifyUnlandedVelopackApply(manager);
	if (unlanded === 'recent') {
		control.settle({reason: 'install-failed', detail: 'the last downloaded update was never installed'});
		return;
	}
	let staged: UpdateInfo | VelopackAsset | null = null;
	if (unlanded === 'none') {
		try {
			staged = manager.getUpdatePendingRestart();
		} catch (error) {
			control.settle({reason: 'check-failed', detail: errorDetail(error)});
			return;
		}
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
		onDownloading(0);
		try {
			await downloadVelopackUpdateOnce(manager, available, (percent) => {
				if (control.isSettled()) return;
				onDownloading(percent);
			});
		} catch (error) {
			control.settle({reason: 'download-failed', detail: errorDetail(error)});
			return;
		}
		if (control.isSettled()) return;
		staged = available;
	}
	if (stagedShell == null) {
		armVelopackApplyOnQuit();
	}
	stagedShell = {updater: 'velopack', manager, update: staged};
	control.succeed();
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

async function stageAppImageShellUpdate(
	control: StageControl,
	onDownloading: (progress: number | null) => void,
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
	onDownloading(0);
	let staged: StagedAppImageUpdate;
	try {
		staged = await stageAppImageUpdateOnce(target, published.version, published.sha256, (percent) => {
			if (control.isSettled()) return;
			onDownloading(percent);
		});
	} catch (error) {
		control.settle({reason: 'download-failed', detail: errorDetail(error)});
		return;
	}
	if (control.isSettled()) return;
	stagedShell = {updater: 'appimage', target, update: staged};
	control.succeed();
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
		stagedShell = {updater: 'electron'};
		logger.info('The shell update finished downloading and installs when the app quits');
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

function stageElectronUpdate(control: StageControl, onDownloading: (progress: number | null) => void): void {
	armElectronUpdateSession();
	const observer = (event: ElectronUpdateEvent): void => {
		switch (event.type) {
			case 'available':
				control.clearCheckDeadline();
				onDownloading(null);
				return;
			case 'not-available':
				control.settle({reason: 'no-update', detail: null});
				return;
			case 'downloaded':
				control.succeed();
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
			control.succeed();
			return;
		case ElectronUpdatePhase.DOWNLOADING:
			control.clearCheckDeadline();
			onDownloading(null);
			return;
		case ElectronUpdatePhase.CHECKING:
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

export function stageShellUpdate(
	plan: ShellSelfUpdatePlan,
	onDownloading: (progress: number | null) => void = () => {},
): Promise<ShellStageResult> {
	if (stagedShell != null) {
		return Promise.resolve({status: 'staged'});
	}
	return new Promise<ShellStageResult>((resolve) => {
		const cleanups: Array<() => void> = [];
		let settled = false;
		let checkTimer: NodeJS.Timeout | null = null;
		let totalTimer: NodeJS.Timeout | null = null;

		const clearCheckDeadline = (): void => {
			if (checkTimer == null) return;
			clearTimeout(checkTimer);
			checkTimer = null;
		};

		const finish = (result: ShellStageResult): void => {
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
					logger.error('A shell update staging cleanup threw', error);
				}
			}
			if (result.status === 'failed') {
				logger.warn('The shell update was not staged', {updater: plan.updater, ...result.failure});
			} else {
				logger.info('The shell update is staged', {updater: plan.updater});
			}
			resolve(result);
		};

		const control: StageControl = {
			succeed: () => finish({status: 'staged'}),
			settle: (failure) => finish({status: 'failed', failure}),
			isSettled: () => settled,
			clearCheckDeadline,
			addCleanup: (cleanup) => {
				cleanups.push(cleanup);
			},
		};

		checkTimer = setTimeout(() => {
			checkTimer = null;
			control.settle({reason: 'timed-out', detail: 'the update check produced no answer'});
		}, SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS);
		checkTimer.unref();
		totalTimer = setTimeout(() => {
			totalTimer = null;
			control.settle({reason: 'timed-out', detail: 'the update did not finish downloading'});
		}, SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS);
		totalTimer.unref();

		const fail = (error: unknown): void => {
			control.settle({reason: 'check-failed', detail: errorDetail(error)});
		};
		switch (plan.updater) {
			case 'appimage':
				stageAppImageShellUpdate(control, onDownloading, plan.target).catch(fail);
				return;
			case 'velopack':
				stageVelopackUpdate(control, onDownloading).catch(fail);
				return;
			case 'electron':
				try {
					stageElectronUpdate(control, onDownloading);
				} catch (error) {
					fail(error);
				}
				return;
		}
	});
}

export function applyStagedShellUpdate(): ShellSelfUpdateFailure | null {
	const staged = stagedShell;
	if (staged == null) {
		return {reason: 'no-update', detail: 'no shell update is staged'};
	}
	applyingShell = true;
	try {
		switch (staged.updater) {
			case 'velopack':
				launchVelopackApply(staged.manager, staged.update, true);
				app.quit();
				break;
			case 'appimage':
				appImageStaging = null;
				applyStagedAppImageUpdate(staged.target, staged.update);
				stagedShell = null;
				relaunchStableLaunchPath();
				app.quit();
				break;
			case 'electron':
				autoUpdater.quitAndInstall();
				break;
		}
	} catch (error) {
		applyingShell = false;
		return {reason: 'install-failed', detail: errorDetail(error)};
	}
	return null;
}

export async function runShellSelfUpdate(
	plan: ShellSelfUpdatePlan,
	hooks: ShellSelfUpdateHooks,
): Promise<ShellSelfUpdateFailure> {
	const staged = await stageShellUpdate(plan, hooks.onDownloading);
	if (staged.status === 'failed') {
		return staged.failure;
	}
	hooks.onRestarting();
	const failure = applyStagedShellUpdate();
	if (failure == null) {
		return await new Promise<never>(() => {});
	}
	logger.warn('The shell self update did not complete', {updater: plan.updater, ...failure});
	return failure;
}
