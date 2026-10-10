// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {DESKTOP_DOWNLOAD_URL} from '@app/features/app/config/I18nDisplayConstants';
import {WORKER_NAVIGATION_CACHE_PREFIX} from '@app/features/platform/service_worker/WorkerCacheCleanup';
import {getProtectedCacheStorage} from '@app/features/platform/state/ProtectedWebStorage';
import type {UpdaterContext, UpdaterDownloadOption, UpdaterEvent} from '@app/features/platform/types/Electron';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getClientInfo} from '@app/features/platform/utils/ClientInfo';
import {flushPendingPersistWrites} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {
	downloadWithNative,
	getElectronAPI,
	isDesktop,
	isElectron,
	openExternalUrl,
} from '@app/features/ui/utils/NativeUtils';
import type * as UpdaterModalCommands from '@app/features/updater/commands/UpdaterModalCommands';
import {
	createUpdaterMachineSnapshot,
	getUpdaterDisplayVersion,
	hasManualNativeDownload,
	transitionUpdaterMachineSnapshot,
	type UpdateInfo,
	type UpdaterMachineEvent,
	type UpdaterMachineSnapshot,
} from '@app/features/updater/state/UpdaterStateMachine';
import {buildLinuxManualUpdateOptions} from '@app/features/updater/utils/LinuxManualUpdateOptions';
import type {UpdaterEvent as NativeUpdaterEvent} from '@app/types/electron.d';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('Updater');
const CHECK_INTERVAL_MS = 5 * 60 * 1000;
const MIN_CHECK_INTERVAL_MS = 60 * 1000;
const MANUAL_DOWNLOAD_REFRESH_TIMEOUT_MS = 5 * 1000;
const WEB_CHECK_TIMEOUT_MS = 15 * 1000;
const NATIVE_CHECK_TIMEOUT_MS = 30 * 1000;
const VERSION_ENDPOINT = '/version.json';
const CURRENT_BUILD_VERSION = Config.PUBLIC_BUILD_VERSION ?? null;
const ALLOWED_WEB_UPDATE_HOSTS = new Set([
	'web.fluxer.app',
	'web.canary.fluxer.app',
	'fluxer.com',
	'canary.fluxer.com',
]);

function loadUpdaterModals(): Promise<typeof UpdaterModalCommands> {
	return import('@app/features/updater/commands/UpdaterModalCommands');
}

function showUpdaterModal(show: (modals: typeof UpdaterModalCommands) => void): void {
	void loadUpdaterModals().then(show, (error) => {
		logger.warn('Failed to load the updater modals', error);
	});
}

async function dropCachedAppShell(): Promise<void> {
	const browserCaches = getProtectedCacheStorage();
	if (!browserCaches) return;
	try {
		const cacheNames = await browserCaches.keys();
		await Promise.all(
			cacheNames
				.filter((cacheName) => cacheName.startsWith(WORKER_NAVIGATION_CACHE_PREFIX))
				.map((cacheName) => browserCaches.delete(cacheName)),
		);
	} catch (error) {
		logger.warn('Failed to drop the cached app shell before reloading', error);
	}
}

function normalizeUpdaterContext(context: NativeUpdaterEvent['context']): UpdaterContext {
	switch (context) {
		case 'user':
		case 'background':
		case 'focus':
			return context;
		default:
			return 'background';
	}
}

function normalizeUpdaterEvent(event: NativeUpdaterEvent): UpdaterEvent | null {
	const context = normalizeUpdaterContext(event.context);
	switch (event.type) {
		case 'checking':
			return {type: 'checking', context};
		case 'available':
			return {
				type: 'available',
				context,
				version: event.version ?? null,
				downloadUrl: event.downloadUrl,
				downloadOptions: event.downloadOptions,
			};
		case 'not-available':
			return {type: 'not-available', context};
		case 'downloaded':
			return {type: 'downloaded', context};
		case 'progress':
			return null;
		case 'error':
			return {
				type: 'error',
				context,
				message: event.message ?? 'Unknown updater error',
			};
		case 'unsupported':
			if (event.reason !== 'platform' && event.reason !== 'unpackaged' && event.reason !== 'managed-package') {
				return null;
			}
			return {
				type: 'unsupported',
				context,
				reason: event.reason,
				downloadUrl: event.downloadUrl,
			};
	}
}

class Updater {
	private snapshot: UpdaterMachineSnapshot = createUpdaterMachineSnapshot();
	currentVersion: string | null = null;
	channel: string | null = null;
	private desktopArch: string | null = null;
	private isNative: boolean;
	private backgroundCheckStarted = false;
	private backgroundCheckInterval: number | null = null;
	private backgroundCheckCleanups: Array<() => void> = [];
	private unsubscribeNativeEvents: (() => void) | null = null;
	private unsubscribeDesktopUpdate: (() => void) | null = null;
	private desktopUpdateReported = false;
	private desktopUpdateRunning = false;
	private olderShellModuleUpdateReady = false;
	private olderShellUpdateDownloaded = false;
	private olderShellInstallWhenDownloaded = false;
	private desktopUpdateStarting = false;
	private pendingManualDownloadRefreshes = 0;
	private checkInProgress = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.isNative = isElectron();
		initializeStore(this, () => this.bootstrap());
	}

	get updateInfo(): UpdateInfo {
		return this.snapshot.context.updateInfo;
	}

	get lastCheckedAt(): number | null {
		return this.snapshot.context.lastCheckedAt;
	}

	get nativeUnsupported(): {
		reason: 'platform' | 'unpackaged' | 'managed-package';
		downloadUrl: string | null;
	} | null {
		return this.snapshot.context.nativeUnsupported;
	}

	get nativeManualDownloadUrl(): string | null {
		return this.snapshot.context.nativeManualDownloadUrl;
	}

	get nativeManualDownloadOptions(): ReadonlyArray<UpdaterDownloadOption> {
		return this.snapshot.context.nativeManualDownloadOptions;
	}

	private get nativeCheckFailed(): boolean {
		return this.snapshot.context.nativeCheckFailed;
	}

	private get manualNativeDownloadInFlight(): boolean {
		return this.snapshot.context.manualNativeDownloadInFlight;
	}

	get desktopUpdateInProgress(): boolean {
		return this.desktopUpdateRunning;
	}

	get desktopUpdateAvailable(): boolean {
		return this.desktopUpdateReported || this.olderShellModuleUpdateReady || this.olderShellUpdateAvailable;
	}

	private get olderShellUpdateAvailable(): boolean {
		const electronApi = getElectronAPI();
		if (electronApi == null || electronApi.desktopUpdate != null || electronApi.updaterInstall == null) {
			return false;
		}
		return this.olderShellUpdateDownloaded || (this.updateInfo.native.available && !this.hasManualNativeDownload);
	}

	get hasUpdate(): boolean {
		return this.desktopUpdateAvailable || this.updateInfo.native.available || this.updateInfo.web.available;
	}

	get hasManualNativeDownload(): boolean {
		return hasManualNativeDownload(this.snapshot);
	}

	get nativeManualUpdateAvailable(): boolean {
		return this.updateInfo.native.available && this.hasManualNativeDownload;
	}

	get isChecking(): boolean {
		return this.snapshot.context.isChecking;
	}

	get displayVersion(): string | null {
		return getUpdaterDisplayVersion(this.snapshot);
	}

	private transition(event: UpdaterMachineEvent): void {
		runInAction(() => {
			this.snapshot = transitionUpdaterMachineSnapshot(this.snapshot, event);
		});
	}

	private async bootstrap(): Promise<void> {
		if (getElectronAPI()?.offlineBuild === true) {
			return;
		}
		if (this.isNative) {
			await this.bootstrapNative();
			await this.bootstrapDesktopUpdate();
		}
		this.startBackgroundChecks();
		void this.checkForUpdates(false);
	}

	private async bootstrapNative(): Promise<void> {
		try {
			const info = await getClientInfo();
			runInAction(() => {
				this.currentVersion = info.desktopVersion ?? null;
				this.channel = info.desktopChannel ?? null;
				this.desktopArch = info.desktopArch ?? info.arch ?? null;
			});
		} catch (error) {
			logger.warn('Failed to read desktop info', error);
		}
		this.subscribeToNativeEvents();
		void loadUpdaterModals().catch((error) => {
			logger.debug('Failed to preload the updater modals', error);
		});
	}

	private async bootstrapDesktopUpdate(): Promise<void> {
		const electronApi = getElectronAPI();
		const desktopUpdate = electronApi?.desktopUpdate;
		if (desktopUpdate == null) {
			await this.bootstrapOlderShellModuleUpdate();
			return;
		}
		this.unsubscribeDesktopUpdate = desktopUpdate.onStateChanged((state) => {
			runInAction(() => {
				this.desktopUpdateReported = state.available;
				this.desktopUpdateRunning = state.updating === true;
			});
		});
		await this.refreshDesktopUpdateState();
	}

	private async bootstrapOlderShellModuleUpdate(): Promise<void> {
		const desktopModules = getElectronAPI()?.desktopModules;
		const onPendingUpdateChanged = desktopModules?.onPendingUpdateChanged;
		const pendingUpdate = desktopModules?.pendingUpdate;
		if (onPendingUpdateChanged == null || pendingUpdate == null) return;
		this.unsubscribeDesktopUpdate = onPendingUpdateChanged((pending) => {
			runInAction(() => {
				this.olderShellModuleUpdateReady = pending != null;
			});
		});
		try {
			const pending = await pendingUpdate();
			runInAction(() => {
				this.olderShellModuleUpdateReady = pending != null;
			});
		} catch (error) {
			logger.warn('Failed to read the pending desktop module update', error);
		}
	}

	private async refreshDesktopUpdateState(): Promise<void> {
		const desktopUpdate = getElectronAPI()?.desktopUpdate;
		if (desktopUpdate == null) return;
		try {
			const state = await desktopUpdate.state();
			runInAction(() => {
				this.desktopUpdateReported = state.available;
				this.desktopUpdateRunning = state.updating === true;
			});
		} catch (error) {
			logger.warn('Failed to read the desktop update state', error);
		}
	}

	private async startDesktopUpdate(): Promise<void> {
		const electronApi = getElectronAPI();
		if (electronApi == null || this.desktopUpdateStarting) return;
		this.desktopUpdateStarting = true;
		try {
			flushPendingPersistWrites();
			if (electronApi.desktopUpdate != null) {
				await electronApi.desktopUpdate.start();
			} else if (this.olderShellModuleUpdateReady) {
				await electronApi.desktopModules?.applyPendingUpdate?.();
			} else if (this.olderShellUpdateDownloaded) {
				await electronApi.updaterInstall?.();
			} else {
				this.olderShellInstallWhenDownloaded = true;
				await electronApi.updaterDownload?.('user');
			}
		} catch (error) {
			this.olderShellInstallWhenDownloaded = false;
			logger.warn('Failed to start the desktop update', error);
		} finally {
			this.desktopUpdateStarting = false;
		}
	}

	private handleOlderShellUpdateDownloaded(): void {
		this.olderShellUpdateDownloaded = true;
		if (!this.olderShellInstallWhenDownloaded) return;
		this.olderShellInstallWhenDownloaded = false;
		void getElectronAPI()
			?.updaterInstall?.()
			.catch((error: unknown) => {
				logger.warn('Failed to install the downloaded desktop update', error);
			});
	}

	private subscribeToNativeEvents(): void {
		const electronApi = getElectronAPI();
		if (!electronApi) return;
		this.unsubscribeNativeEvents = electronApi.onUpdaterEvent((event) => {
			const updaterEvent = normalizeUpdaterEvent(event);
			if (!updaterEvent) {
				if (event.type !== 'progress') {
					logger.warn('Ignored malformed native updater event', {event});
				}
				return;
			}
			this.handleNativeEvent(updaterEvent);
		});
	}

	private handleNativeEvent(event: UpdaterEvent): void {
		const isManualDownloadRefreshResult =
			event.context === 'user' &&
			this.pendingManualDownloadRefreshes > 0 &&
			(event.type === 'available' ||
				event.type === 'not-available' ||
				event.type === 'error' ||
				event.type === 'unsupported');
		if (isManualDownloadRefreshResult) {
			this.pendingManualDownloadRefreshes -= 1;
		}
		const isUserCheck = event.context === 'user' && !isManualDownloadRefreshResult;
		const shouldShowImmediateUserResult = isUserCheck && !this.checkInProgress;
		switch (event.type) {
			case 'checking':
				this.transition({type: 'check.started'});
				break;
			case 'available': {
				const manualDownloadOptions = this.resolveManualNativeDownloadOptions(event, event.downloadOptions ?? []);
				this.transition({
					type: 'native.available',
					version: event.version ?? null,
					downloadUrl: manualDownloadOptions[0]?.url ?? event.downloadUrl ?? null,
					downloadOptions: manualDownloadOptions,
				});
				if (shouldShowImmediateUserResult) {
					this.showCurrentUpdateState();
				}
				break;
			}
			case 'not-available':
				this.transition({type: 'native.notAvailable', now: Date.now()});
				if (shouldShowImmediateUserResult) {
					this.showCurrentUpdateState();
				}
				break;
			case 'downloaded':
				this.handleOlderShellUpdateDownloaded();
				break;
			case 'error':
				this.olderShellInstallWhenDownloaded = false;
				if (isUserCheck) {
					logger.warn('Update check error:', event.message);
				} else {
					logger.debug('Background update check failed silently:', event.message);
				}
				this.transition({type: 'native.error'});
				if (isUserCheck) {
					showUpdaterModal((modals) => modals.pushUpdateCheckFailedModal());
				}
				break;
			case 'unsupported':
				this.transition({
					type: 'native.unsupported',
					reason: event.reason ?? 'platform',
					downloadUrl: event.downloadUrl ?? null,
					now: Date.now(),
				});
				if (shouldShowImmediateUserResult) {
					const reason = event.reason ?? 'platform';
					const downloadUrl = event.downloadUrl ?? null;
					showUpdaterModal((modals) => modals.pushUnsupportedUpdateModal(reason, downloadUrl));
				}
				break;
		}
	}

	private resolveManualNativeDownloadOptions(
		event: Extract<UpdaterEvent, {type: 'available'}>,
		options: ReadonlyArray<UpdaterDownloadOption>,
	): ReadonlyArray<UpdaterDownloadOption> {
		if (getElectronAPI()?.platform !== 'linux') {
			return options;
		}
		return buildLinuxManualUpdateOptions({
			downloadUrl: event.downloadUrl ?? options[0]?.url ?? null,
			channel: this.channel ?? Config.PUBLIC_RELEASE_CHANNEL,
			arch: this.desktopArch,
			version: event.version ?? null,
			knownOptions: options,
		});
	}

	private startBackgroundChecks(): void {
		if (this.backgroundCheckStarted) return;
		this.backgroundCheckStarted = true;
		this.backgroundCheckInterval = window.setInterval(() => {
			if (document.visibilityState === 'visible') {
				void this.checkForUpdates(false);
			}
		}, CHECK_INTERVAL_MS);
		const onFocus = () => void this.checkForUpdates(false);
		const onOnline = () => void this.checkForUpdates(true);
		const onVisibilityChange = () => {
			if (document.visibilityState === 'visible') {
				void this.checkForUpdates(false);
			}
		};
		window.addEventListener('focus', onFocus);
		window.addEventListener('online', onOnline);
		document.addEventListener('visibilitychange', onVisibilityChange);
		this.backgroundCheckCleanups.push(
			() => window.removeEventListener('focus', onFocus),
			() => window.removeEventListener('online', onOnline),
			() => document.removeEventListener('visibilitychange', onVisibilityChange),
		);
	}

	private shouldThrottle(force: boolean): boolean {
		if (force) return false;
		if (this.lastCheckedAt == null) return false;
		return Date.now() - this.lastCheckedAt < MIN_CHECK_INTERVAL_MS;
	}

	private shouldRunNativeCheck(userInitiated: boolean): boolean {
		if (!this.isNative) return false;
		if (this.nativeUnsupported && !userInitiated) return false;
		if (userInitiated) return true;
		return !this.updateInfo.native.available;
	}

	async checkForUpdates(force = false, userInitiated = false): Promise<void> {
		if (this.checkInProgress) {
			return;
		}
		if (this.shouldThrottle(force)) {
			if (userInitiated) {
				this.showCurrentUpdateState();
			}
			return;
		}

		this.checkInProgress = true;
		this.transition({type: 'check.started'});

		const checkContext: 'user' | 'background' = userInitiated ? 'user' : 'background';
		let failed = false;
		try {
			const shouldCheckNative = this.shouldRunNativeCheck(userInitiated);
			const [, webResult] = await Promise.all([
				shouldCheckNative ? this.checkNativeUpdate(checkContext) : Promise.resolve(null),
				this.checkWebUpdate(),
			]);
			if (webResult) {
				this.transition({type: 'web.checked', available: webResult.available, version: webResult.version});
			}
			if (userInitiated && shouldCheckNative) {
				await this.refreshDesktopUpdateState();
			}
			if (userInitiated && (!shouldCheckNative || (!this.isChecking && !this.nativeCheckFailed))) {
				this.showCurrentUpdateState();
			}
		} catch (err) {
			failed = true;
			logger.debug('Update check failed silently:', err);
			if (userInitiated) {
				showUpdaterModal((modals) => modals.pushUpdateCheckFailedModal());
			}
		} finally {
			this.checkInProgress = false;
			this.transition({type: failed ? 'check.failed' : 'check.finished', now: Date.now()});
		}
	}

	private async checkNativeUpdate(context: 'user' | 'background'): Promise<boolean> {
		const electronApi = getElectronAPI();
		if (!electronApi) return false;
		let timeoutId: number | undefined;
		const timedOut = new Promise<false>((resolve) => {
			timeoutId = window.setTimeout(() => resolve(false), NATIVE_CHECK_TIMEOUT_MS);
		});
		try {
			return await Promise.race([electronApi.updaterCheck(context).then(() => true), timedOut]);
		} catch (error) {
			logger.debug('Native update check failed silently:', error);
			return false;
		} finally {
			window.clearTimeout(timeoutId);
		}
	}

	private async checkWebUpdate(): Promise<{
		available: boolean;
		version: string | null;
	} | null> {
		if (isDesktop()) {
			return {available: false, version: null};
		}
		if (!ALLOWED_WEB_UPDATE_HOSTS.has(window.location.host)) {
			return {available: false, version: null};
		}
		try {
			const response = await fetch(VERSION_ENDPOINT, {
				cache: 'no-store',
				headers: {'Cache-Control': 'no-cache'},
				signal: AbortSignal.timeout(WEB_CHECK_TIMEOUT_MS),
			});
			if (!response.ok) {
				logger.debug('Version endpoint not available');
				return null;
			}
			const payload = (await response.json()) as {
				version?: string;
				buildVersion?: string;
			};
			const version = payload.version ?? payload.buildVersion ?? null;
			if (!version) {
				return null;
			}
			const updateAvailable = Boolean(CURRENT_BUILD_VERSION && version !== CURRENT_BUILD_VERSION);
			return {
				available: updateAvailable,
				version,
			};
		} catch (error) {
			logger.debug('Failed to fetch version info silently:', error);
			return null;
		}
	}

	async applyUpdate(): Promise<void> {
		if (!this.hasUpdate) return;
		if (this.desktopUpdateAvailable) {
			await this.startDesktopUpdate();
			return;
		}
		if (this.isNative && this.nativeUnsupported?.reason === 'managed-package' && !this.updateInfo.web.available) {
			showUpdaterModal((modals) => modals.pushUnsupportedUpdateModal('managed-package'));
			return;
		}
		if (this.isNative && this.nativeManualUpdateAvailable && !this.nativeUnsupported) {
			this.showManualNativeUpdateModal();
			return;
		}
		if (this.updateInfo.web.available) {
			logger.info('Applying web update, reloading...');
			await dropCachedAppShell();
			window.location.reload();
			return;
		}
		if (this.isNative && this.updateInfo.native.available) {
			logger.info('Native update is available but not installable in-app; opening desktop downloads...');
			const url = this.nativeManualDownloadUrl ?? this.nativeUnsupported?.downloadUrl ?? null;
			if (url) {
				await openExternalUrl(url);
			} else if (this.nativeUnsupported) {
				const {reason, downloadUrl} = this.nativeUnsupported;
				showUpdaterModal((modals) => modals.pushUnsupportedUpdateModal(reason, downloadUrl));
			} else {
				await openExternalUrl(DESKTOP_DOWNLOAD_URL);
			}
		}
	}

	private showCurrentUpdateState(): void {
		if (this.desktopUpdateAvailable) {
			showUpdaterModal((modals) => modals.pushDesktopUpdateAvailableModal(() => this.startDesktopUpdate()));
			return;
		}
		if (this.nativeManualUpdateAvailable) {
			this.showManualNativeUpdateModal();
			return;
		}
		if (this.hasUpdate) {
			return;
		}
		if (this.nativeUnsupported) {
			const {reason, downloadUrl} = this.nativeUnsupported;
			showUpdaterModal((modals) => modals.pushUnsupportedUpdateModal(reason, downloadUrl));
			return;
		}
		const currentVersion = this.currentVersion;
		showUpdaterModal((modals) => modals.pushUpToDateModal(currentVersion));
	}

	private getManualUpdateSuggestedName(url: string): string {
		try {
			const parsed = new URL(url);
			const fileName = parsed.pathname.split('/').filter(Boolean).pop();
			if (!fileName) return 'Fluxer-update';
			return decodeURIComponent(fileName);
		} catch {
			return 'Fluxer-update';
		}
	}

	private showManualNativeUpdateModal(): void {
		const version = this.updateInfo.native.version;
		const options = this.nativeManualDownloadOptions;
		if (options.length > 0) {
			const currentVersion = this.currentVersion;
			showUpdaterModal((modals) =>
				modals.pushManualUpdateAvailableModal({
					currentVersion,
					version,
					options,
					onDownload: (option) => this.downloadManualNativeUpdateOption(option),
				}),
			);
			return;
		}
		const url = this.nativeManualDownloadUrl ?? this.nativeUnsupported?.downloadUrl ?? DESKTOP_DOWNLOAD_URL;
		showUpdaterModal((modals) =>
			modals.pushUpdateAvailableModal(version, () => this.downloadManualNativeUpdateOrOpen(url)),
		);
	}

	private async refreshManualNativeDownloadOption(option: UpdaterDownloadOption): Promise<UpdaterDownloadOption> {
		if (this.checkInProgress) {
			return option;
		}
		this.checkInProgress = true;
		this.transition({type: 'check.started'});
		let timeoutId: number | undefined;
		const timedOut = new Promise<boolean>((resolve) => {
			timeoutId = window.setTimeout(() => resolve(false), MANUAL_DOWNLOAD_REFRESH_TIMEOUT_MS);
		});
		try {
			this.pendingManualDownloadRefreshes += 1;
			const checked = await Promise.race([this.checkNativeUpdate('user'), timedOut]);
			if (!checked) {
				return option;
			}
			return this.nativeManualDownloadOptions.find((candidate) => candidate.format === option.format) ?? option;
		} finally {
			window.clearTimeout(timeoutId);
			this.checkInProgress = false;
			this.transition({type: 'check.finished', now: Date.now()});
		}
	}

	private async downloadManualNativeUpdateOption(option: UpdaterDownloadOption): Promise<void> {
		if (this.manualNativeDownloadInFlight) {
			return;
		}
		this.transition({type: 'manualDownload.started'});
		let currentOption = option;
		try {
			currentOption = await this.refreshManualNativeDownloadOption(option);
		} finally {
			this.transition({type: 'manualDownload.finished'});
		}
		await this.downloadManualNativeUpdateOrOpen(currentOption.url, currentOption.suggestedName, currentOption.sha256);
	}

	private async downloadManualNativeUpdateOrOpen(
		url: string,
		suggestedName?: string,
		sha256?: string | null,
	): Promise<void> {
		if (this.manualNativeDownloadInFlight) {
			return;
		}
		this.transition({type: 'manualDownload.started'});
		try {
			const outcome = await downloadWithNative({
				url,
				suggestedName: suggestedName ?? this.getManualUpdateSuggestedName(url),
				sha256,
			});
			if (outcome === 'success' || outcome === 'canceled') {
				return;
			}
			if (outcome === 'checksum-mismatch') {
				logger.error('Native manual update download did not match its published checksum', {url});
				showUpdaterModal((modals) => modals.pushDesktopUpdateDownloadFailedModal());
				return;
			}
			logger.warn('Native manual update download unavailable; opening update URL externally', {outcome});
			await openExternalUrl(url);
		} finally {
			this.transition({type: 'manualDownload.finished'});
		}
	}

	dispose(): void {
		if (this.unsubscribeNativeEvents) {
			this.unsubscribeNativeEvents();
			this.unsubscribeNativeEvents = null;
		}
		if (this.unsubscribeDesktopUpdate) {
			this.unsubscribeDesktopUpdate();
			this.unsubscribeDesktopUpdate = null;
		}
		this.desktopUpdateReported = false;
		this.desktopUpdateRunning = false;
		this.olderShellModuleUpdateReady = false;
		this.olderShellUpdateDownloaded = false;
		this.olderShellInstallWhenDownloaded = false;
		if (this.backgroundCheckInterval != null) {
			window.clearInterval(this.backgroundCheckInterval);
			this.backgroundCheckInterval = null;
		}
		for (const cleanup of this.backgroundCheckCleanups) {
			cleanup();
		}
		this.backgroundCheckCleanups = [];
		this.backgroundCheckStarted = false;
	}
}

export default new Updater();
