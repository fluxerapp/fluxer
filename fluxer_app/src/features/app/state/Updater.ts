// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {DESKTOP_DOWNLOAD_URL} from '@app/features/app/config/I18nDisplayConstants';
import {
	shouldShowNativeDesktopUpdateDownloadProgress,
	shouldShowNativeDesktopUpdateInApp,
} from '@app/features/app/utils/UpdaterPlatformUtils';
import type {UpdaterContext, UpdaterDownloadOption, UpdaterEvent} from '@app/features/platform/types/Electron';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getClientInfo} from '@app/features/platform/utils/ClientInfo';
import {downloadWithNative, getElectronAPI, isElectron, openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {
	pushDesktopUpdateDownloadFailedModal,
	pushDesktopUpdateInstallFailedModal,
	pushManualUpdateAvailableModal,
	pushUnsupportedUpdateModal,
	pushUpdateAvailableModal,
	pushUpdateCheckFailedModal,
	pushUpdateReadyModal,
	pushUpToDateModal,
} from '@app/features/updater/commands/UpdaterModalCommands';
import {
	createUpdaterMachineSnapshot,
	getUpdaterDisplayVersion,
	getUpdaterMachineStateValue,
	getUpdaterUpdateType,
	hasManualNativeDownload,
	type NativeDownloadProgress,
	type NativeUpdateInfo,
	transitionUpdaterMachineSnapshot,
	type UpdateInfo,
	type UpdaterMachineEvent,
	type UpdaterMachineSnapshot,
	type UpdaterState,
	type UpdateType,
	type WebUpdateInfo,
} from '@app/features/updater/state/UpdaterStateMachine';
import {buildLinuxManualUpdateOptions} from '@app/features/updater/utils/LinuxManualUpdateOptions';
import type {UpdaterEvent as NativeUpdaterEvent} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {makeAutoObservable, runInAction} from 'mobx';

export type {NativeDownloadProgress, NativeUpdateInfo, UpdateInfo, UpdaterState, UpdateType, WebUpdateInfo};

export const DOWNLOADING_UPDATE_DESCRIPTOR = msg({
	message: 'Downloading desktop update…',
	comment: 'Short desktop updater status label.',
});

const logger = new Logger('Updater');
const CHECK_INTERVAL_MS = 30 * 60 * 1000;
const MIN_CHECK_INTERVAL_MS = 60 * 1000;
const VERSION_ENDPOINT = '/version.json';
const CURRENT_BUILD_VERSION = Config.PUBLIC_BUILD_VERSION ?? null;
const ALLOWED_WEB_UPDATE_HOSTS = new Set(['web.fluxer.app', 'web.canary.fluxer.app']);

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
				downloadSize: event.downloadSize ?? null,
				downloadStarted: event.downloadStarted ?? true,
				downloadUrl: event.downloadUrl,
				downloadOptions: event.downloadOptions,
			};
		case 'not-available':
			return {type: 'not-available', context};
		case 'downloaded':
			return {type: 'downloaded', context, version: event.version ?? null};
		case 'progress':
			if (
				typeof event.percent !== 'number' ||
				typeof event.transferred !== 'number' ||
				typeof event.total !== 'number' ||
				typeof event.bytesPerSecond !== 'number'
			) {
				return null;
			}
			return {
				type: 'progress',
				context,
				percent: event.percent,
				transferred: event.transferred,
				total: event.total,
				bytesPerSecond: event.bytesPerSecond,
			};
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

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.isNative = isElectron();
		void this.bootstrap();
	}

	get updateType(): UpdateType {
		return getUpdaterUpdateType(this.snapshot);
	}

	get updateInfo(): UpdateInfo {
		return this.snapshot.context.updateInfo;
	}

	get downloadProgress(): NativeDownloadProgress | null {
		return this.snapshot.context.downloadProgress;
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

	private get checkInProgress(): boolean {
		return this.snapshot.context.checkInProgress;
	}

	private get nativeCheckFailed(): boolean {
		return this.snapshot.context.nativeCheckFailed;
	}

	private get manualNativeDownloadInFlight(): boolean {
		return this.snapshot.context.manualNativeDownloadInFlight;
	}

	get hasUpdate(): boolean {
		return this.updateInfo.native.available || this.updateInfo.web.available;
	}

	get nativeUpdatePending(): boolean {
		return this.updateInfo.native.available && !this.updateInfo.native.downloaded;
	}

	get nativeUpdateReady(): boolean {
		return this.updateInfo.native.available && this.updateInfo.native.downloaded;
	}

	get nativeDownloadInFlight(): boolean {
		return this.updateInfo.native.downloading && !this.updateInfo.native.downloaded;
	}

	get nativeDownloadProgressSupported(): boolean {
		return shouldShowNativeDesktopUpdateDownloadProgress(getElectronAPI()?.platform);
	}

	get hasManualNativeDownload(): boolean {
		return hasManualNativeDownload(this.snapshot);
	}

	get nativeAwaitingDownload(): boolean {
		if (this.hasManualNativeDownload) return false;
		return (
			this.updateInfo.native.available && !this.updateInfo.native.downloaded && !this.updateInfo.native.downloading
		);
	}

	get nativeManualUpdateAvailable(): boolean {
		return (
			this.updateInfo.native.available &&
			!this.updateInfo.native.downloaded &&
			!this.updateInfo.native.downloading &&
			this.hasManualNativeDownload
		);
	}

	get state(): UpdaterState {
		return getUpdaterMachineStateValue(this.snapshot);
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
		if (this.isNative) {
			await this.bootstrapNative();
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
	}

	private subscribeToNativeEvents(): void {
		const electronApi = getElectronAPI();
		if (!electronApi) return;
		this.unsubscribeNativeEvents = electronApi.onUpdaterEvent((event) => {
			const updaterEvent = normalizeUpdaterEvent(event);
			if (!updaterEvent) {
				logger.warn('Ignored malformed native updater event', {event});
				return;
			}
			this.handleNativeEvent(updaterEvent);
		});
	}

	private handleNativeEvent(event: UpdaterEvent): void {
		const isUserCheck = event.context === 'user';
		const isBackgroundOrFocusCheck = event.context === 'background' || event.context === 'focus';
		const shouldSurfaceNativeDesktopUpdate = this.shouldSurfaceNativeDesktopUpdate();
		const shouldShowImmediateUserResult = isUserCheck && !this.checkInProgress;
		switch (event.type) {
			case 'checking':
				this.transition({type: 'check.started'});
				break;
			case 'available': {
				const downloadStarted = event.downloadStarted ?? true;
				const manualDownloadOptions = this.resolveManualNativeDownloadOptions(event, event.downloadOptions ?? []);
				const manualDownloadUrl = manualDownloadOptions[0]?.url ?? event.downloadUrl ?? null;
				if (!shouldSurfaceNativeDesktopUpdate) {
					this.transition({
						type: 'native.hidden',
						reason: 'platform',
						downloadUrl: manualDownloadUrl,
						now: Date.now(),
					});
					if (isUserCheck) {
						pushUnsupportedUpdateModal('platform', manualDownloadUrl ?? DESKTOP_DOWNLOAD_URL);
					}
					break;
				}
				this.transition({
					type: 'native.available',
					version: event.version ?? null,
					downloadSize: event.downloadSize ?? null,
					downloadStarted,
					downloadUrl: manualDownloadUrl,
					downloadOptions: manualDownloadOptions,
				});
				if (shouldShowImmediateUserResult) {
					this.showCurrentUpdateState();
				}
				break;
			}
			case 'not-available':
				this.transition({type: 'native.notAvailable', now: Date.now()});
				if (isUserCheck && !this.checkInProgress) {
					this.showCurrentUpdateState();
				}
				break;
			case 'error': {
				const phase = event.phase ?? 'check';
				if (isBackgroundOrFocusCheck) {
					logger.debug('Background update failed silently:', event.message);
				} else {
					logger.warn(`Update ${phase} error:`, event.message);
				}
				this.transition({type: 'native.error'});
				if (isUserCheck) {
					if (phase === 'download') {
						pushDesktopUpdateDownloadFailedModal();
					} else if (phase === 'install') {
						pushDesktopUpdateInstallFailedModal();
					} else {
						pushUpdateCheckFailedModal();
					}
				}
				break;
			}
			case 'downloaded':
				if (!shouldSurfaceNativeDesktopUpdate) {
					this.transition({
						type: 'native.hidden',
						reason: 'platform',
						downloadUrl: null,
						now: Date.now(),
					});
					break;
				}
				this.transition({type: 'native.downloaded', version: event.version ?? null});
				if (shouldShowImmediateUserResult) {
					this.showCurrentUpdateState();
				}
				break;
			case 'progress':
				if (!shouldSurfaceNativeDesktopUpdate || !this.nativeDownloadProgressSupported) {
					break;
				}
				this.transition({
					type: 'native.progress',
					progress: {
						percent: event.percent,
						transferred: event.transferred,
						total: event.total,
						bytesPerSecond: event.bytesPerSecond,
					},
				});
				break;
			case 'unsupported':
				this.transition({
					type: 'native.unsupported',
					reason: event.reason ?? 'platform',
					downloadUrl: event.downloadUrl ?? null,
					now: Date.now(),
				});
				if (isUserCheck) {
					pushUnsupportedUpdateModal(event.reason ?? 'platform', event.downloadUrl ?? null);
				}
				break;
		}
	}

	private resolveManualNativeDownloadOptions(
		event: Extract<UpdaterEvent, {type: 'available'}>,
		options: ReadonlyArray<UpdaterDownloadOption>,
	): ReadonlyArray<UpdaterDownloadOption> {
		if ((event.downloadStarted ?? true) || getElectronAPI()?.platform !== 'linux') {
			return options;
		}
		return buildLinuxManualUpdateOptions({
			downloadUrl: event.downloadUrl ?? options[0]?.url ?? null,
			channel: this.channel ?? Config.PUBLIC_RELEASE_CHANNEL,
			arch: this.desktopArch,
			version: event.version ?? null,
			apiEndpoint: Config.PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT,
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
		if (this.nativeDownloadInFlight || this.updateInfo.native.installing) return false;
		if (userInitiated) return true;
		return !this.updateInfo.native.available;
	}

	private shouldSurfaceNativeDesktopUpdate(): boolean {
		return shouldShowNativeDesktopUpdateInApp(getElectronAPI()?.platform);
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

		this.transition({type: 'check.started'});

		const checkContext: 'user' | 'background' = userInitiated ? 'user' : 'background';
		let failed = false;
		try {
			const shouldCheckNative = this.shouldRunNativeCheck(userInitiated);
			const [, webResult] = await Promise.all([
				shouldCheckNative ? this.checkNativeUpdate(checkContext) : Promise.resolve(null),
				this.checkWebUpdate(),
			]);
			this.transition({
				type: 'web.checked',
				available: webResult?.available ?? false,
				version: webResult?.version ?? null,
			});
			if (userInitiated && (!shouldCheckNative || (!this.isChecking && !this.nativeCheckFailed))) {
				this.showCurrentUpdateState();
			}
		} catch (err) {
			failed = true;
			logger.debug('Update check failed silently:', err);
			if (userInitiated) {
				pushUpdateCheckFailedModal();
			}
		} finally {
			this.transition({type: failed ? 'check.failed' : 'check.finished', now: Date.now()});
		}
	}

	private async checkNativeUpdate(context: 'user' | 'background'): Promise<boolean> {
		const electronApi = getElectronAPI();
		if (!electronApi) return false;
		try {
			await electronApi.updaterCheck(context);
			return true;
		} catch (error) {
			logger.debug('Native update check failed silently:', error);
			return false;
		}
	}

	private async checkWebUpdate(): Promise<{
		available: boolean;
		version: string | null;
	}> {
		if (!ALLOWED_WEB_UPDATE_HOSTS.has(window.location.host)) {
			return {available: false, version: null};
		}
		try {
			const response = await fetch(VERSION_ENDPOINT, {
				cache: 'no-store',
				headers: {'Cache-Control': 'no-cache'},
			});
			if (!response.ok) {
				logger.debug('Version endpoint not available');
				return {available: false, version: null};
			}
			const payload = (await response.json()) as {
				version?: string;
				buildVersion?: string;
			};
			const version = payload.version ?? payload.buildVersion ?? null;
			const updateAvailable = Boolean(version && CURRENT_BUILD_VERSION && version !== CURRENT_BUILD_VERSION);
			return {
				available: updateAvailable,
				version,
			};
		} catch (error) {
			logger.debug('Failed to fetch version info silently:', error);
			return {available: false, version: null};
		}
	}

	async applyUpdate(): Promise<void> {
		if (!this.hasUpdate) return;
		const electronApi = getElectronAPI();
		if (this.isNative && this.updateInfo.native.downloaded && electronApi) {
			if (this.updateInfo.native.installing) {
				logger.debug('Install already in progress; ignoring duplicate click.');
				return;
			}
			this.transition({type: 'native.install.started'});
			logger.info('Installing downloaded native update...');
			try {
				await electronApi.updaterInstall();
			} catch (error) {
				logger.warn('Native update install failed', error);
				this.transition({type: 'native.install.failed'});
				pushDesktopUpdateInstallFailedModal();
			}
			return;
		}
		if (this.isNative && this.nativeAwaitingDownload && electronApi?.updaterDownload) {
			await this.startNativeDownload();
			return;
		}
		if (this.isNative && this.nativeDownloadInFlight) {
			return;
		}
		if (this.isNative && this.nativeUnsupported?.reason === 'managed-package' && !this.updateInfo.web.available) {
			pushUnsupportedUpdateModal('managed-package');
			return;
		}
		if (this.isNative && this.nativeManualUpdateAvailable && !this.nativeUnsupported) {
			this.showManualNativeUpdateModal();
			return;
		}
		if (this.updateInfo.web.available) {
			logger.info('Applying web update, reloading...');
			window.location.reload();
			return;
		}
		if (this.isNative && this.updateInfo.native.available) {
			logger.info('Native update is available but not installable in-app; opening desktop downloads...');
			const url = this.nativeManualDownloadUrl ?? this.nativeUnsupported?.downloadUrl ?? null;
			if (url) {
				await openExternalUrl(url);
			} else if (this.nativeUnsupported) {
				pushUnsupportedUpdateModal(this.nativeUnsupported.reason, this.nativeUnsupported.downloadUrl);
			} else {
				await openExternalUrl(DESKTOP_DOWNLOAD_URL);
			}
		}
	}

	async startNativeDownload(userInitiated = true): Promise<void> {
		const electronApi = getElectronAPI();
		if (!electronApi?.updaterDownload) return;
		if (!this.updateInfo.native.available) return;
		if (this.updateInfo.native.downloading || this.updateInfo.native.downloaded) return;
		this.transition({
			type: 'native.download.started',
			progressSupported: this.nativeDownloadProgressSupported,
			total: this.updateInfo.native.downloadSize,
		});
		try {
			await electronApi.updaterDownload(userInitiated ? 'user' : 'background');
		} catch (error) {
			logger.warn('Native update download failed', error);
			this.transition({type: 'native.download.failed'});
			if (userInitiated) {
				pushDesktopUpdateDownloadFailedModal();
			}
		}
	}

	private showCurrentUpdateState(): void {
		if (this.nativeManualUpdateAvailable) {
			this.showManualNativeUpdateModal();
			return;
		}
		if (this.nativeUpdateReady) {
			pushUpdateReadyModal(this.updateInfo.native.version, this.applyUpdate);
			return;
		}
		if (this.hasUpdate) {
			return;
		}
		if (this.nativeUnsupported) {
			pushUnsupportedUpdateModal(this.nativeUnsupported.reason, this.nativeUnsupported.downloadUrl);
			return;
		}
		pushUpToDateModal(this.currentVersion);
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
		if (this.nativeManualDownloadOptions.length > 0) {
			pushManualUpdateAvailableModal({
				currentVersion: this.currentVersion,
				version: this.updateInfo.native.version,
				options: this.nativeManualDownloadOptions,
				onDownload: (option) => this.downloadManualNativeUpdateOrOpen(option.url, option.suggestedName),
			});
			return;
		}
		const url = this.nativeManualDownloadUrl ?? this.nativeUnsupported?.downloadUrl ?? DESKTOP_DOWNLOAD_URL;
		pushUpdateAvailableModal(this.updateInfo.native.version, () => this.downloadManualNativeUpdateOrOpen(url));
	}

	private async downloadManualNativeUpdateOrOpen(url: string, suggestedName?: string): Promise<void> {
		if (this.manualNativeDownloadInFlight) {
			return;
		}
		this.transition({type: 'manualDownload.started'});
		try {
			const outcome = await downloadWithNative({
				url,
				suggestedName: suggestedName ?? this.getManualUpdateSuggestedName(url),
			});
			if (outcome === 'success' || outcome === 'canceled') {
				return;
			}
			logger.warn('Native manual update download unavailable; opening update URL externally', {outcome});
			await openExternalUrl(url);
		} finally {
			this.transition({type: 'manualDownload.finished'});
		}
	}

	reset(): void {
		this.transition({type: 'reset'});
	}

	dispose(): void {
		if (this.unsubscribeNativeEvents) {
			this.unsubscribeNativeEvents();
			this.unsubscribeNativeEvents = null;
		}
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
