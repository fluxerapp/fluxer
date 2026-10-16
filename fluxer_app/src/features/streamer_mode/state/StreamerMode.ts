// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent, stopPersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

const POLL_INTERVAL_MS = 15_000;
const STORAGE_KEY = 'StreamerMode';
const logger = new Logger('StreamerMode');

class StreamerModeState {
	manualEnabled = false;
	autoEnable = true;
	hidePersonalInformation = true;
	hideInviteLinks = true;
	disableSounds = true;
	disableNotifications = true;
	autoDetectedCaptureApp = false;
	nagbarDismissed = false;
	private pollTimer: NodeJS.Timeout | null = null;
	private pollInFlight = false;
	private isPersisting = false;
	private autoPollingDisposer: (() => void) | null = null;

	constructor() {
		makeAutoObservable<StreamerModeState, 'pollTimer' | 'pollInFlight' | 'isPersisting' | 'autoPollingDisposer'>(
			this,
			{
				pollTimer: false,
				pollInFlight: false,
				isPersisting: false,
				autoPollingDisposer: false,
			},
			{autoBind: true},
		);
		initializeStore(this, () => this.initPersistence());
		if (typeof window !== 'undefined') {
			this.autoPollingDisposer = reaction(
				() => this.autoEnable,
				() => this.updateCaptureAppPolling(),
				{fireImmediately: true},
			);
		}
	}

	private async initPersistence(): Promise<void> {
		if (this.isPersisting) return;
		this.isPersisting = true;
		await makePersistent(
			this,
			STORAGE_KEY,
			[
				'manualEnabled',
				'autoEnable',
				'hidePersonalInformation',
				'hideInviteLinks',
				'disableSounds',
				'disableNotifications',
				'nagbarDismissed',
			],
			{syncAcrossTabs: true},
		);
		this.updateCaptureAppPolling();
	}

	cleanup(): void {
		if (this.pollTimer) {
			clearInterval(this.pollTimer);
			this.pollTimer = null;
		}
		this.autoPollingDisposer?.();
		this.autoPollingDisposer = null;
		if (this.isPersisting) {
			stopPersistent(STORAGE_KEY, this);
			this.isPersisting = false;
		}
	}

	get enabled(): boolean {
		return this.manualEnabled || (this.autoEnable && this.autoDetectedCaptureApp);
	}

	get shouldHidePersonalInformation(): boolean {
		return this.enabled && this.hidePersonalInformation;
	}

	get shouldTruncateUsernames(): boolean {
		return this.shouldHidePersonalInformation;
	}

	get shouldHideInviteLinks(): boolean {
		return this.enabled && this.hideInviteLinks;
	}

	get shouldDisableSounds(): boolean {
		return this.enabled && this.disableSounds;
	}

	get shouldDisableNotifications(): boolean {
		return this.enabled && this.disableNotifications;
	}

	get shouldShowNagbar(): boolean {
		return this.enabled && !this.nagbarDismissed;
	}

	setManualEnabled(enabled: boolean): void {
		this.manualEnabled = enabled;
		this.nagbarDismissed = false;
	}

	setAutoEnable(enabled: boolean): void {
		this.autoEnable = enabled;
		this.nagbarDismissed = false;
		this.updateCaptureAppPolling();
	}

	setHidePersonalInformation(enabled: boolean): void {
		this.hidePersonalInformation = enabled;
	}

	setHideInviteLinks(enabled: boolean): void {
		this.hideInviteLinks = enabled;
	}

	setDisableSounds(enabled: boolean): void {
		this.disableSounds = enabled;
	}

	setDisableNotifications(enabled: boolean): void {
		this.disableNotifications = enabled;
	}

	disable(): void {
		this.manualEnabled = false;
		this.autoEnable = false;
		this.autoDetectedCaptureApp = false;
		this.nagbarDismissed = false;
		this.updateCaptureAppPolling();
	}

	dismissNagbar(): void {
		this.nagbarDismissed = true;
	}

	private updateCaptureAppPolling(): void {
		if (!this.autoEnable) {
			this.stopCaptureAppPolling();
			this.autoDetectedCaptureApp = false;
			return;
		}
		if (!getElectronAPI()?.getStreamerModeCaptureAppStatus) {
			this.autoDetectedCaptureApp = false;
			return;
		}
		if (this.pollTimer == null) {
			this.pollTimer = setInterval(() => {
				void this.refreshCaptureAppStatus();
			}, POLL_INTERVAL_MS);
		}
		void this.refreshCaptureAppStatus();
	}

	private stopCaptureAppPolling(): void {
		if (!this.pollTimer) return;
		clearInterval(this.pollTimer);
		this.pollTimer = null;
	}

	private async refreshCaptureAppStatus(): Promise<void> {
		if (this.pollInFlight || !this.autoEnable) return;
		const electronApi = getElectronAPI();
		if (!electronApi?.getStreamerModeCaptureAppStatus) return;
		this.pollInFlight = true;
		try {
			const status = await electronApi.getStreamerModeCaptureAppStatus();
			runInAction(() => {
				const wasDetected = this.autoDetectedCaptureApp;
				this.autoDetectedCaptureApp = status.detected;
				if (status.detected && !wasDetected) {
					this.nagbarDismissed = false;
				}
			});
		} catch (error) {
			logger.warn('Failed to refresh streamer mode capture app status:', error);
		} finally {
			runInAction(() => {
				this.pollInFlight = false;
			});
		}
	}
}

export default new StreamerModeState();
