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

import {makeAutoObservable, runInAction} from 'mobx';
import Config from '~/Config';
import {Logger} from '~/lib/Logger';
import {getClientInfo} from '~/utils/ClientInfoUtils';
import {getElectronAPI, isElectron} from '~/utils/NativeUtils';
import type {UpdaterEvent} from '../../src-electron/common/types';

const logger = new Logger('UpdaterStore');

const CHECK_INTERVAL_MS = 30 * 60 * 1000;
const MIN_CHECK_INTERVAL_MS = 60 * 1000;
const VERSION_ENDPOINT = '/version.json';
const CURRENT_BUILD_SHA = Config.PUBLIC_BUILD_SHA ?? null;

export type UpdaterState = 'idle' | 'checking' | 'available';

export type UpdateType = 'native' | 'web' | 'both' | null;

export interface NativeUpdateInfo {
	available: boolean;
	downloaded: boolean;
	version: string | null;
}

export interface WebUpdateInfo {
	available: boolean;
	sha: string | null;
	buildNumber: number | null;
}

export interface UpdateInfo {
	native: NativeUpdateInfo;
	web: WebUpdateInfo;
}

class UpdaterStoreImpl {
	updateType: UpdateType = null;
	updateInfo: UpdateInfo = {
		native: {available: false, downloaded: false, version: null},
		web: {available: false, sha: null, buildNumber: null},
	};
	lastCheckedAt: number | null = null;
	currentVersion: string | null = null;
	channel: string | null = null;

	private _isChecking = false;

	private isNative: boolean;
	private backgroundCheckStarted = false;
	private unsubscribeNativeEvents: (() => void) | null = null;
	private checkInProgress = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});

		this.isNative = isElectron();
		void this.bootstrap();
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

	get state(): UpdaterState {
		if (this._isChecking) return 'checking';
		if (this.hasUpdate) return 'available';
		return 'idle';
	}

	get isChecking(): boolean {
		return this._isChecking;
	}

	get displayVersion(): string | null {
		if (this.updateInfo.native.available && this.updateInfo.native.version) {
			return this.updateInfo.native.version;
		}
		if (this.updateInfo.web.available) {
			if (this.updateInfo.web.buildNumber) {
				return `Build ${this.updateInfo.web.buildNumber}`;
			}
			return this.updateInfo.web.sha?.slice(0, 7) ?? null;
		}
		return null;
	}

	private refreshUpdateType(): void {
		const nativeReady = this.nativeUpdateReady;
		const hasWeb = this.updateInfo.web.available;

		if (nativeReady && hasWeb) {
			this.updateType = 'both';
		} else if (nativeReady) {
			this.updateType = 'native';
		} else if (hasWeb) {
			this.updateType = 'web';
		} else {
			this.updateType = null;
		}
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
			});
		} catch (error) {
			logger.warn('Failed to read desktop info', error);
		}

		this.subscribeToNativeEvents();
	}

	private subscribeToNativeEvents(): void {
		const electronApi = getElectronAPI();
		if (!electronApi) return;

		this.unsubscribeNativeEvents = electronApi.onUpdaterEvent((event: UpdaterEvent) => {
			this.handleNativeEvent(event);
		});
	}

	private handleNativeEvent(event: UpdaterEvent): void {
		const isBackgroundOrFocusCheck = event.context === 'background' || event.context === 'focus';

		switch (event.type) {
			case 'checking':
				runInAction(() => {
					this._isChecking = true;
				});
				break;

			case 'available':
				runInAction(() => {
					this.updateInfo.native = {
						available: true,
						downloaded: false,
						version: event.version ?? null,
					};
					this.refreshUpdateType();
					this._isChecking = false;
				});
				break;

			case 'not-available': {
				const hadDownloaded = this.updateInfo.native.downloaded;
				runInAction(() => {
					this.lastCheckedAt = Date.now();

					if (!hadDownloaded) {
						this.updateInfo.native = {
							available: false,
							downloaded: false,
							version: null,
						};

						this.refreshUpdateType();
					}

					this._isChecking = false;
				});
				break;
			}

			case 'error':
				if (isBackgroundOrFocusCheck) {
					logger.debug('Background update check failed silently:', event.message);
				} else {
					logger.warn('Update check error:', event.message);
				}
				runInAction(() => {
					this._isChecking = false;
					this.checkInProgress = false;
				});
				break;

			case 'downloaded':
				runInAction(() => {
					this.updateInfo.native = {
						available: true,
						downloaded: true,
						version: event.version ?? null,
					};
					this.refreshUpdateType();
					this._isChecking = false;
				});
				break;

			case 'progress':
				break;
		}
	}

	private startBackgroundChecks(): void {
		if (this.backgroundCheckStarted) return;
		this.backgroundCheckStarted = true;

		window.setInterval(() => {
			if (document.visibilityState === 'visible') {
				void this.checkForUpdates(false);
			}
		}, CHECK_INTERVAL_MS);

		window.addEventListener('focus', () => void this.checkForUpdates(false));
		window.addEventListener('online', () => void this.checkForUpdates(true));

		document.addEventListener('visibilitychange', () => {
			if (document.visibilityState === 'visible') {
				void this.checkForUpdates(false);
			}
		});
	}

	private shouldThrottle(force: boolean): boolean {
		if (force) return false;
		if (this.lastCheckedAt == null) return false;
		return Date.now() - this.lastCheckedAt < MIN_CHECK_INTERVAL_MS;
	}

	private shouldRunNativeCheck(): boolean {
		return this.isNative && !this.updateInfo.native.available;
	}

	async checkForUpdates(force = false): Promise<void> {
		if (this.checkInProgress) return;
		if (this.shouldThrottle(force)) return;

		this.checkInProgress = true;

		runInAction(() => {
			this._isChecking = true;
		});

		try {
			const shouldCheckNative = this.shouldRunNativeCheck();
			const [, webResult] = await Promise.all([
				shouldCheckNative ? this.checkNativeUpdate(force ? 'user' : 'background') : Promise.resolve(null),
				this.checkWebUpdate(),
			]);

			runInAction(() => {
				this.lastCheckedAt = Date.now();

				this.updateInfo.web = {
					available: webResult?.available ?? false,
					sha: webResult?.sha ?? null,
					buildNumber: webResult?.buildNumber ?? null,
				};

				this.refreshUpdateType();
			});
		} catch (err) {
			logger.debug('Update check failed silently:', err);
		} finally {
			runInAction(() => {
				this.lastCheckedAt = Date.now();
				this.checkInProgress = false;
				this._isChecking = false;
			});
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

	private async checkWebUpdate(): Promise<{available: boolean; sha: string | null; buildNumber: number | null}> {
		try {
			const response = await fetch(VERSION_ENDPOINT, {
				cache: 'no-store',
				headers: {'Cache-Control': 'no-cache'},
			});
			if (!response.ok) {
				logger.debug('Version endpoint not available');
				return {available: false, sha: null, buildNumber: null};
			}

			const payload = (await response.json()) as {sha?: string; buildNumber?: number; buildTimestamp?: string};
			const updateAvailable = Boolean(payload.sha && CURRENT_BUILD_SHA && payload.sha !== CURRENT_BUILD_SHA);

			return {
				available: updateAvailable,
				sha: payload.sha ?? null,
				buildNumber: payload.buildNumber ?? null,
			};
		} catch (error) {
			logger.debug('Failed to fetch version info silently:', error);
			return {available: false, sha: null, buildNumber: null};
		}
	}

	async applyUpdate(): Promise<void> {
		if (!this.hasUpdate) return;

		if (this.updateType === 'web') {
			logger.info('Applying web update, reloading...');
			window.location.reload();
			return;
		}

		if (this.isNative && (this.updateType === 'native' || this.updateType === 'both')) {
			const electronApi = getElectronAPI();
			if (electronApi && this.updateInfo.native.downloaded) {
				logger.info('Installing downloaded native update...');
				await electronApi.updaterInstall();
			}
		}
	}

	reset(): void {
		runInAction(() => {
			this.updateType = null;
			this.updateInfo = {
				native: {available: false, downloaded: false, version: null},
				web: {available: false, sha: null, buildNumber: null},
			};
			this.lastCheckedAt = null;
			this._isChecking = false;
			this.checkInProgress = false;
			this.refreshUpdateType();
		});
	}

	dispose(): void {
		if (this.unsubscribeNativeEvents) {
			this.unsubscribeNativeEvents();
			this.unsubscribeNativeEvents = null;
		}
	}
}

export default new UpdaterStoreImpl();
