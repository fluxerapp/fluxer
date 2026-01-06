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
import {Logger} from '~/lib/Logger';
import {makePersistent} from '~/lib/MobXPersistence';
import MediaPermissionStore from '~/stores/MediaPermissionStore';
import VoiceDevicePermissionStore, {type VoiceDeviceState} from '~/stores/voice/VoiceDevicePermissionStore';

const logger = new Logger('LocalVoiceStateStore');

class LocalVoiceStateStore {
	selfMute = !MediaPermissionStore.isMicrophoneGranted();
	selfDeaf = false;
	selfVideo = false;
	selfStream = false;
	selfStreamAudio = false;
	selfStreamAudioMute = false;
	noiseSuppressionEnabled = true;
	viewerStreamKey: string | null = null;

	hasUserSetMute = false;
	hasUserSetDeaf = false;

	private shouldUnmuteOnUndeafen = false;

	private microphonePermissionGranted: boolean | null = MediaPermissionStore.isMicrophoneGranted();
	private mutedByPermission = !MediaPermissionStore.isMicrophoneGranted();
	private persistenceHydrationPromise: Promise<void>;
	private _disposers: Array<() => void> = [];
	private lastDevicePermissionStatus: VoiceDeviceState['permissionStatus'] | null =
		VoiceDevicePermissionStore.getState().permissionStatus;
	private isNotifyingServerOfPermissionMute = false;

	constructor() {
		makeAutoObservable<
			this,
			| 'microphonePermissionGranted'
			| 'mutedByPermission'
			| '_disposers'
			| 'isNotifyingServerOfPermissionMute'
			| 'shouldUnmuteOnUndeafen'
		>(
			this,
			{
				microphonePermissionGranted: false,
				mutedByPermission: false,
				_disposers: false,
				isNotifyingServerOfPermissionMute: false,
				shouldUnmuteOnUndeafen: false,
			},
			{autoBind: true},
		);
		this._disposers = [];
		this.persistenceHydrationPromise = this.initPersistence();
		this.initializePermissionSync();
		this.initializeDevicePermissionSync();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'LocalVoiceStateStore', [
			'selfMute',
			'selfDeaf',
			'noiseSuppressionEnabled',
			'hasUserSetMute',
			'hasUserSetDeaf',
		]);
		logger.debug('LocalVoiceStateStore hydrated from localStorage on reload');
	}

	dispose(): void {
		this._disposers.forEach((disposer) => disposer());
		this._disposers = [];
	}

	private async initializePermissionSync(): Promise<void> {
		try {
			let defaultMuteInitialized = false;
			await this.persistenceHydrationPromise;

			const syncWithPermission = (source: 'init' | 'change') => {
				if (!MediaPermissionStore.isInitialized()) {
					return;
				}

				const isMicGranted = MediaPermissionStore.isMicrophoneGranted();
				const permissionState = MediaPermissionStore.getMicrophonePermissionState();

				this.microphonePermissionGranted = isMicGranted;

				logger.debug(source === 'init' ? 'Checking microphone permission for sync' : 'Microphone permission changed', {
					isMicGranted,
					permissionState,
					currentMute: this.selfMute,
					hasUserSetMute: this.hasUserSetMute,
					mutedByPermission: this.mutedByPermission,
				});

				if (!isMicGranted) {
					this.applyPermissionMute();
					return;
				}

				const shouldAutoUnmute = this.mutedByPermission && this.selfMute && !this.hasUserSetMute;
				const shouldApplyDefaultUnmute = !defaultMuteInitialized && !this.hasUserSetMute && this.selfMute;

				if (shouldAutoUnmute || shouldApplyDefaultUnmute) {
					logger.info(
						shouldAutoUnmute
							? 'Microphone permission granted, auto-unmuting after forced mute'
							: 'Microphone permission granted, defaulting to unmuted state',
						{permissionState},
					);
					runInAction(() => {
						this.selfMute = false;
					});
				}

				this.mutedByPermission = false;
				defaultMuteInitialized = true;
			};

			syncWithPermission('init');

			const disposer = MediaPermissionStore.addChangeListener(() => {
				syncWithPermission('change');
			});

			this._disposers.push(disposer);
		} catch (err) {
			logger.error('Failed to initialize permission sync', err);
		}
	}

	private initializeDevicePermissionSync(): void {
		const disposer = VoiceDevicePermissionStore.subscribe((state) => {
			this.handleDevicePermissionStatus(state.permissionStatus);
		});
		this._disposers.push(disposer);
	}

	private handleDevicePermissionStatus(status: VoiceDeviceState['permissionStatus']): void {
		if (status === this.lastDevicePermissionStatus) {
			return;
		}

		this.lastDevicePermissionStatus = status;
		if (status === 'granted') {
			void this.applyPermissionGrant();
		} else if (status === 'denied') {
			this.applyPermissionMute();
		}
	}

	private enforcePermissionMuteIfNeeded(): void {
		const devicePermission = VoiceDevicePermissionStore.getState().permissionStatus;
		const granted = MediaPermissionStore.isMicrophoneGranted() || devicePermission === 'granted';
		if (granted) {
			this.microphonePermissionGranted = true;
			return;
		}

		this.microphonePermissionGranted = false;
		this.applyPermissionMute();
	}

	private applyPermissionMute(): void {
		const shouldNotify = !this.isNotifyingServerOfPermissionMute;

		runInAction(() => {
			this.microphonePermissionGranted = false;
			this.mutedByPermission = true;
			if (!this.selfMute) {
				this.selfMute = true;
			}
		});

		if (shouldNotify) {
			void this.notifyServerOfPermissionMute();
		}
	}

	private async applyPermissionGrant(): Promise<void> {
		await this.persistenceHydrationPromise;
		runInAction(() => {
			this.microphonePermissionGranted = true;
			if (this.mutedByPermission && this.selfMute && !this.hasUserSetMute) {
				this.selfMute = false;
			}
			this.mutedByPermission = false;
		});
	}

	private notifyServerOfPermissionMute(): void {
		if (this.isNotifyingServerOfPermissionMute) {
			logger.debug('Skipping recursive notifyServerOfPermissionMute call');
			return;
		}

		try {
			this.isNotifyingServerOfPermissionMute = true;
			const store = (
				window as {_mediaEngineStore?: {syncLocalVoiceStateWithServer?: (p: {self_mute: boolean}) => void}}
			)._mediaEngineStore;
			if (store?.syncLocalVoiceStateWithServer) {
				store.syncLocalVoiceStateWithServer({self_mute: true});
			}
		} catch (error) {
			logger.debug('Failed to sync permission-mute to server', {error});
		} finally {
			this.isNotifyingServerOfPermissionMute = false;
		}
	}

	getSelfMute(): boolean {
		return this.selfMute;
	}

	ensurePermissionMute(): void {
		this.enforcePermissionMuteIfNeeded();
	}

	getSelfDeaf(): boolean {
		return this.selfDeaf;
	}

	getSelfVideo(): boolean {
		return this.selfVideo;
	}

	getSelfStream(): boolean {
		return this.selfStream;
	}

	getSelfStreamAudio(): boolean {
		return this.selfStreamAudio;
	}

	getSelfStreamAudioMute(): boolean {
		return this.selfStreamAudioMute;
	}

	getViewerStreamKey(): string | null {
		return this.viewerStreamKey;
	}

	updateViewerStreamKey(value: string | null): void {
		runInAction(() => {
			this.viewerStreamKey = value;
		});
	}

	getNoiseSuppressionEnabled(): boolean {
		return this.noiseSuppressionEnabled;
	}

	getHasUserSetMute(): boolean {
		return this.hasUserSetMute;
	}

	getHasUserSetDeaf(): boolean {
		return this.hasUserSetDeaf;
	}

	toggleSelfMute(): void {
		runInAction(() => {
			const newSelfMute = !this.selfMute;
			const micDenied = this.microphonePermissionGranted === false;

			if (this.selfDeaf && !newSelfMute) {
				this.hasUserSetMute = true;
				this.hasUserSetDeaf = true;
				this.shouldUnmuteOnUndeafen = false;

				if (micDenied) {
					this.mutedByPermission = true;
					this.selfDeaf = false;
					logger.debug('Mic denied: user attempted unmute while deaf; undeafening only');
					return;
				}

				this.selfMute = false;
				this.selfDeaf = false;
				logger.debug('User unmuted while deafened; also undeafened');
				return;
			}

			if (micDenied && !newSelfMute) {
				this.hasUserSetMute = true;
				this.mutedByPermission = true;
				logger.debug('Microphone permission denied, keeping self mute enabled despite toggle');
				return;
			}

			this.hasUserSetMute = true;
			this.selfMute = newSelfMute;

			if (!this.selfDeaf) {
				this.shouldUnmuteOnUndeafen = false;
			}

			logger.debug('User toggled self mute', {newSelfMute, hasUserSetMute: true});
		});
	}

	toggleSelfDeaf(): void {
		runInAction(() => {
			const newSelfDeaf = !this.selfDeaf;
			this.hasUserSetDeaf = true;
			const micDenied = this.microphonePermissionGranted === false;

			if (newSelfDeaf) {
				const wasMutedBefore = this.selfMute || micDenied;

				this.selfDeaf = true;
				this.selfMute = true;
				this.shouldUnmuteOnUndeafen = !wasMutedBefore;
			} else {
				this.selfDeaf = false;

				if (this.shouldUnmuteOnUndeafen && !micDenied) {
					this.selfMute = false;
				}
				this.shouldUnmuteOnUndeafen = false;
			}

			logger.debug('User toggled self deaf', {newSelfDeaf, hasUserSetDeaf: true});
		});
	}

	toggleSelfVideo(): void {
		runInAction(() => {
			this.selfVideo = !this.selfVideo;
			logger.debug('User toggled self video', {selfVideo: this.selfVideo});
		});
	}

	toggleSelfStream(): void {
		runInAction(() => {
			this.selfStream = !this.selfStream;
			logger.debug('User toggled self stream', {selfStream: this.selfStream});
		});
	}

	toggleSelfStreamAudio(): void {
		runInAction(() => {
			this.selfStreamAudio = !this.selfStreamAudio;
			logger.debug('User toggled self stream audio', {selfStreamAudio: this.selfStreamAudio});
		});
	}

	toggleSelfStreamAudioMute(): void {
		runInAction(() => {
			this.selfStreamAudioMute = !this.selfStreamAudioMute;
			logger.debug('User toggled self stream audio mute', {selfStreamAudioMute: this.selfStreamAudioMute});
		});
	}

	toggleNoiseSuppression(): void {
		runInAction(() => {
			this.noiseSuppressionEnabled = !this.noiseSuppressionEnabled;
			logger.debug('User toggled noise suppression', {enabled: this.noiseSuppressionEnabled});
		});
	}

	updateSelfMute(muted: boolean): void {
		runInAction(() => {
			if (this.microphonePermissionGranted === false && !muted) {
				this.mutedByPermission = true;
				if (!this.selfMute) {
					this.selfMute = true;
					logger.debug('Microphone permission denied, overriding requested unmute');
				}
				return;
			}

			this.selfMute = muted;
			logger.debug('Self mute updated', {muted});
		});
	}

	updateSelfDeaf(deafened: boolean): void {
		runInAction(() => {
			this.selfDeaf = deafened;
			if (!deafened) {
				this.shouldUnmuteOnUndeafen = false;
			}
			logger.debug('Self deaf updated', {deafened});
		});
	}

	updateSelfVideo(video: boolean): void {
		runInAction(() => {
			this.selfVideo = video;
			logger.debug('Self video updated', {video});
		});
	}

	updateSelfStream(streaming: boolean): void {
		runInAction(() => {
			this.selfStream = streaming;
			logger.debug('Self stream updated', {streaming});
		});
	}

	updateSelfStreamAudio(enabled: boolean): void {
		runInAction(() => {
			this.selfStreamAudio = enabled;
			logger.debug('Self stream audio updated', {enabled});
		});
	}

	updateSelfStreamAudioMute(muted: boolean): void {
		runInAction(() => {
			this.selfStreamAudioMute = muted;
			logger.debug('Self stream audio mute updated', {muted});
		});
	}

	resetUserPreferences(): void {
		runInAction(() => {
			this.hasUserSetMute = false;
			this.hasUserSetDeaf = false;
			this.selfMute = false;
			this.selfDeaf = false;
			this.selfVideo = false;
			this.selfStream = false;
			this.selfStreamAudio = false;
			this.selfStreamAudioMute = false;
			this.noiseSuppressionEnabled = true;
			this.mutedByPermission = false;
			this.shouldUnmuteOnUndeafen = false;
		});
		if (this.microphonePermissionGranted === false) {
			logger.debug('Resetting preferences while microphone permission denied, keeping user muted');
			runInAction(() => {
				this.selfMute = true;
				this.mutedByPermission = true;
			});
		}
		logger.info('Reset user voice preferences');
	}
}

export default new LocalVoiceStateStore();
