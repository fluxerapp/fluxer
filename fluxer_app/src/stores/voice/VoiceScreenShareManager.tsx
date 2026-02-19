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

import * as SoundActionCreators from '@app/actions/SoundActionCreators';
import {getStreamKey} from '@app/components/voice/StreamKeys';
import {Logger} from '@app/lib/Logger';
import {Platform} from '@app/lib/Platform';
import LocalVoiceStateStore from '@app/stores/LocalVoiceStateStore';
import MediaPermissionStore from '@app/stores/MediaPermissionStore';
import VoiceSettingsStore from '@app/stores/VoiceSettingsStore';
import VoiceConnectionManager from '@app/stores/voice/VoiceConnectionManager';
import {syncLocalVoiceStateWithServer, updateLocalParticipantFromRoom} from '@app/stores/voice/VoiceMediaEngineBridge';
import VoiceMediaStateCoordinator from '@app/stores/voice/VoiceMediaStateCoordinator';
import {ScreenRecordingPermissionDeniedError} from '@app/utils/errors/ScreenRecordingPermissionDeniedError';
import {ensureNativePermission} from '@app/utils/NativePermissions';
import {isDesktop, isNativeMacOS} from '@app/utils/NativeUtils';
import {SoundType} from '@app/utils/SoundUtils';
import {
	type Room,
	type ScreenShareCaptureOptions,
	Track,
	type TrackPublishOptions,
	type VideoCodec,
} from 'livekit-client';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('VoiceScreenShareManager');

function getPreferredScreenShareCodec(): VideoCodec {
	if (VoiceSettingsStore.getScreenShareHardwareAcceleration()) {
		return 'h265';
	}

	return 'vp9';
}

class VoiceScreenShareManager {
	isScreenSharePending = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getIsScreenSharePending(): boolean {
		return this.isScreenSharePending;
	}

	private getLocalStreamKey(): string {
		const {guildId, channelId, connectionId} = VoiceConnectionManager.connectionState;
		if (!connectionId) {
			const error = new Error('Cannot sync screen share watcher without an active voice connection');
			logger.error('Missing connection id when deriving local stream key', {
				connectionState: VoiceConnectionManager.connectionState,
			});
			throw error;
		}
		return getStreamKey(guildId, channelId, connectionId);
	}

	private syncLocalStreamWatchState(enabled: boolean): void {
		const streamKey = this.getLocalStreamKey();
		const current = LocalVoiceStateStore.getViewerStreamKeys();
		if (enabled) {
			if (current.includes(streamKey)) return;
			const updated = [...current, streamKey];
			LocalVoiceStateStore.updateViewerStreamKeys(updated);
			syncLocalVoiceStateWithServer({viewer_stream_keys: updated});
			return;
		}

		if (!current.includes(streamKey)) {
			logger.error('Viewer stream key not found in array while disabling screen share', {
				current,
				expected: streamKey,
			});
			const updated = current.filter((k) => k !== streamKey);
			LocalVoiceStateStore.updateViewerStreamKeys(updated);
			syncLocalVoiceStateWithServer({viewer_stream_keys: updated});
			return;
		}
		const updated = current.filter((k) => k !== streamKey);
		LocalVoiceStateStore.updateViewerStreamKeys(updated);
		syncLocalVoiceStateWithServer({viewer_stream_keys: updated});
	}

	async setScreenShareEnabled(
		room: Room | null,
		enabled: boolean,
		options?: ScreenShareCaptureOptions & {
			sendUpdate?: boolean;
			playSound?: boolean;
			restartIfEnabled?: boolean;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		if (Platform.OS !== 'web') {
			logger.warn('Screen share not supported on native');
			return;
		}

		const {sendUpdate = true, playSound = true, restartIfEnabled = false, ...restOptions} = options || {};
		const participant = room?.localParticipant;

		if (!participant) {
			logger.warn('No participant');
			return;
		}

		if (this.isScreenSharePending) {
			logger.debug('Already pending, ignoring request');
			return;
		}

		if (enabled && restartIfEnabled && participant.isScreenShareEnabled) {
			await this.setScreenShareEnabled(room, false, {
				sendUpdate: false,
				playSound: false,
			});
			await this.setScreenShareEnabled(
				room,
				true,
				{
					...restOptions,
					sendUpdate,
					playSound,
				},
				publishOptions,
			);
			return;
		}

		if (enabled && isDesktop() && isNativeMacOS()) {
			const denied = MediaPermissionStore.isScreenRecordingExplicitlyDenied();
			if (denied) {
				logger.warn('Screen recording permission explicitly denied');
				throw new ScreenRecordingPermissionDeniedError();
			}

			const nativeResult = await ensureNativePermission('screen');
			if (nativeResult === 'denied') {
				logger.warn('Screen recording permission denied');
				MediaPermissionStore.markScreenRecordingExplicitlyDenied();
				throw new ScreenRecordingPermissionDeniedError();
			}

			if (nativeResult === 'granted') {
				MediaPermissionStore.updateScreenRecordingPermissionGranted();
			}
		}

		const stateReason = sendUpdate ? 'user' : 'server';
		const applyState = (value: boolean) => {
			VoiceMediaStateCoordinator.applyScreenShareState(value, {reason: stateReason, sendUpdate});
		};

		if (!enabled) applyState(false);

		runInAction(() => {
			this.isScreenSharePending = true;
		});

		const preferredVideoCodec = publishOptions?.videoCodec ?? getPreferredScreenShareCodec();
		const effectivePublishOptions = enabled
			? {
					...publishOptions,
					videoCodec: preferredVideoCodec,
				}
			: publishOptions;

		try {
			await participant.setScreenShareEnabled(enabled, restOptions, effectivePublishOptions);
			if (enabled) applyState(true);

			updateLocalParticipantFromRoom(room);
			this.syncLocalStreamWatchState(enabled);

			if (playSound) {
				if (enabled) {
					SoundActionCreators.playSound(SoundType.ScreenShareStart);
				} else {
					SoundActionCreators.playSound(SoundType.ScreenShareStop);
				}
			}
			logger.info('Success', {enabled});
		} catch (e) {
			if (e instanceof Error && (e.name === 'AbortError' || e.name === 'NotAllowedError')) {
				logger.debug('User cancelled or permission denied', {name: e.name});
				const actual = participant.isScreenShareEnabled;
				applyState(actual);
				updateLocalParticipantFromRoom(room);
				this.syncLocalStreamWatchState(actual);
				return;
			}

			logger.error('Failed', {enabled, error: e});
			const actual = participant.isScreenShareEnabled;
			applyState(actual);
			updateLocalParticipantFromRoom(room);
			this.syncLocalStreamWatchState(actual);

			if (playSound) {
				if (actual) {
					SoundActionCreators.playSound(SoundType.ScreenShareStart);
				} else {
					SoundActionCreators.playSound(SoundType.ScreenShareStop);
				}
			}
		} finally {
			runInAction(() => {
				this.isScreenSharePending = false;
			});
		}
	}

	async updateActiveScreenShareSettings(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		if (Platform.OS !== 'web') {
			logger.warn('Screen share updates are not supported on native');
			return false;
		}

		const participant = room?.localParticipant;
		if (!participant || !participant.isScreenShareEnabled) {
			return false;
		}

		const screenSharePublication = participant.getTrackPublication(Track.Source.ScreenShare);
		const screenShareTrack = screenSharePublication?.videoTrack;
		if (!screenShareTrack) {
			logger.warn('No active screen share track to update');
			return false;
		}

		let appliedAnySetting = false;

		const resolution = options?.resolution;
		if (resolution) {
			const nextConstraints: MediaTrackConstraints = {};
			if (resolution.width > 0) {
				nextConstraints.width = {ideal: resolution.width};
			}
			if (resolution.height > 0) {
				nextConstraints.height = {ideal: resolution.height};
			}
			if (resolution.frameRate !== undefined) {
				nextConstraints.frameRate = resolution.frameRate;
			}

			try {
				await screenShareTrack.mediaStreamTrack.applyConstraints(nextConstraints);
				appliedAnySetting = true;
			} catch (error) {
				logger.warn('Failed to update active screen share constraints', {
					error,
					resolution,
				});
			}
		}

		if (options?.contentHint) {
			screenShareTrack.mediaStreamTrack.contentHint = options.contentHint;
			appliedAnySetting = true;
		}

		const screenShareEncoding = publishOptions?.screenShareEncoding;
		if (screenShareEncoding) {
			const sender = screenShareTrack.sender;
			if (!sender) {
				logger.warn('No sender found for active screen share track');
			} else {
				const senderParameters = sender.getParameters();
				if (!senderParameters.encodings || senderParameters.encodings.length === 0) {
					logger.warn('No sender encodings available for active screen share track');
				} else {
					senderParameters.encodings = senderParameters.encodings.map((encoding) => ({
						...encoding,
						maxBitrate: screenShareEncoding.maxBitrate,
						maxFramerate: screenShareEncoding.maxFramerate ?? encoding.maxFramerate,
						priority: screenShareEncoding.priority ?? encoding.priority,
					}));
					try {
						await sender.setParameters(senderParameters);
						appliedAnySetting = true;
					} catch (error) {
						logger.warn('Failed to update active screen share sender parameters', {
							error,
							screenShareEncoding,
						});
					}
				}
			}
		}

		if (typeof options?.audio === 'boolean') {
			const screenShareAudioPublication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
			if (screenShareAudioPublication) {
				try {
					if (options.audio) {
						await screenShareAudioPublication.unmute();
					} else {
						await screenShareAudioPublication.mute();
					}
					appliedAnySetting = true;
				} catch (error) {
					logger.warn('Failed to update active screen share audio state', {
						error,
						includeAudio: options.audio,
					});
				}
			} else if (options.audio) {
				logger.info('Cannot enable screen share audio without restarting screen share');
			}
		}

		updateLocalParticipantFromRoom(room);
		return appliedAnySetting;
	}

	async toggleScreenShareFromKeybind(room: Room | null): Promise<void> {
		const current = LocalVoiceStateStore.getSelfStream();
		await this.setScreenShareEnabled(room, !current);
	}

	resetStreamTracking(): void {
		runInAction(() => {
			this.isScreenSharePending = false;
		});
	}
}

export default new VoiceScreenShareManager();
