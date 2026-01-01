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

import type {LocalAudioTrack, Room, ScreenShareCaptureOptions, TrackPublishOptions} from 'livekit-client';
import {Track, VideoPresets} from 'livekit-client';
import {makeAutoObservable} from 'mobx';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import {CameraPermissionDeniedModal} from '~/components/alerts/CameraPermissionDeniedModal';
import {MicrophonePermissionDeniedModal} from '~/components/alerts/MicrophonePermissionDeniedModal';
import {Logger} from '~/lib/Logger';
import CallMediaPrefsStore from '~/stores/CallMediaPrefsStore';
import ChannelStore from '~/stores/ChannelStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import MediaPermissionStore from '~/stores/MediaPermissionStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import VoiceDevicePermissionStore from '~/stores/voice/VoiceDevicePermissionStore';
import {ensureNativePermission} from '~/utils/NativePermissions';
import {isDesktop} from '~/utils/NativeUtils';
import {SoundType} from '~/utils/SoundUtils';
import {
	applyAllLocalAudioPreferences as applyAllLocalAudioPreferencesFn,
	applyLocalAudioPreferencesForUser as applyLocalAudioPreferencesForUserFn,
	applyPushToTalkHold as applyPushToTalkHoldFn,
	getMuteReason as getMuteReasonFn,
	handlePushToTalkModeChange as handlePushToTalkModeChangeFn,
} from './VoiceAudioManager';
import {playEntranceSound} from './VoiceEntranceSoundManager';
import VoiceScreenShareManager from './VoiceScreenShareManager';
import type {VoiceState} from './VoiceStateManager';

const logger = new Logger('VoiceMediaManager');

export interface SetCameraEnabledOptions {
	deviceId?: string;
	sendUpdate?: boolean;
}

class VoiceMediaManager {
	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get isScreenSharePending(): boolean {
		return VoiceScreenShareManager.getIsScreenSharePending();
	}

	async ensureMicrophone(room: Room, channelId: string): Promise<void> {
		const selfMute = LocalVoiceStateStore.getSelfMute();
		const selfDeaf = LocalVoiceStateStore.getSelfDeaf();
		const denied = MediaPermissionStore.isMicrophoneExplicitlyDenied();

		if (selfMute || selfDeaf) {
			logger.debug('[ensureMicrophone] Skipping: user is muted or deafened', {selfMute, selfDeaf});
			if (selfMute) {
				this.syncVoiceState({self_mute: true});
			}
			return;
		}

		if (denied) {
			logger.debug('[ensureMicrophone] Microphone explicitly denied');
			if (!LocalVoiceStateStore.getSelfMute()) {
				LocalVoiceStateStore.updateSelfMute(true);
			}
			this.syncVoiceState({self_mute: true});
			return;
		}

		if (!room.localParticipant) {
			logger.warn('[ensureMicrophone] No local participant');
			return;
		}

		try {
			await this.enableMicrophone(room, channelId);
			MediaPermissionStore.updateMicrophonePermissionGranted();

			this.syncVoiceState({self_mute: selfMute});
		} catch (e: unknown) {
			if (e instanceof Error && (e.name === 'NotAllowedError' || e.name === 'PermissionDeniedError')) {
				MediaPermissionStore.markMicrophoneExplicitlyDenied();
				ModalActionCreators.push(modal(() => <MicrophonePermissionDeniedModal />));
				if (!LocalVoiceStateStore.getSelfMute()) {
					LocalVoiceStateStore.updateSelfMute(true);
				}
				this.syncVoiceState({self_mute: true});
			}
		}
	}

	async enableMicrophone(room: Room, channelId: string): Promise<void> {
		const channel = ChannelStore.getChannel(channelId);
		const audioBitrate = channel?.bitrate ? channel.bitrate * 1000 : undefined;

		try {
			if (isDesktop()) {
				const nativeResult = await ensureNativePermission('microphone');
				if (nativeResult === 'denied') {
					logger.warn('[enableMicrophone] Native microphone permission denied');
					throw Object.assign(new Error('Native microphone permission denied'), {
						name: 'NotAllowedError',
					});
				}
				if (nativeResult === 'granted') {
					MediaPermissionStore.updateMicrophonePermissionGranted();
				}
			}

			await VoiceDevicePermissionStore.ensureDevices({requestPermissions: false}).catch(() => {});

			if (!room.localParticipant) {
				logger.warn('[enableMicrophone] No local participant');
				return;
			}

			let inputDeviceId = VoiceSettingsStore.getInputDeviceId();
			const deviceState = VoiceDevicePermissionStore.getState();
			const exists = inputDeviceId === 'default' || deviceState.inputDevices.some((d) => d.deviceId === inputDeviceId);

			if (!exists && deviceState.inputDevices.length > 0) {
				inputDeviceId = 'default';
			}

			await room.localParticipant.setMicrophoneEnabled(true, {
				deviceId: inputDeviceId,
				echoCancellation: VoiceSettingsStore.getEchoCancellation(),
				noiseSuppression: VoiceSettingsStore.getNoiseSuppression(),
				autoGainControl: VoiceSettingsStore.getAutoGainControl(),
				...(audioBitrate && {audioBitrate}),
			});

			MediaPermissionStore.updateMicrophonePermissionGranted();
			logger.info('[enableMicrophone] Successfully enabled microphone');
		} catch (e: unknown) {
			if (e instanceof Error && (e.name === 'NotAllowedError' || e.name === 'PermissionDeniedError')) {
				logger.error('[enableMicrophone] Permission denied');
				MediaPermissionStore.markMicrophoneExplicitlyDenied();
				ModalActionCreators.push(modal(() => <MicrophonePermissionDeniedModal />));
			} else {
				logger.error('[enableMicrophone] Failed', e);
			}
			throw e;
		}
	}

	async disableMicrophone(room: Room): Promise<void> {
		if (!room.localParticipant) {
			logger.warn('[disableMicrophone] No local participant');
			return;
		}

		try {
			const participant = room.localParticipant;
			const audioPublications = Array.from(participant.audioTrackPublications.values());

			if (audioPublications.length > 0) {
				const tracks = audioPublications
					.map((pub) => pub.track)
					.filter((track): track is LocalAudioTrack => Boolean(track));

				await Promise.allSettled(tracks.map((track) => participant.unpublishTrack(track)));
				logger.info('[disableMicrophone] Successfully disabled microphone');
			}
		} catch (e) {
			logger.error('[disableMicrophone] Failed', e);
		}
	}

	async setMicrophoneEnabled(enabled: boolean, room: Room, channelId: string): Promise<void> {
		if (enabled) {
			await this.enableMicrophone(room, channelId);
		} else {
			await this.disableMicrophone(room);
		}
	}

	async setCameraEnabled(enabled: boolean, options?: SetCameraEnabledOptions): Promise<void> {
		const room = this.getRoomFromMediaEngineStore();
		const {sendUpdate = true, ...restOptions} = options || {};

		if (!room?.localParticipant) {
			logger.warn('[setCameraEnabled] No room or local participant');
			return;
		}

		if (enabled) {
			const nativeResult = await ensureNativePermission('camera');
			if (nativeResult === 'denied') {
				MediaPermissionStore.markCameraExplicitlyDenied();
				ModalActionCreators.push(modal(() => <CameraPermissionDeniedModal />));
				return;
			}
			if (nativeResult === 'granted') {
				MediaPermissionStore.updateCameraPermissionGranted();
			}
		}

		try {
			const participant = room.localParticipant;
			const resolution = VoiceSettingsStore.getCameraResolution();
			const videoResolution = this.getVideoPreset(resolution);

			await participant.setCameraEnabled(enabled, {resolution: videoResolution, ...restOptions});

			LocalVoiceStateStore.updateSelfVideo(enabled);
			if (sendUpdate) this.syncVoiceState({self_video: enabled});

			this.updateLocalParticipant();

			if (enabled) {
				SoundActionCreators.playSound(SoundType.CameraOn);
			} else {
				SoundActionCreators.playSound(SoundType.CameraOff);
			}
			logger.info('[setCameraEnabled] Success', {enabled});
		} catch (e) {
			logger.error('[setCameraEnabled] Failed', {enabled, error: e});
			const actual = room.localParticipant?.isCameraEnabled ?? false;
			LocalVoiceStateStore.updateSelfVideo(actual);
			if (sendUpdate) this.syncVoiceState({self_video: actual});

			if (actual) {
				SoundActionCreators.playSound(SoundType.CameraOn);
			} else {
				SoundActionCreators.playSound(SoundType.CameraOff);
			}
		}
	}

	private getVideoPreset(resolution: string) {
		switch (resolution) {
			case 'high':
				return VideoPresets.h1080;
			case 'medium':
				return VideoPresets.h720;
			default:
				return VideoPresets.h360;
		}
	}

	async toggleCameraFromKeybind(): Promise<void> {
		const current = LocalVoiceStateStore.getSelfVideo();
		await this.setCameraEnabled(!current, {deviceId: VoiceSettingsStore.getVideoDeviceId()});
	}

	async setScreenShareEnabled(
		enabled: boolean,
		options?: ScreenShareCaptureOptions & {sendUpdate?: boolean},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		const room = this.getRoomFromMediaEngineStore();
		await VoiceScreenShareManager.setScreenShareEnabled(
			room,
			enabled,
			{syncVoiceState: this.syncVoiceState, updateLocalParticipant: this.updateLocalParticipant},
			options,
			publishOptions,
		);
	}

	async toggleScreenShareFromKeybind(): Promise<void> {
		const room = this.getRoomFromMediaEngineStore();
		await VoiceScreenShareManager.toggleScreenShareFromKeybind(room, {
			syncVoiceState: this.syncVoiceState,
			updateLocalParticipant: this.updateLocalParticipant,
		});
	}

	async playEntranceSound(): Promise<void> {
		const room = this.getRoomFromMediaEngineStore();
		await playEntranceSound(room);
	}

	resetStreamTracking(): void {
		VoiceScreenShareManager.resetStreamTracking();
	}

	syncVoiceState(partial: {
		self_video?: boolean;
		self_stream?: boolean;
		self_mute?: boolean;
		self_deaf?: boolean;
		viewer_stream_key?: string | null;
	}): void {
		try {
			if ('_mediaEngineStore' in window) {
				const store = (window as {_mediaEngineStore?: {syncLocalVoiceStateWithServer?: (p: typeof partial) => void}})
					._mediaEngineStore;
				if (store?.syncLocalVoiceStateWithServer) {
					store.syncLocalVoiceStateWithServer(partial);
				}
			}
		} catch (e) {
			logger.error('[syncVoiceState] Failed to sync voice state with server', e);
		}
	}

	private getRoomFromMediaEngineStore(): Room | null {
		try {
			if ('_mediaEngineStore' in window) {
				const store = (window as {_mediaEngineStore?: {room?: Room}})._mediaEngineStore;
				return store?.room ?? null;
			}
		} catch (e) {
			logger.error('[getRoomFromMediaEngineStore] Failed to get room', e);
		}
		return null;
	}

	private updateLocalParticipant(): void {
		try {
			const room = this.getRoomFromMediaEngineStore();
			if (room?.localParticipant && '_mediaEngineStore' in window) {
				const store = (window as {_mediaEngineStore?: {updateLocalParticipant?: () => void}})._mediaEngineStore;
				if (store && 'upsertParticipant' in store) {
					(store as {upsertParticipant?: (p: unknown) => void}).upsertParticipant?.(room.localParticipant);
				}
			}
		} catch (e) {
			logger.error('[updateLocalParticipant] Failed to update local participant', e);
		}
	}

	applyLocalAudioPreferencesForUser(userId: string, room: Room | null): void {
		applyLocalAudioPreferencesForUserFn(userId, room);
	}

	applyAllLocalAudioPreferences(room: Room | null): void {
		applyAllLocalAudioPreferencesFn(room);
	}

	setLocalVideoDisabled(identity: string, disabled: boolean, room: Room | null, connectionId: string | null): void {
		if (!connectionId) {
			logger.warn('[setLocalVideoDisabled] No connection ID');
			return;
		}
		CallMediaPrefsStore.setVideoDisabled(connectionId, identity, disabled);
		if (!room) return;
		const p = room.remoteParticipants.get(identity);
		if (!p) return;

		p.videoTrackPublications.forEach((pub) => {
			if (pub.source === Track.Source.Camera || pub.source === Track.Source.ScreenShare) {
				try {
					if (disabled) {
						pub.setSubscribed(false);
						logger.debug('[setLocalVideoDisabled] Unsubscribed from track', {
							identity,
							source: pub.source,
							trackSid: pub.trackSid,
						});
					} else {
						pub.setSubscribed(true);
						logger.debug('[setLocalVideoDisabled] Re-subscribed to track', {
							identity,
							source: pub.source,
							trackSid: pub.trackSid,
						});
					}
				} catch (err) {
					logger.error('[setLocalVideoDisabled] Failed to update subscription', {
						error: err,
						identity,
						source: pub.source,
						disabled,
					});
				}
			}
		});
	}

	applyPushToTalkHold(held: boolean, room: Room | null, getCurrentUserVoiceState: () => VoiceState | null): void {
		applyPushToTalkHoldFn(held, room, getCurrentUserVoiceState, this.syncVoiceState);
	}

	handlePushToTalkModeChange(room: Room | null, getCurrentUserVoiceState: () => VoiceState | null): void {
		handlePushToTalkModeChangeFn(room, getCurrentUserVoiceState, this.syncVoiceState);
	}

	getMuteReason(voiceState: VoiceState | null): 'guild' | 'push_to_talk' | 'self' | null {
		return getMuteReasonFn(voiceState);
	}
}

export default new VoiceMediaManager();
