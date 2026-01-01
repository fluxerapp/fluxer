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

import type {LocalTrackPublication, RemoteParticipant, Room} from 'livekit-client';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import {MicrophonePermissionDeniedModal} from '~/components/alerts/MicrophonePermissionDeniedModal';
import {Logger} from '~/lib/Logger';
import ChannelStore from '~/stores/ChannelStore';
import ConnectionStore from '~/stores/ConnectionStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import MediaPermissionStore from '~/stores/MediaPermissionStore';
import ParticipantVolumeStore from '~/stores/ParticipantVolumeStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import VoiceDevicePermissionStore from '~/stores/voice/VoiceDevicePermissionStore';
import {ensureNativePermission} from '~/utils/NativePermissions';
import {isDesktop} from '~/utils/NativeUtils';
import {SoundType} from '~/utils/SoundUtils';

const logger = new Logger('VoiceStateActionCreators');

export const toggleSelfDeaf = async (_guildId: string | null = null): Promise<void> => {
	const connectedGuildId = MediaEngineStore.guildId;
	const connectedChannelId = MediaEngineStore.channelId;

	const currentDeaf = LocalVoiceStateStore.getSelfDeaf();
	const willUndeafen = currentDeaf;
	const willDeafen = !currentDeaf;

	logger.info('toggleSelfDeaf', {
		currentDeaf,
		willUndeafen,
		willDeafen,
		connectedGuildId,
		connectedChannelId,
		micPermissionState: MediaPermissionStore.getMicrophonePermissionState(),
	});

	if (willUndeafen) {
		const hasMicPermission = MediaPermissionStore.isMicrophoneGranted();

		if (!hasMicPermission) {
			logger.info('Undeafening without mic permission, keeping user muted');
			LocalVoiceStateStore.updateSelfDeaf(false);
			LocalVoiceStateStore.updateSelfMute(true);

			SoundActionCreators.playSound(SoundType.Undeaf);

			MediaEngineStore.syncLocalVoiceStateWithServer({
				self_mute: true,
				self_deaf: false,
			});
			return;
		}
	}

	LocalVoiceStateStore.toggleSelfDeaf();
	const newDeafState = LocalVoiceStateStore.getSelfDeaf();
	const newMuteState = LocalVoiceStateStore.getSelfMute();

	logger.debug('Voice state updated', {newDeafState, newMuteState});

	const room = MediaEngineStore.room;
	if (room?.localParticipant) {
		room.localParticipant.audioTrackPublications.forEach((publication: LocalTrackPublication) => {
			const track = publication.track;
			if (!track) return;
			const operation = newMuteState ? track.mute() : track.unmute();
			operation.catch((error) =>
				logger.error(newMuteState ? 'Failed to mute local track' : 'Failed to unmute local track', {error}),
			);
		});

		room.remoteParticipants.forEach((participant: RemoteParticipant) => {
			ParticipantVolumeStore.applySettingsToParticipant(participant, newDeafState);
		});

		logger.debug('Applied mute/deafen state to LiveKit tracks immediately', {
			newDeafState,
			newMuteState,
			localTrackCount: room.localParticipant.audioTrackPublications.size,
			remoteParticipantCount: room.remoteParticipants.size,
		});
	}

	if (newDeafState) {
		SoundActionCreators.playSound(SoundType.Deaf);
	} else {
		SoundActionCreators.playSound(SoundType.Undeaf);
	}

	MediaEngineStore.syncLocalVoiceStateWithServer({
		self_mute: newMuteState,
		self_deaf: newDeafState,
	});
};

const showMicrophonePermissionDeniedModal = () => {
	ModalActionCreators.push(modal(() => <MicrophonePermissionDeniedModal />));
};

const requestMicrophoneInVoiceChannel = async (room: Room, channelId: string | null): Promise<boolean> => {
	const channel = ChannelStore.getChannel(channelId ?? '');
	const audioBitrate = channel?.bitrate ? channel.bitrate * 1000 : undefined;

	try {
		if (isDesktop()) {
			const nativeResult = await ensureNativePermission('microphone');
			if (nativeResult === 'denied') {
				logger.warn('Microphone permission denied via native API before LiveKit request');
				throw Object.assign(new Error('Native microphone permission denied'), {
					name: 'NotAllowedError',
				});
			}
			if (nativeResult === 'granted') {
				MediaPermissionStore.updateMicrophonePermissionGranted();
			}
		}

		await VoiceDevicePermissionStore.ensureDevices({requestPermissions: false}).catch(() => {});

		let inputDeviceId = VoiceSettingsStore.getInputDeviceId();
		const deviceState = VoiceDevicePermissionStore.getState();

		const deviceExists =
			inputDeviceId === 'default' || deviceState.inputDevices.some((device) => device.deviceId === inputDeviceId);

		if (!deviceExists && deviceState.inputDevices.length > 0) {
			inputDeviceId = 'default';
		}

		const micSettings = {
			deviceId: inputDeviceId,
			echoCancellation: VoiceSettingsStore.getEchoCancellation(),
			noiseSuppression: VoiceSettingsStore.getNoiseSuppression(),
			autoGainControl: VoiceSettingsStore.getAutoGainControl(),
			...(audioBitrate && {audioBitrate}),
		};

		logger.debug('Requesting microphone permission via LiveKit');
		await room.localParticipant.setMicrophoneEnabled(true, micSettings);

		MediaPermissionStore.updateMicrophonePermissionGranted();
		logger.info('Microphone permission granted via LiveKit');
		return true;
	} catch (error) {
		logger.error('Failed to enable microphone', {
			error,
			errorName: error instanceof Error ? error.name : 'unknown',
			errorMessage: error instanceof Error ? error.message : String(error),
		});

		if (error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')) {
			MediaPermissionStore.markMicrophoneExplicitlyDenied();
			showMicrophonePermissionDeniedModal();
		}

		return false;
	}
};

const requestMicrophoneDirectly = async (): Promise<boolean> => {
	try {
		if (isDesktop()) {
			const nativeResult = await ensureNativePermission('microphone');
			if (nativeResult === 'granted') {
				MediaPermissionStore.updateMicrophonePermissionGranted();
				logger.info('Microphone permission granted via native API');
				return true;
			}
			if (nativeResult === 'denied') {
				logger.warn('Microphone permission denied via native API');
				MediaPermissionStore.markMicrophoneExplicitlyDenied();
				showMicrophonePermissionDeniedModal();
				return false;
			}
		}

		logger.debug('Requesting microphone permission via getUserMedia');
		const stream = await navigator.mediaDevices.getUserMedia({audio: true});
		stream.getTracks().forEach((track) => track.stop());

		MediaPermissionStore.updateMicrophonePermissionGranted();
		logger.info('Microphone permission granted via getUserMedia');
		return true;
	} catch (error) {
		logger.error('Failed to get microphone permission', {
			error,
			errorName: error instanceof Error ? error.name : 'unknown',
			errorMessage: error instanceof Error ? error.message : String(error),
		});

		if (error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')) {
			MediaPermissionStore.markMicrophoneExplicitlyDenied();
			showMicrophonePermissionDeniedModal();
		}

		return false;
	}
};

export const toggleSelfMute = async (_guildId: string | null = null): Promise<void> => {
	const room = MediaEngineStore.room;
	const connectedChannelId = MediaEngineStore.channelId;

	const currentMute = LocalVoiceStateStore.getSelfMute();
	const currentDeaf = LocalVoiceStateStore.getSelfDeaf();

	const willUndeafen = currentDeaf;
	const willUnmute = currentMute;
	const willMute = !currentMute && !currentDeaf;
	const willBeUnmuted = willUnmute || willUndeafen;

	logger.info('toggleSelfMute', {
		currentMute,
		currentDeaf,
		willUnmute,
		willUndeafen,
		willMute,
		willBeUnmuted,
		hasRoom: !!room,
		micPermissionState: MediaPermissionStore.getMicrophonePermissionState(),
	});

	if (willBeUnmuted) {
		if (MediaPermissionStore.isMicrophoneExplicitlyDenied()) {
			logger.warn('Microphone permission explicitly denied, cannot unmute');
			showMicrophonePermissionDeniedModal();
			return;
		}

		if (!MediaPermissionStore.isMicrophoneGranted()) {
			logger.info('Microphone permission not granted, requesting permission');

			const permissionGranted = room?.localParticipant
				? await requestMicrophoneInVoiceChannel(room, connectedChannelId)
				: await requestMicrophoneDirectly();

			if (!permissionGranted) {
				logger.warn('Microphone permission request failed, staying muted');
				LocalVoiceStateStore.updateSelfMute(true);
				if (room) {
					MediaEngineStore.syncLocalVoiceStateWithServer({self_mute: true});
				}
				return;
			}

			const currentMuteAfterPermission = LocalVoiceStateStore.getSelfMute();
			if (!currentMuteAfterPermission) {
				logger.debug('Already unmuted after permission grant, skipping toggle');
				SoundActionCreators.playSound(SoundType.Unmute);
				if (room) {
					MediaEngineStore.syncLocalVoiceStateWithServer({
						self_mute: false,
						self_deaf: LocalVoiceStateStore.getSelfDeaf(),
					});
				}
				return;
			}
		}
	}

	LocalVoiceStateStore.toggleSelfMute();
	const newMute = LocalVoiceStateStore.getSelfMute();
	const newDeaf = LocalVoiceStateStore.getSelfDeaf();

	logger.debug('Voice state updated', {newMute, newDeaf});

	if (room?.localParticipant) {
		room.localParticipant.audioTrackPublications.forEach((publication: LocalTrackPublication) => {
			const track = publication.track;
			if (!track) return;
			const operation = newMute ? track.mute() : track.unmute();
			operation.catch((error) =>
				logger.error(newMute ? 'Failed to mute local track' : 'Failed to unmute local track', {error}),
			);
		});

		logger.debug('Applied mute state to LiveKit tracks immediately', {
			newMute,
			newDeaf,
			localTrackCount: room.localParticipant.audioTrackPublications.size,
		});
	}

	if (!newMute) {
		SoundActionCreators.playSound(SoundType.Unmute);
	} else {
		SoundActionCreators.playSound(SoundType.Mute);
	}

	if (room) {
		MediaEngineStore.syncLocalVoiceStateWithServer({
			self_mute: newMute,
			self_deaf: newDeaf,
		});
	}
};

type VoiceStateProperty = 'self_mute' | 'self_deaf' | 'self_video' | 'self_stream';

const updateConnectionProperty = async (
	connectionId: string,
	property: VoiceStateProperty,
	value: boolean,
): Promise<void> => {
	const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
	if (!voiceState) return;

	const socket = ConnectionStore.socket;
	if (!socket) return;

	socket.updateVoiceState({
		guild_id: voiceState.guild_id,
		channel_id: voiceState.channel_id,
		connection_id: connectionId,
		self_mute: property === 'self_mute' ? value : voiceState.self_mute,
		self_deaf: property === 'self_deaf' ? value : voiceState.self_deaf,
		self_video: property === 'self_video' ? value : voiceState.self_video,
		self_stream: property === 'self_stream' ? value : voiceState.self_stream,
	});
};

const updateConnectionsProperty = async (
	connectionIds: Array<string>,
	property: VoiceStateProperty,
	value: boolean,
): Promise<void> => {
	const socket = ConnectionStore.socket;
	if (!socket) return;

	for (const connectionId of connectionIds) {
		const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
		if (!voiceState) continue;

		socket.updateVoiceState({
			guild_id: voiceState.guild_id,
			channel_id: voiceState.channel_id,
			connection_id: connectionId,
			self_mute: property === 'self_mute' ? value : voiceState.self_mute,
			self_deaf: property === 'self_deaf' ? value : voiceState.self_deaf,
			self_video: property === 'self_video' ? value : voiceState.self_video,
			self_stream: property === 'self_stream' ? value : voiceState.self_stream,
		});
	}
};

export const toggleSelfMuteForConnection = async (connectionId: string): Promise<void> => {
	const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
	if (!voiceState) return;
	const target = !voiceState.self_mute;
	await updateConnectionProperty(connectionId, 'self_mute', target);
	if (target) SoundActionCreators.playSound(SoundType.Mute);
	else SoundActionCreators.playSound(SoundType.Unmute);
};

export const toggleSelfDeafenForConnection = async (connectionId: string): Promise<void> => {
	const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
	if (!voiceState) return;
	const target = !voiceState.self_deaf;
	await updateConnectionProperty(connectionId, 'self_deaf', target);
	if (target) SoundActionCreators.playSound(SoundType.Deaf);
	else SoundActionCreators.playSound(SoundType.Undeaf);
};

export const turnOffCameraForConnection = async (connectionId: string): Promise<void> => {
	await updateConnectionProperty(connectionId, 'self_video', false);
};

export const turnOffStreamForConnection = async (connectionId: string): Promise<void> => {
	await updateConnectionProperty(connectionId, 'self_stream', false);
};

export const bulkMuteConnections = async (connectionIds: Array<string>, mute: boolean = true): Promise<void> => {
	await updateConnectionsProperty(connectionIds, 'self_mute', mute);
};

export const bulkDeafenConnections = async (connectionIds: Array<string>, deafen: boolean = true): Promise<void> => {
	await updateConnectionsProperty(connectionIds, 'self_deaf', deafen);
};

export const bulkTurnOffCameras = async (connectionIds: Array<string>): Promise<void> => {
	await updateConnectionsProperty(connectionIds, 'self_video', false);
};

export const bulkDisconnect = async (connectionIds: Array<string>): Promise<void> => {
	const socket = ConnectionStore.socket;
	if (!socket) return;

	for (const connectionId of connectionIds) {
		const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
		if (!voiceState) continue;

		socket.updateVoiceState({
			guild_id: voiceState.guild_id,
			channel_id: null,
			connection_id: connectionId,
			self_mute: true,
			self_deaf: true,
			self_video: false,
			self_stream: false,
		});
	}
};

export const bulkMoveConnections = async (connectionIds: Array<string>, targetChannelId: string): Promise<void> => {
	const socket = ConnectionStore.socket;
	if (!socket) return;

	for (const connectionId of connectionIds) {
		const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
		if (!voiceState) continue;

		socket.updateVoiceState({
			guild_id: voiceState.guild_id,
			channel_id: targetChannelId,
			connection_id: connectionId,
			self_mute: voiceState.self_mute,
			self_deaf: voiceState.self_deaf,
			self_video: voiceState.self_video,
			self_stream: voiceState.self_stream,
		});
	}
};
