// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import Keybind from '@app/features/input/state/InputKeybind';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {handleMediaPermissionBlocked} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {ensureNativePermission} from '@app/features/permissions/system/utils/NativePermissions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {isVoicePermissionMuteActive} from '@app/features/voice/utils/VoicePermissionUtils';
import type {Room} from 'livekit-client';

const logger = new Logger('VoiceStateCommands');

export async function toggleSelfDeaf(_guildId: string | null = null): Promise<void> {
	const connectedGuildId = MediaEngine.guildId;
	const connectedChannelId = MediaEngine.channelId;
	const currentDeaf = LocalVoiceState.getSelfDeaf();
	const willUndeafen = currentDeaf;
	const willDeafen = !currentDeaf;
	logger.info('toggleSelfDeaf', {
		currentDeaf,
		willUndeafen,
		willDeafen,
		connectedGuildId,
		connectedChannelId,
		micPermissionState: MediaPermission.getMicrophonePermissionState(),
	});
	if (willUndeafen) {
		const hasMicPermission = MediaPermission.isMicrophoneGranted();
		if (!hasMicPermission) {
			logger.info('Undeafening without mic permission, keeping user muted');
			LocalVoiceState.updateSelfDeaf(false);
			LocalVoiceState.updateSelfMute(true);
			SoundCommands.playSound(SoundType.Undeaf);
			return;
		}
	}
	LocalVoiceState.toggleSelfDeaf();
	const newDeafState = LocalVoiceState.getSelfDeaf();
	const newMuteState = LocalVoiceState.getSelfMute();
	logger.debug('Voice state updated', {newDeafState, newMuteState});
	if (newDeafState) {
		SoundCommands.playSound(SoundType.Deaf);
	} else {
		SoundCommands.playSound(SoundType.Undeaf);
	}
}

const handleMicrophonePermissionBlocked = () => handleMediaPermissionBlocked('microphone');
const requestMicrophoneInVoiceChannel = async (room: Room, channelId: string | null): Promise<boolean> => {
	if (
		isVoicePermissionMuteActive(
			MediaEngine.getCurrentUserVoiceState(MediaEngine.guildId),
			MediaEngine.guildId,
			channelId,
		)
	) {
		logger.info('Skipping microphone request while voice permissions mute the user');
		return false;
	}
	try {
		logger.debug('Requesting microphone permission via MediaEngineFacade');
		await MediaEngine.enableMicrophone(room, channelId);
		MediaPermission.updateMicrophonePermissionGranted();
		logger.info('Microphone permission granted via MediaEngineFacade');
		return true;
	} catch (error) {
		logger.error('Failed to enable microphone', {
			error,
			errorName: error instanceof Error ? error.name : 'unknown',
			errorMessage: error instanceof Error ? error.message : String(error),
		});
		if (error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')) {
			MediaPermission.markMicrophoneExplicitlyDenied();
			handleMicrophonePermissionBlocked();
		}
		return false;
	}
};
const requestMicrophoneDirectly = async (): Promise<boolean> => {
	try {
		if (isDesktop()) {
			const nativeResult = await ensureNativePermission('microphone');
			if (nativeResult === 'granted') {
				MediaPermission.updateMicrophonePermissionGranted();
				logger.info('Microphone permission granted via native API');
				return true;
			}
			if (nativeResult === 'denied') {
				logger.warn('Microphone permission denied via native API');
				MediaPermission.markMicrophoneExplicitlyDenied();
				handleMicrophonePermissionBlocked();
				return false;
			}
		}
		logger.debug('Requesting microphone permission via getUserMedia');
		const stream = await navigator.mediaDevices.getUserMedia({audio: true});
		stream.getTracks().forEach((track) => track.stop());
		MediaPermission.updateMicrophonePermissionGranted();
		logger.info('Microphone permission granted via getUserMedia');
		return true;
	} catch (error) {
		logger.error('Failed to get microphone permission', {
			error,
			errorName: error instanceof Error ? error.name : 'unknown',
			errorMessage: error instanceof Error ? error.message : String(error),
		});
		if (error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')) {
			MediaPermission.markMicrophoneExplicitlyDenied();
			handleMicrophonePermissionBlocked();
		}
		return false;
	}
};

export async function toggleSelfMute(_guildId: string | null = null): Promise<void> {
	if (Keybind.isPushToTalkEffective()) {
		logger.debug('Ignoring self-mute toggle: push-to-talk controls the microphone');
		return;
	}
	const room = MediaEngine.room;
	const connectedChannelId = MediaEngine.channelId;
	const microphoneFailureLatched = MediaEngine.isMicrophoneFailureLatched();
	const currentMute = LocalVoiceState.getSelfMute() || microphoneFailureLatched;
	const currentDeaf = LocalVoiceState.getSelfDeaf();
	const willUndeafen = currentDeaf;
	const willUnmute = currentMute;
	const willMute = !currentMute && !currentDeaf;
	const willBeUnmuted = willUnmute || willUndeafen;
	const isPermissionMuted = isVoicePermissionMuteActive(
		MediaEngine.getCurrentUserVoiceState(MediaEngine.guildId),
		MediaEngine.guildId,
		connectedChannelId,
	);
	logger.info('toggleSelfMute', {
		currentMute,
		currentDeaf,
		microphoneFailureLatched,
		willUnmute,
		willUndeafen,
		willMute,
		willBeUnmuted,
		hasRoom: !!room,
		micPermissionState: MediaPermission.getMicrophonePermissionState(),
		isPermissionMuted,
	});
	if (willBeUnmuted) {
		if (isPermissionMuted) {
			logger.info('Voice permissions prevent unmute, staying muted');
			MediaEngine.syncLocalVoiceStateWithServer({self_mute: true});
			return;
		}
		if (MediaPermission.isMicrophoneExplicitlyDenied()) {
			logger.warn('Microphone permission explicitly denied, cannot unmute');
			handleMicrophonePermissionBlocked();
			return;
		}
		MediaEngine.clearMicrophoneFailureLatch();
		if (!MediaPermission.isMicrophoneGranted()) {
			logger.info('Microphone permission not granted, requesting permission');
			const permissionGranted = room?.localParticipant
				? await requestMicrophoneInVoiceChannel(room, connectedChannelId)
				: await requestMicrophoneDirectly();
			if (!permissionGranted) {
				logger.warn('Microphone permission request failed, staying muted');
				LocalVoiceState.updateSelfMute(true);
				return;
			}
			const currentMuteAfterPermission = LocalVoiceState.getSelfMute();
			if (!currentMuteAfterPermission) {
				logger.debug('Already unmuted after permission grant, skipping toggle');
				SoundCommands.playSound(SoundType.Unmute);
				return;
			}
		}
		if (!LocalVoiceState.getSelfMute() && !LocalVoiceState.getSelfDeaf()) {
			logger.debug('Unmute intent only had to clear the microphone failure latch');
			SoundCommands.playSound(SoundType.Unmute);
			return;
		}
	}
	LocalVoiceState.toggleSelfMute();
	const newMute = LocalVoiceState.getSelfMute();
	const newDeaf = LocalVoiceState.getSelfDeaf();
	logger.debug('Voice state updated', {newMute, newDeaf});
	if (!newMute) {
		SoundCommands.playSound(SoundType.Unmute);
	} else {
		SoundCommands.playSound(SoundType.Mute);
	}
}

type VoiceStateProperty = 'self_mute' | 'self_deaf' | 'self_video' | 'self_stream';

const updateConnectionProperty = async (
	connectionId: string,
	property: VoiceStateProperty,
	value: boolean,
): Promise<void> => {
	const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
	if (!voiceState) return;
	const socket = GatewayConnection.socket;
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
	const socket = GatewayConnection.socket;
	if (!socket) return;
	for (const connectionId of connectionIds) {
		const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
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

export async function toggleSelfMuteForConnection(connectionId: string): Promise<void> {
	const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
	if (!voiceState) return;
	const target = !voiceState.self_mute;
	await updateConnectionProperty(connectionId, 'self_mute', target);
	if (target) SoundCommands.playSound(SoundType.Mute);
	else SoundCommands.playSound(SoundType.Unmute);
}

export async function toggleSelfDeafenForConnection(connectionId: string): Promise<void> {
	const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
	if (!voiceState) return;
	const target = !voiceState.self_deaf;
	await updateConnectionProperty(connectionId, 'self_deaf', target);
	if (target) SoundCommands.playSound(SoundType.Deaf);
	else SoundCommands.playSound(SoundType.Undeaf);
}

export async function turnOffCameraForConnection(connectionId: string): Promise<void> {
	await updateConnectionProperty(connectionId, 'self_video', false);
}

export async function turnOffStreamForConnection(connectionId: string): Promise<void> {
	await updateConnectionProperty(connectionId, 'self_stream', false);
}

export async function bulkMuteConnections(connectionIds: Array<string>, mute: boolean = true): Promise<void> {
	await updateConnectionsProperty(connectionIds, 'self_mute', mute);
}

export async function bulkDeafenConnections(connectionIds: Array<string>, deafen: boolean = true): Promise<void> {
	await updateConnectionsProperty(connectionIds, 'self_deaf', deafen);
}

export async function bulkTurnOffCameras(connectionIds: Array<string>): Promise<void> {
	await updateConnectionsProperty(connectionIds, 'self_video', false);
}

export async function bulkDisconnect(connectionIds: Array<string>): Promise<void> {
	const socket = GatewayConnection.socket;
	if (!socket) return;
	for (const connectionId of connectionIds) {
		const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
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
}

export async function bulkMoveConnections(connectionIds: Array<string>, targetChannelId: string): Promise<void> {
	const socket = GatewayConnection.socket;
	if (!socket) return;
	const localConnectionId = MediaEngine.connectionId;
	for (const connectionId of connectionIds) {
		const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
		if (!voiceState) continue;
		if (localConnectionId && connectionId === localConnectionId) {
			await MediaEngine.connectToVoiceChannel(voiceState.guild_id ?? null, targetChannelId);
			continue;
		}
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
}
