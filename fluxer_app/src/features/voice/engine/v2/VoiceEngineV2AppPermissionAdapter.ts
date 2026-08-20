// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Store} from '@app/features/voice/engine/Store';
import {syncLocalVoiceStateWithServer} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	createVoicePermissionSnapshot,
	transitionVoicePermissionSnapshot,
	type VoicePermissionCommand,
	type VoicePermissionEvent,
	type VoicePermissionSnapshot,
	type VoiceRemoteMicrophonePublicationInput,
} from '@app/features/voice/engine/VoicePermissionStateMachine';
import {
	enforceLocalMediaPublicationCap,
	getLocalMicrophonePublications,
	getLocalScreenSharePublications,
	unpublishLocalMediaPublications,
} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {removeVoiceInputProcessor} from '@app/features/voice/utils/VoiceInputProcessor';
import {getVoiceChannelPermissions, type VoiceChannelPermissions} from '@app/features/voice/utils/VoicePermissionUtils';
import type {LocalAudioTrack, Room} from 'livekit-client';

export {
	createVoiceEngineV2AppSystemPermissionAdapter,
	VoiceEngineV2AppSystemPermissionAdapter,
	type VoiceEngineV2SystemPermissionsApi,
} from './VoiceEngineV2AppSystemPermissionAdapter';

const logger = new Logger('VoiceEngineV2AppPermissionAdapter');

export type VoiceEngineV2AppPermissionTrackSource = 'audio' | 'video' | 'screenShare';

type RemotePublicationAdapter = {
	isDesired?: boolean;
	setEnabled?: (enabled: boolean) => void;
	setSubscribed: (subscribed: boolean) => void;
};

class VoiceEngineV2AppPermissionAdapter extends Store {
	private snapshot: VoicePermissionSnapshot = createVoicePermissionSnapshot();
	private currentRoom: Room | null = null;
	private permissionDisposer: (() => void) | null = null;

	private hasEnforceableMediaSession(): boolean {
		return this.currentRoom != null;
	}

	private sendPermissionEvent(
		event: VoicePermissionEvent,
		options: {
			room?: Room | null;
			publicationsById?: ReadonlyMap<string, RemotePublicationAdapter>;
		} = {},
	): void {
		this.update(() => {
			this.snapshot = transitionVoicePermissionSnapshot(this.snapshot, event);
		});
		this.flushPermissionCommands(options);
	}

	private flushPermissionCommands(options: {
		room?: Room | null;
		publicationsById?: ReadonlyMap<string, RemotePublicationAdapter>;
	}): void {
		const commands = this.snapshot.context.commands;
		if (commands.length === 0) return;
		for (const command of commands) {
			this.applyPermissionCommand(command, options);
		}
		this.update(() => {
			this.snapshot = transitionVoicePermissionSnapshot(this.snapshot, {type: 'permission.clearCommands'});
		});
	}

	private applyPermissionCommand(
		command: VoicePermissionCommand,
		options: {
			room?: Room | null;
			publicationsById?: ReadonlyMap<string, RemotePublicationAdapter>;
		},
	): void {
		switch (command.type) {
			case 'syncSpeakPermission':
				this.syncSpeakPermissionState(command.canSpeak);
				break;
			case 'revokeLocalAudio':
				this.revokePermissionForRoom('audio', options.room ?? this.currentRoom);
				break;
			case 'stopScreenShare':
				this.revokePermissionForRoom('screenShare', options.room ?? this.currentRoom);
				break;
			case 'disableCamera':
				this.revokePermissionForRoom('video', options.room ?? this.currentRoom);
				break;
			case 'setRemoteMicrophoneSubscribed':
			case 'setRemoteVideoSubscribed': {
				const publication = options.publicationsById?.get(command.publicationId);
				if (!publication) return;
				try {
					publication.setSubscribed(command.subscribed);
				} catch (error) {
					logger.error('Failed to set remote subscription', {error, command});
				}
				break;
			}
			case 'setRemoteMicrophoneEnabled': {
				const publication = options.publicationsById?.get(command.publicationId);
				if (!publication?.setEnabled) return;
				try {
					publication.setEnabled(command.enabled);
				} catch (error) {
					logger.error('Failed to set remote microphone enabled state', {error, command});
				}
				break;
			}
			case 'stopPermissionWatch':
				this.disposePermissionWatch();
				break;
		}
	}

	private revokePermissionForRoom(source: VoiceEngineV2AppPermissionTrackSource, room: Room | null | undefined): void {
		if (!room) {
			logger.debug('No active room, skipping permission enforcement', {source});
			return;
		}
		void this.handlePermissionRevoked(source, room);
	}

	private buildRemotePublicationInputs(room: Room): {
		microphonePublications: Array<VoiceRemoteMicrophonePublicationInput>;
		videoPublicationIds: Array<string>;
		publicationsById: Map<string, RemotePublicationAdapter>;
	} {
		const microphonePublications: Array<VoiceRemoteMicrophonePublicationInput> = [];
		const videoPublicationIds: Array<string> = [];
		const publicationsById = new Map<string, RemotePublicationAdapter>();
		room.remoteParticipants.forEach((participant) => {
			let audioIndex = 0;
			participant.audioTrackPublications.forEach((publication) => {
				if (asVoiceTrackSource(publication.source) !== VoiceTrackSource.Microphone) return;
				const publicationId = `${participant.identity}:microphone:${audioIndex}`;
				audioIndex += 1;
				microphonePublications.push({
					publicationId,
					isDesired: publication.isDesired,
				});
				publicationsById.set(publicationId, publication);
			});
			let videoIndex = 0;
			participant.videoTrackPublications.forEach((publication) => {
				const publicationId = `${participant.identity}:video:${videoIndex}`;
				videoIndex += 1;
				videoPublicationIds.push(publicationId);
				publicationsById.set(publicationId, publication);
			});
		});
		return {microphonePublications, videoPublicationIds, publicationsById};
	}

	syncWithPermissionState(guildId: string, channelId: string, room: Room): void {
		this.sendPermissionEvent({type: 'permission.watch.stop'});
		this.update(() => {
			this.currentRoom = room;
		});
		this.beginPermissionWatch(guildId, channelId);
		logger.info('Started permission watching', {guildId, channelId});
	}

	private beginPermissionWatch(guildId: string | null, channelId: string): void {
		assert.equal(this.permissionDisposer, null, 'permission watch must be stopped before starting a new watch');
		assert.ok(this.hasEnforceableMediaSession(), 'permission watch requires an enforceable media session');
		this.sendPermissionEvent({type: 'permission.watch.start', guildId, channelId, roomPresent: true});
		this.permissionDisposer = Permission.subscribe(() => {
			const newPermissions = getVoiceChannelPermissions(channelId);
			if (!newPermissions) {
				logger.warn('No permissions for channel', {channelId});
				return;
			}
			logger.debug('Permissions computed', {
				channelId,
				permissions: newPermissions,
			});
			this.handlePermissionUpdate(newPermissions);
		});
	}

	private disposePermissionWatch(): void {
		if (this.permissionDisposer) {
			this.permissionDisposer();
			this.permissionDisposer = null;
			logger.debug('Stopped permission watching');
		}
	}

	private handlePermissionUpdate(newPermissions: VoiceChannelPermissions): void {
		const oldPermissions = this.snapshot.context.permissions;
		this.sendPermissionEvent({
			type: 'permission.update',
			permissions: newPermissions,
			roomPresent: this.hasEnforceableMediaSession(),
		});
		if (this.snapshot.context.permissions === oldPermissions) {
			logger.debug('Permissions unchanged, skipping update');
			return;
		}
		logger.info('Permissions updated and enforced', {
			old: oldPermissions,
			new: newPermissions,
		});
	}

	handlePermissionChange(permission: 'speak' | 'stream' | 'video', allowed: boolean): void {
		const room = this.currentRoom;
		if (!room) {
			logger.warn('No active media session');
			return;
		}
		const previousSnapshot = this.snapshot;
		this.sendPermissionEvent({type: 'permission.change', permission, allowed, roomPresent: true}, {room});
		if (this.snapshot.context.permissions === previousSnapshot.context.permissions) {
			logger.debug('Permission unchanged, skipping', {permission, allowed});
			return;
		}
		logger.info('Processing permission change', {permission, allowed});
	}

	initializeSubscriptions(room: Room): void {
		if (!room) {
			logger.warn('No room provided');
			return;
		}
		logger.debug('Setting up initial subscriptions', {
			deafened: this.snapshot.context.deafened,
			participantCount: room.remoteParticipants.size,
		});
		const publicationInputs = this.buildRemotePublicationInputs(room);
		this.sendPermissionEvent(
			{
				type: 'subscription.initialize',
				microphonePublications: publicationInputs.microphonePublications,
				videoPublicationIds: publicationInputs.videoPublicationIds,
			},
			{
				publicationsById: publicationInputs.publicationsById,
			},
		);
		logger.info('Complete', {participantCount: room.remoteParticipants.size});
	}

	applyDeafen(room: Room, deafened: boolean): void {
		if (!room) {
			logger.warn('No room provided');
			return;
		}
		logger.debug('Applying deaf state', {
			deafened,
			participantCount: room.remoteParticipants.size,
		});
		const publicationInputs = this.buildRemotePublicationInputs(room);
		this.sendPermissionEvent(
			{
				type: 'deafen.apply',
				deafened,
				microphonePublications: publicationInputs.microphonePublications,
			},
			{
				publicationsById: publicationInputs.publicationsById,
			},
		);
		logger.info('Complete', {deafened, participantCount: room.remoteParticipants.size});
	}

	updateSubscriptionsForPermissionChange(room: Room, permissions: VoiceChannelPermissions): void {
		if (!room) {
			logger.warn('No room provided');
			return;
		}
		const oldPermissions = this.snapshot.context.permissions;
		this.sendPermissionEvent({type: 'permission.update', permissions, roomPresent: true}, {room});
		if (this.snapshot.context.permissions === oldPermissions) {
			logger.debug('Permissions unchanged, skipping update');
			return;
		}
		logger.debug('Permissions updated', {
			old: oldPermissions,
			new: permissions,
		});
		logger.info('Complete', {permissions});
	}

	canPublishAudio(): boolean {
		return this.snapshot.context.permissions.canSpeak;
	}

	canPublishVideo(): boolean {
		return this.snapshot.context.permissions.canUseVideo;
	}

	canPublishScreenShare(): boolean {
		return this.snapshot.context.permissions.canStream;
	}

	private async handlePermissionRevoked(source: VoiceEngineV2AppPermissionTrackSource, room: Room): Promise<void> {
		if (!room?.localParticipant) {
			logger.warn('No local participant');
			return;
		}
		logger.info('Revoking permission', {source});
		const localParticipant = room.localParticipant;
		try {
			switch (source) {
				case 'audio': {
					const microphonePublications = getLocalMicrophonePublications(localParticipant);
					const tracks = microphonePublications
						.map((pub) => pub.track)
						.filter((track): track is LocalAudioTrack => Boolean(track));
					if (tracks.length > 0) {
						await removeVoiceInputProcessor();
						await Promise.allSettled(tracks.map((track) => localParticipant.unpublishTrack(track)));
					}
					break;
				}
				case 'video':
					await enforceLocalMediaPublicationCap(localParticipant, 'camera');
					await localParticipant.setCameraEnabled(false);
					await enforceLocalMediaPublicationCap(localParticipant, 'camera');
					break;
				case 'screenShare':
					await unpublishLocalMediaPublications(localParticipant, getLocalScreenSharePublications(localParticipant), {
						stopOnUnpublish: true,
					});
					break;
			}
			logger.info('Successfully revoked permission', {source});
		} catch (error) {
			logger.error('Failed to revoke permission', {source, error});
		}
	}

	private syncSpeakPermissionState(canSpeak: boolean): void {
		syncLocalVoiceStateWithServer(canSpeak ? {} : {self_mute: true});
	}

	getPermissions(): VoiceChannelPermissions {
		return {...this.snapshot.context.permissions};
	}

	setPermissions(permissions: Partial<VoiceChannelPermissions>): void {
		const hasChanges = Object.entries(permissions).some(
			([key, value]) => this.snapshot.context.permissions[key as keyof VoiceChannelPermissions] !== value,
		);
		if (!hasChanges) {
			logger.debug('No changes detected, skipping update');
			return;
		}
		this.sendPermissionEvent({type: 'permission.set', permissions});
		logger.debug('Updated', {permissions: this.snapshot.context.permissions});
	}

	getDeafened(): boolean {
		return this.snapshot.context.deafened;
	}

	reset(): void {
		this.sendPermissionEvent({type: 'permission.reset'});
		this.update(() => {
			this.currentRoom = null;
		});
		logger.debug('Permissions reset to defaults');
	}

	extractUserIdFromIdentity(identity: string): string | null {
		const match = identity.match(/^user_(\d+)(?:_(.+))?$/);
		return match ? match[1] : null;
	}
}

const voiceEngineV2AppPermissionAdapter = new VoiceEngineV2AppPermissionAdapter();

export default voiceEngineV2AppPermissionAdapter;
