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

import type {LocalAudioTrack, LocalParticipant, Room} from 'livekit-client';
import {Track, VideoQuality} from 'livekit-client';
import {autorun, makeAutoObservable, runInAction} from 'mobx';
import {Permissions} from '~/Constants';
import {Logger} from '~/lib/Logger';
import PermissionStore from '~/stores/PermissionStore';

const logger = new Logger('VoicePermissionManager');

export interface VoicePermissions {
	canSpeak: boolean;
	canStream: boolean;
	canUseVideo: boolean;
	canConnect: boolean;
	canPrioritySpeaker: boolean;
}

export type TrackSource = 'audio' | 'video' | 'screenShare';
export type MuteReason = 'guild' | 'push_to_talk' | 'self' | null;

export interface VoiceState {
	guild_id: string;
	channel_id: string | null;
	user_id: string;
	connection_id: string;
	mute: boolean;
	deaf: boolean;
	self_mute: boolean;
	self_deaf: boolean;
	self_video: boolean;
	self_stream: boolean;
	suppress: boolean;
}

type LocalParticipantWithScreenShare = LocalParticipant & {
	isScreenShareEnabled?: boolean;
	setScreenShareEnabled?: (
		enabled: boolean,
		constraints?: {audio?: boolean; video?: boolean; selfBrowserSurface?: 'include' | 'exclude'},
	) => Promise<void>;
};

const DEFAULT_PERMISSIONS: VoicePermissions = {
	canSpeak: true,
	canStream: true,
	canUseVideo: true,
	canConnect: true,
	canPrioritySpeaker: false,
};

class VoicePermissionManager {
	private currentPermissions: VoicePermissions = {...DEFAULT_PERMISSIONS};
	private isDeafened = false;
	private currentChannelId: string | null = null;
	private currentRoom: Room | null = null;
	private permissionDisposer: (() => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		logger.debug('VoicePermissionManager initialized');
	}

	syncWithPermissionStore(guildId: string, channelId: string, room: Room): void {
		this.currentChannelId = channelId;
		this.currentRoom = room;

		this.stopPermissionWatch();

		this.permissionDisposer = autorun(
			() => {
				if (!this.currentChannelId) return;

				const permissions = PermissionStore.getChannelPermissions(this.currentChannelId);
				if (permissions === undefined) {
					logger.warn('[syncWithPermissionStore] No permissions for channel', {channelId: this.currentChannelId});
					return;
				}

				const currentPermissionsValue = PermissionStore.getChannelPermissions(this.currentChannelId);
				if (currentPermissionsValue === undefined) return;

				const newPermissions: VoicePermissions = {
					canSpeak: (currentPermissionsValue & Permissions.SPEAK) === Permissions.SPEAK,
					canStream: (currentPermissionsValue & Permissions.STREAM) === Permissions.STREAM,
					canUseVideo: (currentPermissionsValue & Permissions.STREAM) === Permissions.STREAM,
					canConnect: (currentPermissionsValue & Permissions.CONNECT) === Permissions.CONNECT,
					canPrioritySpeaker: (currentPermissionsValue & Permissions.PRIORITY_SPEAKER) === Permissions.PRIORITY_SPEAKER,
				};

				logger.debug('[syncWithPermissionStore] Permissions computed', {
					channelId: this.currentChannelId,
					permissions: newPermissions,
				});

				this.handlePermissionUpdate(newPermissions);
			},
			{
				name: 'VoicePermissionManager-syncWithPermissionStore',
			},
		);

		logger.info('[syncWithPermissionStore] Started permission watching', {guildId, channelId});
	}

	private stopPermissionWatch(): void {
		if (this.permissionDisposer) {
			this.permissionDisposer();
			this.permissionDisposer = null;
			logger.debug('[stopPermissionWatch] Stopped permission watching');
		}
	}

	private handlePermissionUpdate(newPermissions: VoicePermissions): void {
		const oldPermissions = this.currentPermissions;

		if (
			oldPermissions.canSpeak === newPermissions.canSpeak &&
			oldPermissions.canStream === newPermissions.canStream &&
			oldPermissions.canUseVideo === newPermissions.canUseVideo &&
			oldPermissions.canConnect === newPermissions.canConnect &&
			oldPermissions.canPrioritySpeaker === newPermissions.canPrioritySpeaker
		) {
			logger.debug('[handlePermissionUpdate] Permissions unchanged, skipping update');
			return;
		}

		runInAction(() => {
			this.currentPermissions = newPermissions;
		});

		if (!this.currentRoom) {
			logger.debug('[handlePermissionUpdate] No active room, skipping enforcement');
			return;
		}

		if (oldPermissions.canSpeak && !newPermissions.canSpeak) {
			logger.warn('[handlePermissionUpdate] SPEAK permission revoked - enforcing');
			void this.handlePermissionRevoked('audio', this.currentRoom);
		}

		if (oldPermissions.canStream && !newPermissions.canStream) {
			logger.warn('[handlePermissionUpdate] STREAM permission revoked - enforcing');
			void this.handlePermissionRevoked('screenShare', this.currentRoom);
		}

		if (oldPermissions.canUseVideo && !newPermissions.canUseVideo) {
			logger.warn('[handlePermissionUpdate] VIDEO permission revoked - enforcing');
			void this.handlePermissionRevoked('video', this.currentRoom);
		}

		logger.info('[handlePermissionUpdate] Permissions updated and enforced', {
			old: oldPermissions,
			new: newPermissions,
		});
	}

	handlePermissionChange(permission: 'speak' | 'stream' | 'video', allowed: boolean): void {
		const room = this.currentRoom;
		if (!room) {
			logger.warn('[handlePermissionChange] No active room');
			return;
		}

		let hasChanged = false;
		switch (permission) {
			case 'speak':
				if (this.currentPermissions.canSpeak !== allowed) hasChanged = true;
				break;
			case 'stream':
				if (this.currentPermissions.canStream !== allowed || this.currentPermissions.canUseVideo !== allowed)
					hasChanged = true;
				break;
			case 'video':
				if (this.currentPermissions.canUseVideo !== allowed) hasChanged = true;
				break;
		}

		if (!hasChanged) {
			logger.debug('[handlePermissionChange] Permission unchanged, skipping', {permission, allowed});
			return;
		}

		logger.info('[handlePermissionChange] Processing permission change', {permission, allowed});

		runInAction(() => {
			switch (permission) {
				case 'speak':
					this.currentPermissions.canSpeak = allowed;
					if (!allowed) void this.handlePermissionRevoked('audio', room);
					break;
				case 'stream':
					this.currentPermissions.canStream = allowed;
					this.currentPermissions.canUseVideo = allowed;
					if (!allowed) {
						void this.handlePermissionRevoked('screenShare', room);
						void this.handlePermissionRevoked('video', room);
					}
					break;
				case 'video':
					this.currentPermissions.canUseVideo = allowed;
					if (!allowed) void this.handlePermissionRevoked('video', room);
					break;
			}
		});
	}

	initializeSubscriptions(room: Room): void {
		if (!room) {
			logger.warn('[initializeSubscriptions] No room provided');
			return;
		}

		logger.debug('[initializeSubscriptions] Setting up initial subscriptions', {
			deafened: this.isDeafened,
			participantCount: room.remoteParticipants.size,
		});

		room.remoteParticipants.forEach((participant) => {
			participant.audioTrackPublications.forEach((publication) => {
				try {
					if (publication.source === Track.Source.Microphone) {
						publication.setSubscribed(!this.isDeafened);
					}
				} catch (error) {
					logger.error('[initializeSubscriptions] Failed to set audio subscription', {error});
				}
			});

			participant.videoTrackPublications.forEach((publication) => {
				try {
					publication.setSubscribed(false);
					publication.setVideoQuality(VideoQuality.LOW);
				} catch (error) {
					logger.error('[initializeSubscriptions] Failed to unsubscribe video', {error});
				}
			});
		});

		logger.info('[initializeSubscriptions] Complete', {participantCount: room.remoteParticipants.size});
	}

	applyDeafen(room: Room, deafened: boolean): void {
		if (!room) {
			logger.warn('[applyDeafen] No room provided');
			return;
		}

		runInAction(() => {
			this.isDeafened = deafened;
		});

		logger.debug('[applyDeafen] Applying deaf state', {
			deafened,
			participantCount: room.remoteParticipants.size,
		});

		room.remoteParticipants.forEach((participant) => {
			participant.audioTrackPublications.forEach((publication) => {
				try {
					publication.setEnabled(!deafened);
					publication.setSubscribed(!deafened);
				} catch (error) {
					logger.error('[applyDeafen] Failed to apply deaf state', {
						error,
						participantId: participant.identity,
					});
				}
			});
		});

		logger.info('[applyDeafen] Complete', {deafened, participantCount: room.remoteParticipants.size});
	}

	updateSubscriptionsForPermissionChange(room: Room, permissions: VoicePermissions): void {
		if (!room) {
			logger.warn('[updateSubscriptionsForPermissionChange] No room provided');
			return;
		}

		const oldPermissions = this.currentPermissions;

		if (
			oldPermissions.canSpeak === permissions.canSpeak &&
			oldPermissions.canStream === permissions.canStream &&
			oldPermissions.canUseVideo === permissions.canUseVideo &&
			oldPermissions.canConnect === permissions.canConnect &&
			oldPermissions.canPrioritySpeaker === permissions.canPrioritySpeaker
		) {
			logger.debug('[updateSubscriptionsForPermissionChange] Permissions unchanged, skipping update');
			return;
		}

		runInAction(() => {
			this.currentPermissions = permissions;
		});

		logger.debug('[updateSubscriptionsForPermissionChange] Permissions updated', {
			old: oldPermissions,
			new: permissions,
		});

		if (oldPermissions.canSpeak && !permissions.canSpeak) {
			logger.warn('[updateSubscriptionsForPermissionChange] Speak permission revoked');
			void this.handlePermissionRevoked('audio', room);
		}
		if (oldPermissions.canStream && !permissions.canStream) {
			logger.warn('[updateSubscriptionsForPermissionChange] Stream permission revoked');
			void this.handlePermissionRevoked('screenShare', room);
		}
		if (oldPermissions.canUseVideo && !permissions.canUseVideo) {
			logger.warn('[updateSubscriptionsForPermissionChange] Video permission revoked');
			void this.handlePermissionRevoked('video', room);
		}

		logger.info('[updateSubscriptionsForPermissionChange] Complete', {permissions});
	}

	canPublishAudio(): boolean {
		return this.currentPermissions.canSpeak;
	}

	canPublishVideo(): boolean {
		return this.currentPermissions.canUseVideo;
	}

	canPublishScreenShare(): boolean {
		return this.currentPermissions.canStream;
	}

	private async handlePermissionRevoked(source: TrackSource, room: Room): Promise<void> {
		if (!room?.localParticipant) {
			logger.warn('[handlePermissionRevoked] No local participant');
			return;
		}

		logger.info('[handlePermissionRevoked] Revoking permission', {source});
		const localParticipant = room.localParticipant;

		try {
			switch (source) {
				case 'audio': {
					const audioPublications = Array.from(localParticipant.audioTrackPublications.values());
					const tracks = audioPublications
						.map((pub) => pub.track)
						.filter((track): track is LocalAudioTrack => Boolean(track));
					if (tracks.length > 0) {
						await Promise.allSettled(tracks.map((track) => localParticipant.unpublishTrack(track)));
					}
					break;
				}
				case 'video':
					await localParticipant.setCameraEnabled(false);
					break;
				case 'screenShare': {
					const participant = localParticipant as LocalParticipantWithScreenShare;
					if (participant.setScreenShareEnabled) {
						await participant.setScreenShareEnabled(false);
					}
					break;
				}
			}
			logger.info('[handlePermissionRevoked] Successfully revoked permission', {source});
		} catch (error) {
			logger.error('[handlePermissionRevoked] Failed to revoke permission', {source, error});
		}
	}

	getMuteReason(voiceState: VoiceState | null): MuteReason {
		if (!voiceState) return null;
		if (voiceState.mute) return 'guild';
		if (voiceState.self_mute) return 'self';
		return null;
	}

	getPermissions(): VoicePermissions {
		return {...this.currentPermissions};
	}

	setPermissions(permissions: Partial<VoicePermissions>): void {
		const hasChanges = Object.entries(permissions).some(
			([key, value]) => this.currentPermissions[key as keyof VoicePermissions] !== value,
		);

		if (!hasChanges) {
			logger.debug('[setPermissions] No changes detected, skipping update');
			return;
		}

		runInAction(() => {
			this.currentPermissions = {...this.currentPermissions, ...permissions};
		});
		logger.debug('[setPermissions] Updated', {permissions: this.currentPermissions});
	}

	getDeafened(): boolean {
		return this.isDeafened;
	}

	reset(): void {
		this.stopPermissionWatch();
		runInAction(() => {
			this.currentPermissions = {...DEFAULT_PERMISSIONS};
			this.isDeafened = false;
			this.currentChannelId = null;
			this.currentRoom = null;
		});
		logger.debug('[reset] Permissions reset to defaults');
	}

	extractUserIdFromIdentity(identity: string): string | null {
		const match = identity.match(/^user_(\d+)(?:_(.+))?$/);
		return match ? match[1] : null;
	}
}

const instance = new VoicePermissionManager();
(window as typeof window & {_voicePermissionManager?: VoicePermissionManager})._voicePermissionManager = instance;
export default instance;
