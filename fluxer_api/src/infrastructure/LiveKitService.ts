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

import {AccessToken, RoomServiceClient, TrackSource, TrackType} from 'livekit-server-sdk';
import type {ChannelID, GuildID, UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {Logger} from '~/Logger';
import type {VoiceRegionMetadata, VoiceServerRecord} from '~/voice/VoiceModel';
import type {VoiceTopology} from '~/voice/VoiceTopology';
import {ILiveKitService} from './ILiveKitService';

interface CreateTokenParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
	mute?: boolean;
	deaf?: boolean;
	canSpeak?: boolean;
	canStream?: boolean;
	canVideo?: boolean;
}

interface UpdateParticipantParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
	mute?: boolean;
	deaf?: boolean;
}

interface DisconnectParticipantParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
}

interface UpdateParticipantPermissionsParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
	canSpeak: boolean;
	canStream: boolean;
	canVideo: boolean;
}

interface ServerClientConfig {
	endpoint: string;
	apiKey: string;
	apiSecret: string;
	roomServiceClient: RoomServiceClient;
}

export class LiveKitService extends ILiveKitService {
	private serverClients: Map<string, Map<string, ServerClientConfig>> = new Map();
	private topology: VoiceTopology;
	private static readonly DEFAULT_PUBLISH_SOURCES = [
		TrackSource.CAMERA,
		TrackSource.MICROPHONE,
		TrackSource.SCREEN_SHARE,
		TrackSource.SCREEN_SHARE_AUDIO,
	];

	constructor(topology: VoiceTopology) {
		super();

		if (!Config.voice.enabled) {
			throw new Error('Voice is not enabled. Set VOICE_ENABLED=true to use voice features.');
		}

		this.topology = topology;
		this.refreshServerClients();
		this.topology.registerSubscriber(() => {
			try {
				this.refreshServerClients();
			} catch (error) {
				Logger.error({error}, 'Failed to refresh LiveKit server clients after topology update');
			}
		});
	}

	async createToken(params: CreateTokenParams): Promise<{token: string; endpoint: string}> {
		const {
			userId,
			guildId,
			channelId,
			connectionId,
			regionId,
			serverId,
			deaf = false,
			canSpeak = true,
			canStream = true,
			canVideo = true,
		} = params;
		const server = this.resolveServerClient(regionId, serverId);
		const roomName = this.getRoomName(guildId, channelId);
		const participantIdentity = this.getParticipantIdentity(userId, connectionId);

		const metadata: Record<string, string> = {
			user_id: userId.toString(),
			channel_id: channelId.toString(),
			connection_id: connectionId,
			region_id: regionId,
			server_id: serverId,
		};

		if (guildId !== undefined) {
			metadata.guild_id = guildId.toString();
		} else {
			metadata.dm_call = 'true';
		}

		const canPublishSources = LiveKitService.computePublishSources({canSpeak, canStream, canVideo});

		const accessToken = new AccessToken(server.apiKey, server.apiSecret, {
			identity: participantIdentity,
			metadata: JSON.stringify(metadata),
		});

		accessToken.addGrant({
			roomJoin: true,
			room: roomName,
			canPublish: !deaf && canPublishSources.length > 0,
			canSubscribe: !deaf,
			canPublishSources,
		});

		const token = await accessToken.toJwt();
		return {token, endpoint: server.endpoint};
	}

	private static computePublishSources(permissions: {
		canSpeak: boolean;
		canStream: boolean;
		canVideo: boolean;
	}): Array<TrackSource> {
		const sources: Array<TrackSource> = [];

		if (permissions.canSpeak) {
			sources.push(TrackSource.MICROPHONE);
		}

		if (permissions.canVideo) {
			sources.push(TrackSource.CAMERA);
		}

		if (permissions.canStream) {
			sources.push(TrackSource.SCREEN_SHARE);
			sources.push(TrackSource.SCREEN_SHARE_AUDIO);
		}

		return sources;
	}

	async updateParticipant(params: UpdateParticipantParams): Promise<void> {
		const {userId, guildId, channelId, connectionId, regionId, serverId, mute, deaf} = params;
		const roomName = this.getRoomName(guildId, channelId);
		const participantIdentity = this.getParticipantIdentity(userId, connectionId);
		const server = this.resolveServerClient(regionId, serverId);

		try {
			const participants = await server.roomServiceClient.listParticipants(roomName);
			const participant = participants.find((p) => p.identity === participantIdentity);

			if (!participant) {
				return;
			}

			if (mute !== undefined && participant.tracks) {
				for (const track of participant.tracks) {
					if (track.type === TrackType.AUDIO) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid!, mute);
					}
				}
			}

			if (deaf !== undefined) {
				await server.roomServiceClient.updateParticipant(roomName, participantIdentity, undefined, {
					canPublish: !deaf,
					canSubscribe: !deaf,
					canPublishSources: LiveKitService.DEFAULT_PUBLISH_SOURCES,
				});
			}
		} catch (error) {
			Logger.error({error}, 'Error updating LiveKit participant');
		}
	}

	async updateParticipantPermissions(params: UpdateParticipantPermissionsParams): Promise<void> {
		const {userId, guildId, channelId, connectionId, regionId, serverId, canSpeak, canStream, canVideo} = params;
		const roomName = this.getRoomName(guildId, channelId);
		const participantIdentity = this.getParticipantIdentity(userId, connectionId);
		const server = this.resolveServerClient(regionId, serverId);

		try {
			const participants = await server.roomServiceClient.listParticipants(roomName);
			const participant = participants.find((p) => p.identity === participantIdentity);

			if (!participant) {
				Logger.warn({participantIdentity, roomName}, 'Participant not found for permission update');
				return;
			}

			const canPublishSources = LiveKitService.computePublishSources({canSpeak, canStream, canVideo});

			await server.roomServiceClient.updateParticipant(roomName, participantIdentity, undefined, {
				canPublish: canPublishSources.length > 0,
				canPublishSources,
			});

			if (!canStream && participant.tracks) {
				for (const track of participant.tracks) {
					if (track.source === TrackSource.SCREEN_SHARE || track.source === TrackSource.SCREEN_SHARE_AUDIO) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid!, true);
					}
				}
			}

			if (!canSpeak && participant.tracks) {
				for (const track of participant.tracks) {
					if (track.source === TrackSource.MICROPHONE) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid!, true);
					}
				}
			}

			if (!canVideo && participant.tracks) {
				for (const track of participant.tracks) {
					if (track.source === TrackSource.CAMERA) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid!, true);
					}
				}
			}

			Logger.info({participantIdentity, roomName, canSpeak, canStream, canVideo}, 'Updated participant permissions');
		} catch (error) {
			Logger.error({error}, 'Error updating LiveKit participant permissions');
		}
	}

	async disconnectParticipant(params: DisconnectParticipantParams): Promise<void> {
		const {userId, guildId, channelId, connectionId, regionId, serverId} = params;
		const roomName = this.getRoomName(guildId, channelId);
		const participantIdentity = this.getParticipantIdentity(userId, connectionId);
		const server = this.resolveServerClient(regionId, serverId);

		try {
			await server.roomServiceClient.removeParticipant(roomName, participantIdentity);
		} catch (error) {
			if (error instanceof Error && 'status' in error && (error as {status: number}).status === 404) {
				Logger.debug({participantIdentity, roomName}, 'LiveKit participant already disconnected');
				return;
			}
			Logger.error({error}, 'Error disconnecting LiveKit participant');
		}
	}

	async listParticipants(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		regionId: string;
		serverId: string;
	}): Promise<Array<{identity: string}>> {
		const {guildId, channelId, regionId, serverId} = params;
		const roomName = this.getRoomName(guildId, channelId);
		const server = this.resolveServerClient(regionId, serverId);

		try {
			const participants = await server.roomServiceClient.listParticipants(roomName);
			return participants.map((participant) => ({
				identity: participant.identity,
			}));
		} catch (error) {
			Logger.error({error}, 'Error listing LiveKit participants');
			return [];
		}
	}

	getDefaultRegionId(): string | null {
		return this.topology.getDefaultRegionId();
	}

	getRegionMetadata(): Array<VoiceRegionMetadata> {
		return this.topology.getRegionMetadataList();
	}

	getServer(regionId: string, serverId: string): VoiceServerRecord | null {
		return this.topology.getServer(regionId, serverId);
	}

	private getRoomName(guildId: GuildID | undefined, channelId: ChannelID): string {
		if (guildId === undefined) {
			return `dm_channel_${channelId}`;
		}
		return `guild_${guildId}_channel_${channelId}`;
	}

	private getParticipantIdentity(userId: UserID, connectionId: string): string {
		return `user_${userId}_${connectionId}`;
	}

	private resolveServerClient(regionId: string, serverId: string): ServerClientConfig {
		const region = this.serverClients.get(regionId);
		if (!region) {
			throw new Error(`Unknown LiveKit region: ${regionId}`);
		}

		const server = region.get(serverId);
		if (!server) {
			throw new Error(`Unknown LiveKit server: ${regionId}/${serverId}`);
		}

		return server;
	}

	private refreshServerClients(): void {
		const newMap: Map<string, Map<string, ServerClientConfig>> = new Map();
		const regions = this.topology.getAllRegions();

		for (const region of regions) {
			const servers = this.topology.getServersForRegion(region.id);
			const serverMap: Map<string, ServerClientConfig> = new Map();

			for (const server of servers) {
				serverMap.set(server.serverId, {
					endpoint: server.endpoint,
					apiKey: server.apiKey,
					apiSecret: server.apiSecret,
					roomServiceClient: new RoomServiceClient(server.endpoint, server.apiKey, server.apiSecret),
				});
			}

			newMap.set(region.id, serverMap);
		}

		this.serverClients = newMap;
	}
}
