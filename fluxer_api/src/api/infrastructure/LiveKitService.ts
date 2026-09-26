// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ListParticipantsResult} from '@app/api/infrastructure/ILiveKitService';
import {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import {Logger} from '@app/api/Logger';
import type {VoiceRegionMetadata, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';
import {AccessToken, RoomServiceClient, TrackSource} from 'livekit-server-sdk';

interface CreateTokenParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	tokenNonce: string;
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
	canSpeak?: boolean;
	canStream?: boolean;
	canVideo?: boolean;
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
	deaf?: boolean;
}

interface ServerClientConfig {
	endpoint: string;
	apiKey: string;
	apiSecret: string;
	isActive: boolean;
	roomServiceClient: RoomServiceClient;
}

interface LiveKitPublishPermissions {
	canSpeak: boolean;
	canStream: boolean;
	canVideo: boolean;
}

export const VOICE_TOKEN_TTL_SECONDS = 60 * 10;

export function computeLiveKitPublishSources(permissions: LiveKitPublishPermissions): Array<TrackSource> {
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

function toHttpUrl(wsUrl: string): string {
	return wsUrl.replace(/^wss:\/\//, 'https://').replace(/^ws:\/\//, 'http://');
}

function createRoomServiceClient(endpoint: string, apiKey: string, apiSecret: string): RoomServiceClient {
	const httpUrl = toHttpUrl(endpoint);
	const parsed = new URL(httpUrl);
	const pathPrefix = parsed.pathname.replace(/\/+$/, '');
	const client = new RoomServiceClient(parsed.origin, apiKey, apiSecret, {requestTimeout: 60});
	if (pathPrefix) {
		const rpc = Reflect.get(client, 'rpc');
		if (rpc != null && typeof rpc === 'object' && 'prefix' in rpc) {
			Reflect.set(rpc, 'prefix', `${pathPrefix}${String(Reflect.get(rpc, 'prefix'))}`);
		}
	}
	return client;
}

function resolveRoomServiceEndpoint(server: VoiceServerRecord): string {
	const defaultRegion = Config.voice.defaultRegion;
	const internalUrl = Config.voice.internalUrl;
	if (!defaultRegion || !internalUrl) {
		return server.endpoint;
	}
	if (server.regionId !== defaultRegion.id || server.serverId !== `${defaultRegion.id}-server-1`) {
		return server.endpoint;
	}
	return internalUrl;
}

export class LiveKitService extends ILiveKitService {
	private serverClients: Map<string, Map<string, ServerClientConfig>> = new Map();
	private topology: VoiceTopology;

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

	async createToken(params: CreateTokenParams): Promise<{
		token: string;
		endpoint: string;
	}> {
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
		metadata['token_nonce'] = params.tokenNonce;
		metadata['issued_at'] = Math.floor(Date.now() / 1000).toString();
		if (guildId !== undefined) {
			metadata['guild_id'] = guildId.toString();
		} else {
			metadata['dm_call'] = 'true';
		}
		const canPublishSources = computeLiveKitPublishSources({canSpeak, canStream, canVideo});
		const accessToken = new AccessToken(server.apiKey, server.apiSecret, {
			identity: participantIdentity,
			metadata: JSON.stringify(metadata),
			ttl: VOICE_TOKEN_TTL_SECONDS,
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
					if (track.source === TrackSource.MICROPHONE && track.sid) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid, mute);
					}
				}
			}
			if (deaf !== undefined) {
				const canPublishSources = computeLiveKitPublishSources({
					canSpeak: params.canSpeak ?? true,
					canStream: params.canStream ?? true,
					canVideo: params.canVideo ?? true,
				});
				await server.roomServiceClient.updateParticipant(roomName, participantIdentity, undefined, {
					canPublish: !deaf && canPublishSources.length > 0,
					canSubscribe: !deaf,
					canPublishSources,
				});
			}
		} catch (error) {
			Logger.error({error}, 'Error updating LiveKit participant');
		}
	}

	async updateParticipantPermissions(params: UpdateParticipantPermissionsParams): Promise<void> {
		const {userId, guildId, channelId, connectionId, regionId, serverId, canSpeak, canStream, canVideo, deaf} = params;
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
			const canPublishSources = computeLiveKitPublishSources({canSpeak, canStream, canVideo});
			const participantPermission = {
				canPublish: !deaf && canPublishSources.length > 0,
				canPublishSources,
				...(deaf !== undefined ? {canSubscribe: !deaf} : {}),
			};
			await server.roomServiceClient.updateParticipant(roomName, participantIdentity, undefined, {
				...participantPermission,
			});
			if (!canStream && participant.tracks) {
				for (const track of participant.tracks) {
					if (
						(track.source === TrackSource.SCREEN_SHARE || track.source === TrackSource.SCREEN_SHARE_AUDIO) &&
						track.sid
					) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid, true);
					}
				}
			}
			if (!canSpeak && participant.tracks) {
				for (const track of participant.tracks) {
					if (track.source === TrackSource.MICROPHONE && track.sid) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid, true);
					}
				}
			}
			if (!canVideo && participant.tracks) {
				for (const track of participant.tracks) {
					if (track.source === TrackSource.CAMERA && track.sid) {
						await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, track.sid, true);
					}
				}
			}
			Logger.debug({participantIdentity, roomName, canSpeak, canStream, canVideo}, 'Updated participant permissions');
		} catch (error) {
			Logger.error({error}, 'Error updating LiveKit participant permissions');
		}
	}

	async disconnectParticipant(params: DisconnectParticipantParams): Promise<void> {
		const {userId, guildId, channelId, connectionId, regionId, serverId} = params;
		const roomName = this.getRoomName(guildId, channelId);
		const participantIdentity = this.getParticipantIdentity(userId, connectionId);
		const server = this.tryResolveServerClient(regionId, serverId);
		if (server === null) {
			Logger.debug(
				{regionId, serverId, participantIdentity, roomName},
				'LiveKit disconnect skipped — pinned server no longer exists in topology',
			);
			return;
		}
		try {
			await server.roomServiceClient.removeParticipant(roomName, participantIdentity);
		} catch (error) {
			if (LiveKitService.isHttp404(error)) {
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
	}): Promise<ListParticipantsResult> {
		const {guildId, channelId, regionId, serverId} = params;
		const roomName = this.getRoomName(guildId, channelId);
		const server = this.tryResolveServerClient(regionId, serverId);
		if (server === null) {
			return {
				status: 'error',
				errorCode: 'server_missing',
				retryable: false,
				serverMissing: true,
			};
		}
		try {
			const participants = await server.roomServiceClient.listParticipants(roomName);
			return {
				status: 'ok',
				participants: participants.map((participant) => ({identity: participant.identity})),
			};
		} catch (error) {
			Logger.warn({error, regionId, serverId, roomName}, 'LiveKit listParticipants failed');
			const status = LiveKitService.getHttpStatus(error);
			const isRetryable = status != null && status >= 500;
			return {
				status: 'error',
				errorCode: error instanceof Error ? error.message : 'unknown',
				retryable: isRetryable,
			};
		}
	}

	private static isHttp404(error: unknown): boolean {
		return LiveKitService.getHttpStatus(error) === 404;
	}

	private static getHttpStatus(error: unknown): number | null {
		if (!(error instanceof Error) || !('status' in error)) {
			return null;
		}
		const status = (
			error as {
				status: unknown;
			}
		).status;
		return typeof status === 'number' ? status : null;
	}

	private tryResolveServerClient(regionId: string, serverId: string): ServerClientConfig | null {
		const region = this.serverClients.get(regionId);
		if (!region) {
			return null;
		}
		return region.get(serverId) ?? null;
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
				const roomServiceEndpoint = resolveRoomServiceEndpoint(server);
				serverMap.set(server.serverId, {
					endpoint: server.endpoint,
					apiKey: server.apiKey,
					apiSecret: server.apiSecret,
					isActive: server.isActive,
					roomServiceClient: createRoomServiceClient(roomServiceEndpoint, server.apiKey, server.apiSecret),
				});
			}
			newMap.set(region.id, serverMap);
		}
		this.serverClients = newMap;
	}
}
