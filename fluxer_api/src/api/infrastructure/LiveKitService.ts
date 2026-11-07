// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	type CreateTokenParams,
	type DisconnectParticipantParams,
	type GetParticipantResult,
	ILiveKitService,
	type ListParticipantsParams,
	type ListParticipantsResult,
	type ListRoomsParams,
	type ListRoomsResult,
	type LiveKitParticipantRef,
	type MuteMicrophoneTrackParams,
	type SetParticipantPermissionParams,
	type UpdateParticipantParams,
} from '@app/api/infrastructure/ILiveKitService';
import {Logger} from '@app/api/Logger';
import type {VoiceRegionMetadata, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {resolveRoomServiceEndpoint, toHttpUrl} from '@app/api/voice/VoiceServerEndpoints';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';
import {AccessToken, RoomServiceClient, TrackSource} from 'livekit-server-sdk';

interface ServerClientConfig {
	endpoint: string;
	apiKey: string;
	apiSecret: string;
	roomServiceClient: RoomServiceClient;
}

interface LiveKitPublishPermissions {
	canSpeak: boolean;
	canStream: boolean;
	canVideo: boolean;
}

const VOICE_TOKEN_TTL_SECONDS = 60 * 10;

export const SERVER_MUTE_ATTRIBUTE = 'server_mute';

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
			mute = false,
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
		const canPublishSources = computeLiveKitPublishSources({canSpeak: canSpeak && !mute, canStream, canVideo});
		const accessToken = new AccessToken(server.apiKey, server.apiSecret, {
			identity: participantIdentity,
			metadata: JSON.stringify(metadata),
			ttl: VOICE_TOKEN_TTL_SECONDS,
			...(mute ? {attributes: {[SERVER_MUTE_ATTRIBUTE]: 'true'}} : {}),
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
		if (mute === undefined && deaf === undefined) {
			return;
		}
		const roomName = this.getRoomName(guildId, channelId);
		const participantIdentity = this.getParticipantIdentity(userId, connectionId);
		const server = this.resolveServerClient(regionId, serverId);
		const participants = await server.roomServiceClient.listParticipants(roomName);
		const participant = participants.find((p) => p.identity === participantIdentity);
		if (!participant) {
			return;
		}
		const wasServerMuted = participant.attributes[SERVER_MUTE_ATTRIBUTE] === 'true';
		const serverMuted = mute ?? wasServerMuted;
		const deafened = deaf ?? participant.permission?.canSubscribe === false;
		const currentSources = participant.permission?.canPublishSources ?? [];
		const canPublishSources = computeLiveKitPublishSources({
			canSpeak: (params.canSpeak ?? currentSources.includes(TrackSource.MICROPHONE)) && !serverMuted,
			canStream: params.canStream ?? currentSources.includes(TrackSource.SCREEN_SHARE),
			canVideo: params.canVideo ?? currentSources.includes(TrackSource.CAMERA),
		});
		const permission = {
			canPublish: !deafened && canPublishSources.length > 0,
			canSubscribe: !deafened,
			canPublishSources,
		};
		if (serverMuted !== wasServerMuted) {
			await this.setServerMuteAttribute(server, roomName, participantIdentity, serverMuted);
		}
		const updated = await server.roomServiceClient.updateParticipant(
			roomName,
			participantIdentity,
			undefined,
			permission,
		);
		for (const track of updated.tracks ?? participant.tracks) {
			if (!track.sid) {
				continue;
			}
			if (!canPublishSources.includes(track.source)) {
				await this.setTrackMuted(server, roomName, participantIdentity, track.sid, true);
			} else if (track.source === TrackSource.MICROPHONE && wasServerMuted) {
				await this.setTrackMuted(server, roomName, participantIdentity, track.sid, false);
			}
		}
	}

	private async setTrackMuted(
		server: ServerClientConfig,
		roomName: string,
		participantIdentity: string,
		trackSid: string,
		muted: boolean,
	): Promise<void> {
		try {
			await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, trackSid, muted);
		} catch (error) {
			Logger.debug({error, participantIdentity, roomName, trackSid, muted}, 'LiveKit declined track mute change');
		}
	}

	private async setServerMuteAttribute(
		server: ServerClientConfig,
		roomName: string,
		participantIdentity: string,
		mute: boolean,
	): Promise<void> {
		try {
			await server.roomServiceClient.updateParticipant(roomName, participantIdentity, {
				attributes: {[SERVER_MUTE_ATTRIBUTE]: mute ? 'true' : ''},
			});
		} catch (error) {
			Logger.warn({error, participantIdentity, roomName}, 'Failed to record server mute on LiveKit participant');
		}
	}

	async muteMicrophoneTrack(params: MuteMicrophoneTrackParams): Promise<void> {
		const {userId, guildId, channelId, connectionId, regionId, serverId, trackSid} = params;
		const roomName = this.getRoomName(guildId, channelId);
		const participantIdentity = this.getParticipantIdentity(userId, connectionId);
		const server = this.tryResolveServerClient(regionId, serverId);
		if (server === null) {
			Logger.debug({regionId, serverId, participantIdentity, roomName}, 'LiveKit track mute skipped: unknown server');
			return;
		}
		try {
			await server.roomServiceClient.mutePublishedTrack(roomName, participantIdentity, trackSid, true);
		} catch (error) {
			Logger.error({error, participantIdentity, roomName, trackSid}, 'Error muting LiveKit microphone track');
		}
	}

	async disconnectParticipant(params: DisconnectParticipantParams): Promise<void> {
		const {userId, guildId, channelId, connectionId, regionId, serverId} = params;
		await this.removeParticipant({
			regionId,
			serverId,
			roomName: this.getRoomName(guildId, channelId),
			identity: this.getParticipantIdentity(userId, connectionId),
		});
	}

	async removeParticipant(params: LiveKitParticipantRef): Promise<void> {
		const {regionId, serverId, roomName, identity} = params;
		const server = this.tryResolveServerClient(regionId, serverId);
		if (server === null) {
			Logger.debug(
				{regionId, serverId, participantIdentity: identity, roomName},
				'LiveKit disconnect skipped — pinned server no longer exists in topology',
			);
			return;
		}
		try {
			await server.roomServiceClient.removeParticipant(roomName, identity);
		} catch (error) {
			if (LiveKitService.isHttp404(error)) {
				Logger.debug({participantIdentity: identity, roomName}, 'LiveKit participant already disconnected');
				return;
			}
			Logger.error({error}, 'Error disconnecting LiveKit participant');
		}
	}

	async getParticipant(params: LiveKitParticipantRef): Promise<GetParticipantResult> {
		const {regionId, serverId, roomName, identity} = params;
		const server = this.tryResolveServerClient(regionId, serverId);
		if (server === null) {
			return {status: 'error', errorCode: 'server_missing', retryable: false, serverMissing: true};
		}
		try {
			const participant = await server.roomServiceClient.getParticipant(roomName, identity);
			return {
				status: 'ok',
				participant: {joinedAt: Number(participant.joinedAt), metadata: participant.metadata},
			};
		} catch (error) {
			if (LiveKitService.isHttp404(error)) {
				return {status: 'ok', participant: null};
			}
			Logger.warn(
				{error, regionId, serverId, roomName, participantIdentity: identity},
				'LiveKit getParticipant failed',
			);
			return LiveKitService.lookupError(error);
		}
	}

	async setParticipantPermission(params: SetParticipantPermissionParams): Promise<void> {
		const {regionId, serverId, roomName, identity, permission} = params;
		const server = this.tryResolveServerClient(regionId, serverId);
		if (server === null) {
			return;
		}
		try {
			await server.roomServiceClient.updateParticipant(roomName, identity, undefined, permission);
		} catch (error) {
			if (LiveKitService.isHttp404(error)) {
				return;
			}
			Logger.error({error, participantIdentity: identity, roomName}, 'Error updating LiveKit participant permission');
		}
	}

	async listRooms(params: ListRoomsParams): Promise<ListRoomsResult> {
		const {regionId, serverId} = params;
		const server = this.tryResolveServerClient(regionId, serverId);
		if (server === null) {
			return {status: 'error', errorCode: 'server_missing', retryable: false, serverMissing: true};
		}
		try {
			const rooms = await server.roomServiceClient.listRooms();
			return {
				status: 'ok',
				rooms: rooms.map((room) => ({name: room.name, numParticipants: room.numParticipants})),
			};
		} catch (error) {
			Logger.warn({error, regionId, serverId}, 'LiveKit listRooms failed');
			return LiveKitService.lookupError(error);
		}
	}

	async listParticipants(params: ListParticipantsParams): Promise<ListParticipantsResult> {
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
				participants: participants.map((participant) => ({
					identity: participant.identity,
					joinedAt: Number(participant.joinedAt),
					metadata: participant.metadata,
				})),
			};
		} catch (error) {
			Logger.warn({error, regionId, serverId, roomName}, 'LiveKit listParticipants failed');
			return LiveKitService.lookupError(error);
		}
	}

	private static lookupError(error: unknown): {status: 'error'; errorCode: string; retryable: boolean} {
		const status = LiveKitService.getHttpStatus(error);
		return {
			status: 'error',
			errorCode: error instanceof Error ? error.message : 'unknown',
			retryable: status != null && status >= 500,
		};
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
		return this.serverClients.get(regionId)?.get(serverId) ?? null;
	}

	listServers(): Array<{regionId: string; serverId: string}> {
		const servers: Array<{regionId: string; serverId: string}> = [];
		for (const [regionId, regionServers] of this.serverClients) {
			for (const serverId of regionServers.keys()) {
				servers.push({regionId, serverId});
			}
		}
		return servers;
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
					roomServiceClient: createRoomServiceClient(roomServiceEndpoint, server.apiKey, server.apiSecret),
				});
			}
			newMap.set(region.id, serverMap);
		}
		this.serverClients = newMap;
	}
}
