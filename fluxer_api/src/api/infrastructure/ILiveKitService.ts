// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {VoiceRegionMetadata, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import type {ParticipantPermission} from 'livekit-server-sdk';

export interface CreateTokenParams {
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

export interface UpdateParticipantParams {
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

export interface MuteMicrophoneTrackParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
	trackSid: string;
}

export interface DisconnectParticipantParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
}

export interface LiveKitParticipantRef {
	regionId: string;
	serverId: string;
	roomName: string;
	identity: string;
}

export interface ListRoomsParams {
	regionId: string;
	serverId: string;
}

interface ListRoomsSuccess {
	status: 'ok';
	rooms: Array<{
		name: string;
		numParticipants: number;
	}>;
}

interface LiveKitLookupError {
	status: 'error';
	errorCode: string;
	retryable: boolean;
	serverMissing?: boolean;
}

export type ListRoomsResult = ListRoomsSuccess | LiveKitLookupError;

export interface ListParticipantsParams {
	guildId?: GuildID;
	channelId: ChannelID;
	regionId: string;
	serverId: string;
}

interface ListParticipantsSuccess {
	status: 'ok';
	participants: Array<{
		identity: string;
		joinedAt: number;
		metadata: string;
	}>;
}

export type ListParticipantsResult = ListParticipantsSuccess | LiveKitLookupError;

export interface SetParticipantPermissionParams extends LiveKitParticipantRef {
	permission: Partial<ParticipantPermission>;
}

interface GetParticipantSuccess {
	status: 'ok';
	participant: {
		joinedAt: number;
		metadata: string;
	} | null;
}

export type GetParticipantResult = GetParticipantSuccess | LiveKitLookupError;

export abstract class ILiveKitService {
	abstract createToken(params: CreateTokenParams): Promise<{
		token: string;
		endpoint: string;
	}>;

	abstract updateParticipant(params: UpdateParticipantParams): Promise<void>;

	abstract muteMicrophoneTrack(params: MuteMicrophoneTrackParams): Promise<void>;

	abstract disconnectParticipant(params: DisconnectParticipantParams): Promise<void>;

	abstract removeParticipant(params: LiveKitParticipantRef): Promise<void>;
	abstract getParticipant(params: LiveKitParticipantRef): Promise<GetParticipantResult>;
	abstract setParticipantPermission(params: SetParticipantPermissionParams): Promise<void>;

	abstract listRooms(params: ListRoomsParams): Promise<ListRoomsResult>;

	abstract listParticipants(params: ListParticipantsParams): Promise<ListParticipantsResult>;

	abstract listServers(): Array<{regionId: string; serverId: string}>;

	abstract getDefaultRegionId(): string | null;

	abstract getRegionMetadata(): Array<VoiceRegionMetadata>;

	abstract getServer(regionId: string, serverId: string): VoiceServerRecord | null;
}
