// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {VoiceRegionMetadata, VoiceServerRecord} from '@app/api/voice/VoiceModel';

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

interface DisconnectParticipantParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
}

interface ListParticipantsParams {
	guildId?: GuildID;
	channelId: ChannelID;
	regionId: string;
	serverId: string;
}

export interface ListParticipantsSuccess {
	status: 'ok';
	participants: Array<{
		identity: string;
	}>;
}

export interface ListParticipantsError {
	status: 'error';
	errorCode: string;
	retryable: boolean;
	serverMissing?: boolean;
}

export type ListParticipantsResult = ListParticipantsSuccess | ListParticipantsError;

export abstract class ILiveKitService {
	abstract createToken(params: CreateTokenParams): Promise<{
		token: string;
		endpoint: string;
	}>;

	abstract updateParticipant(params: UpdateParticipantParams): Promise<void>;

	abstract updateParticipantPermissions(params: UpdateParticipantPermissionsParams): Promise<void>;

	abstract disconnectParticipant(params: DisconnectParticipantParams): Promise<void>;

	abstract listParticipants(params: ListParticipantsParams): Promise<ListParticipantsResult>;

	abstract getDefaultRegionId(): string | null;

	abstract getRegionMetadata(): Array<VoiceRegionMetadata>;

	abstract getServer(regionId: string, serverId: string): VoiceServerRecord | null;
}
