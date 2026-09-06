// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '../BrandedTypes';
import type {VoiceRegionMetadata, VoiceServerRecord} from '../voice/VoiceModel';
import type {ILiveKitService, ListActiveRoomsResult, ListParticipantsResult} from './ILiveKitService';

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
	deaf?: boolean;
}

export class DisabledLiveKitService implements ILiveKitService {
	async createToken(_params: CreateTokenParams): Promise<{
		token: string;
		endpoint: string;
	}> {
		throw new Error('Voice is disabled');
	}

	async updateParticipant(_params: UpdateParticipantParams): Promise<void> {}

	async updateParticipantPermissions(_params: UpdateParticipantPermissionsParams): Promise<void> {}

	async disconnectParticipant(_params: DisconnectParticipantParams): Promise<void> {}

	async listParticipants(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
		regionId: string;
		serverId: string;
	}): Promise<ListParticipantsResult> {
		return {status: 'ok', participants: []};
	}

	async listActiveRooms(): Promise<ListActiveRoomsResult> {
		return {rooms: [], errors: [], searchedServers: 0, completed: true};
	}

	getDefaultRegionId(): string | null {
		return null;
	}

	getRegionMetadata(): Array<VoiceRegionMetadata> {
		return [];
	}

	getServer(_regionId: string, _serverId: string): VoiceServerRecord | null {
		return null;
	}
}
