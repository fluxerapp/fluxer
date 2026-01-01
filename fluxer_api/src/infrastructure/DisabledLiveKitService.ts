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

import type {ChannelID, GuildID, UserID} from '~/BrandedTypes';
import type {VoiceRegionMetadata, VoiceServerRecord} from '~/voice/VoiceModel';
import type {ILiveKitService} from './ILiveKitService';

interface CreateTokenParams {
	userId: UserID;
	guildId?: GuildID;
	channelId: ChannelID;
	connectionId: string;
	regionId: string;
	serverId: string;
	mute?: boolean;
	deaf?: boolean;
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

export class DisabledLiveKitService implements ILiveKitService {
	async createToken(_params: CreateTokenParams): Promise<{token: string; endpoint: string}> {
		throw new Error('Voice is disabled');
	}

	async updateParticipant(_params: UpdateParticipantParams): Promise<void> {}

	async updateParticipantPermissions(_params: UpdateParticipantPermissionsParams): Promise<void> {}

	async disconnectParticipant(_params: DisconnectParticipantParams): Promise<void> {}

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
