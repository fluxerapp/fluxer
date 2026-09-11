// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '../BrandedTypes';

interface VoiceRestriction {
	vipOnly: boolean;
	requiredGuildFeatures: Set<string>;
	allowedGuildIds: Set<GuildID>;
	allowedUserIds: Set<UserID>;
}

export interface VoiceRegionRecord {
	id: string;
	name: string;
	emoji: string;
	latitude: number;
	longitude: number;
	isDefault: boolean;
	restrictions: VoiceRestriction;
	createdAt: Date | null;
	updatedAt: Date | null;
}

export interface VoiceServerRecord {
	regionId: string;
	serverId: string;
	endpoint: string;
	apiKey: string;
	apiSecret: string;
	latitude: number | null;
	longitude: number | null;
	isActive: boolean;
	softConnectionLimit: number | null;
	restrictions: VoiceRestriction;
	createdAt: Date | null;
	updatedAt: Date | null;
}

export interface VoiceRegionWithServers extends VoiceRegionRecord {
	servers: Array<VoiceServerRecord>;
}

export interface VoiceRegionMetadata {
	id: string;
	name: string;
	emoji: string;
	latitude: number;
	longitude: number;
	isDefault: boolean;
	vipOnly: boolean;
	requiredGuildFeatures: Array<string>;
}

export interface VoiceRegionAvailability extends VoiceRegionMetadata {
	isAccessible: boolean;
	restrictions: VoiceRestriction;
	serverCount: number;
	activeServerCount: number;
}
