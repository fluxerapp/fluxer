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

import type {GuildID, UserID} from '~/BrandedTypes';

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
	isActive: boolean;
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
