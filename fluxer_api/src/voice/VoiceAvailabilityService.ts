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
import {GuildFeatures} from '~/Constants';
import type {
	VoiceRegionAvailability,
	VoiceRegionMetadata,
	VoiceRegionRecord,
	VoiceServerRecord,
} from '~/voice/VoiceModel';
import type {VoiceTopology} from '~/voice/VoiceTopology';

export interface VoiceAccessContext {
	requestingUserId: UserID;
	guildId?: GuildID;
	guildFeatures?: Set<string>;
}

export class VoiceAvailabilityService {
	private rotationIndex: Map<string, number> = new Map();

	constructor(private topology: VoiceTopology) {}

	getRegionMetadata(): Array<VoiceRegionMetadata> {
		return this.topology.getRegionMetadataList();
	}

	isRegionAccessible(region: VoiceRegionRecord, context: VoiceAccessContext): boolean {
		const {restrictions} = region;

		if (restrictions.allowedUserIds.size > 0 && !restrictions.allowedUserIds.has(context.requestingUserId)) {
			return false;
		}

		const hasAllowedGuildIds = restrictions.allowedGuildIds.size > 0;
		const hasRequiredGuildFeatures = restrictions.requiredGuildFeatures.size > 0;
		const hasVipOnly = restrictions.vipOnly;

		if (!hasAllowedGuildIds && !hasRequiredGuildFeatures && !hasVipOnly) {
			return true;
		}

		if (!context.guildId) {
			return false;
		}

		const isGuildAllowed = hasAllowedGuildIds && restrictions.allowedGuildIds.has(context.guildId);

		if (isGuildAllowed) {
			return true;
		}

		if (!hasRequiredGuildFeatures && !hasVipOnly) {
			return !hasAllowedGuildIds;
		}

		if (!context.guildFeatures) {
			return false;
		}

		if (hasVipOnly && !context.guildFeatures.has(GuildFeatures.VIP_VOICE)) {
			return false;
		}

		if (hasRequiredGuildFeatures) {
			for (const feature of restrictions.requiredGuildFeatures) {
				if (context.guildFeatures.has(feature)) {
					return true;
				}
			}
			return false;
		}

		return true;
	}

	isServerAccessible(server: VoiceServerRecord, context: VoiceAccessContext): boolean {
		const {restrictions} = server;

		if (!server.isActive) {
			return false;
		}

		if (restrictions.allowedUserIds.size > 0 && !restrictions.allowedUserIds.has(context.requestingUserId)) {
			return false;
		}

		const hasAllowedGuildIds = restrictions.allowedGuildIds.size > 0;
		const hasRequiredGuildFeatures = restrictions.requiredGuildFeatures.size > 0;
		const hasVipOnly = restrictions.vipOnly;

		if (!hasAllowedGuildIds && !hasRequiredGuildFeatures && !hasVipOnly) {
			return true;
		}

		if (!context.guildId) {
			return false;
		}

		const isGuildAllowed = hasAllowedGuildIds && restrictions.allowedGuildIds.has(context.guildId);

		if (isGuildAllowed) {
			return true;
		}

		if (!hasRequiredGuildFeatures && !hasVipOnly) {
			return !hasAllowedGuildIds;
		}

		if (!context.guildFeatures) {
			return false;
		}

		if (hasVipOnly && !context.guildFeatures.has(GuildFeatures.VIP_VOICE)) {
			return false;
		}

		if (hasRequiredGuildFeatures) {
			for (const feature of restrictions.requiredGuildFeatures) {
				if (context.guildFeatures.has(feature)) {
					return true;
				}
			}
			return false;
		}

		return true;
	}

	getAvailableRegions(context: VoiceAccessContext): Array<VoiceRegionAvailability> {
		const regions = this.topology.getAllRegions();
		return regions.map<VoiceRegionAvailability>((region) => {
			const servers = this.topology.getServersForRegion(region.id);
			const accessibleServers = servers.filter((server) => this.isServerAccessible(server, context));
			const regionAccessible = this.isRegionAccessible(region, context);

			return {
				id: region.id,
				name: region.name,
				emoji: region.emoji,
				latitude: region.latitude,
				longitude: region.longitude,
				isDefault: region.isDefault,
				vipOnly: region.restrictions.vipOnly,
				requiredGuildFeatures: Array.from(region.restrictions.requiredGuildFeatures),
				serverCount: servers.length,
				activeServerCount: accessibleServers.length,
				isAccessible: regionAccessible && accessibleServers.length > 0,
				restrictions: region.restrictions,
			};
		});
	}

	selectServer(regionId: string, context: VoiceAccessContext): VoiceServerRecord | null {
		const servers = this.topology.getServersForRegion(regionId);
		if (servers.length === 0) {
			return null;
		}

		const accessibleServers = servers.filter((server) => this.isServerAccessible(server, context));
		if (accessibleServers.length === 0) {
			return null;
		}

		const index = this.rotationIndex.get(regionId) ?? 0;
		const server = accessibleServers[index % accessibleServers.length];
		this.rotationIndex.set(regionId, (index + 1) % accessibleServers.length);

		return server;
	}

	resetRotation(regionId: string): void {
		this.rotationIndex.delete(regionId);
	}
}
