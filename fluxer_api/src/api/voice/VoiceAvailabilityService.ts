// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildID, UserID} from '../BrandedTypes';
import type {VoiceRegionAvailability, VoiceRegionMetadata, VoiceRegionRecord, VoiceServerRecord} from './VoiceModel';
import {preferServersUnderSoftLimit} from './VoiceRegionSelection';
import type {VoiceServerLoadSource} from './VoiceServerLoad';
import type {VoiceTopology} from './VoiceTopology';

export interface VoiceAccessContext {
	requestingUserId: UserID;
	guildId?: GuildID;
	guildFeatures?: Set<string>;
}

const EMPTY_CONNECTION_COUNTS: ReadonlyMap<string, number> = new Map();

export class VoiceAvailabilityService {
	private rotationIndex: Map<string, number> = new Map();

	constructor(
		private topology: VoiceTopology,
		private loadSource: VoiceServerLoadSource | null = null,
	) {}

	getServerConnectionCounts(): ReadonlyMap<string, number> {
		return this.loadSource?.getConnectionCounts() ?? EMPTY_CONNECTION_COUNTS;
	}

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

	getAccessibleServersForRegion(regionId: string, context: VoiceAccessContext): Array<VoiceServerRecord> {
		const servers = this.topology.getServersForRegion(regionId);
		return servers.filter((server) => this.isServerAccessible(server, context));
	}

	selectServer(regionId: string, context: VoiceAccessContext): VoiceServerRecord | null {
		const accessibleServers = this.getAccessibleServersForRegion(regionId, context).sort((left, right) => {
			if (left.serverId < right.serverId) {
				return -1;
			}
			if (left.serverId > right.serverId) {
				return 1;
			}
			return 0;
		});
		if (accessibleServers.length === 0) {
			return null;
		}
		const candidateServers = preferServersUnderSoftLimit(accessibleServers, this.getServerConnectionCounts());
		const index = this.rotationIndex.get(regionId) ?? 0;
		const server = candidateServers[index % candidateServers.length];
		this.rotationIndex.set(regionId, (index + 1) % candidateServers.length);
		return server;
	}

	resetRotation(regionId: string): void {
		this.rotationIndex.delete(regionId);
	}
}
