// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {VoiceRegionAvailability, VoiceRegionRecord, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {preferServersUnderSoftLimit} from '@app/api/voice/VoiceRegionSelection';
import type {VoiceServerHealthSource} from '@app/api/voice/VoiceServerHealth';
import type {VoiceServerLoadSource} from '@app/api/voice/VoiceServerLoad';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';

export interface VoiceAccessContext {
	requestingUserId: UserID;
	guildId?: GuildID;
	guildFeatures?: Set<string>;
}

const EMPTY_CONNECTION_COUNTS: ReadonlyMap<string, number> = new Map();

function meetsVoiceRestrictions(restrictions: VoiceRegionRecord['restrictions'], context: VoiceAccessContext): boolean {
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

export class VoiceAvailabilityService {
	private rotationIndex: Map<string, number> = new Map();

	constructor(
		private topology: VoiceTopology,
		private loadSource: VoiceServerLoadSource | null = null,
		private healthSource: VoiceServerHealthSource | null = null,
	) {}

	isServerDown(server: VoiceServerRecord): boolean {
		return this.healthSource?.get(server).status === 'down';
	}

	isServerPlaceable(server: VoiceServerRecord): boolean {
		return !server.draining && !this.isServerDown(server);
	}

	preferPlaceableServers(servers: Array<VoiceServerRecord>): Array<VoiceServerRecord> {
		const open = servers.filter((server) => !server.draining);
		const up = open.filter((server) => !this.isServerDown(server));
		if (up.length > 0) return up;
		if (open.length > 0) return open;
		return servers;
	}

	getServerConnectionCounts(): ReadonlyMap<string, number> {
		return this.loadSource?.getConnectionCounts() ?? EMPTY_CONNECTION_COUNTS;
	}

	isServerAccessible(server: VoiceServerRecord, context: VoiceAccessContext): boolean {
		const {restrictions} = server;
		if (!server.isActive) {
			return false;
		}
		return meetsVoiceRestrictions(restrictions, context);
	}

	getAvailableRegions(context: VoiceAccessContext): Array<VoiceRegionAvailability> {
		const regions = this.topology.getAllRegions();
		return regions.map<VoiceRegionAvailability>((region) => {
			const servers = this.topology.getServersForRegion(region.id);
			const accessibleServers = servers.filter((server) => this.isServerAccessible(server, context));
			const regionAccessible = meetsVoiceRestrictions(region.restrictions, context);
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
		const candidateServers = preferServersUnderSoftLimit(
			this.preferPlaceableServers(accessibleServers),
			this.getServerConnectionCounts(),
		);
		const index = this.rotationIndex.get(regionId) ?? 0;
		const server = candidateServers[index % candidateServers.length];
		this.rotationIndex.set(regionId, (index + 1) % candidateServers.length);
		return server;
	}

	selectServerOrFallback(
		regionId: string,
		context: VoiceAccessContext,
		accessibleRegions: Array<VoiceRegionAvailability>,
	): {regionId: string; server: VoiceServerRecord} | null {
		const initialServer = this.selectServer(regionId, context);
		if (initialServer && this.isServerPlaceable(initialServer)) {
			return {regionId, server: initialServer};
		}
		let fallback: {regionId: string; server: VoiceServerRecord} | null = null;
		for (const region of accessibleRegions) {
			if (region.id === regionId) {
				continue;
			}
			const server = this.selectServer(region.id, context);
			if (!server) {
				continue;
			}
			if (this.isServerPlaceable(server)) {
				return {regionId: region.id, server};
			}
			fallback ??= {regionId: region.id, server};
		}
		if (initialServer) {
			return {regionId, server: initialServer};
		}
		return fallback;
	}
}
