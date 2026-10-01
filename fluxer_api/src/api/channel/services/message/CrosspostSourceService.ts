// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, type GuildID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {mapGuildToPartialResponse} from '@app/api/guild/GuildModel';
import type {IGuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {IGuildDataRepository} from '@app/api/guild/repositories/IGuildDataRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import {MessageFlags, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {CrosspostSourceResponse} from '@fluxer/schema/src/domains/message/CrosspostSourceSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const CROSSPOST_SOURCE_COUNTS_TTL_SECONDS = 60;

const CROSSPOST_SOURCE_PUBLIC_FEATURES = new Set<string>([
	GuildFeatures.VERIFIED,
	GuildFeatures.PARTNERED,
	GuildFeatures.DISCOVERABLE,
]);

interface CrosspostSourceCounts {
	memberCount: number;
	presenceCount: number;
}

export function crosspostSourceCountsCacheKey(guildId: GuildID): string {
	return `crosspost-source:counts:${guildId.toString()}`;
}

export function getCrosspostSourceGuildId(message: MessageResponse): GuildID | null {
	const isCopy = (message.flags & MessageFlags.IS_CROSSPOST) !== 0;
	const isFollowNotice = message.type === MessageTypes.CHANNEL_FOLLOW_ADD;
	if (!isCopy && !isFollowNotice) {
		return null;
	}
	const guildId = message.message_reference?.guild_id;
	return guildId ? createGuildID(BigInt(guildId)) : null;
}

export class CrosspostSourceService {
	constructor(
		private readonly guildRepository: IGuildDataRepository,
		private readonly discoveryRepository: IGuildDiscoveryRepository,
		private readonly gatewayService: IGatewayService,
		private readonly cacheService: ICacheService,
	) {}

	async getSource(message: MessageResponse): Promise<CrosspostSourceResponse> {
		const guildId = getCrosspostSourceGuildId(message);
		if (guildId === null) {
			throw new UnknownMessageError();
		}
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const partial = mapGuildToPartialResponse(guild);
		const isListed = Config.discovery.enabled && guild.features.has(GuildFeatures.DISCOVERABLE);
		const [counts, description] = await Promise.all([
			this.getCounts(guildId),
			isListed ? this.getListedDescription(guildId) : null,
		]);
		return {
			guild: {
				id: partial.id,
				name: partial.name,
				icon: partial.icon ?? null,
				banner: partial.banner ?? null,
				features: partial.features.filter((feature) => CROSSPOST_SOURCE_PUBLIC_FEATURES.has(feature)),
				approximate_member_count: counts?.memberCount ?? null,
				approximate_presence_count: counts?.presenceCount ?? null,
				description,
				discoverable: isListed && !guild.features.has(GuildFeatures.INVITES_DISABLED),
			},
		};
	}

	private async getListedDescription(guildId: GuildID): Promise<string | null> {
		const row = await this.discoveryRepository.findByGuildId(guildId);
		if (row?.status !== DiscoveryApplicationStatus.APPROVED) {
			return null;
		}
		return row.description || null;
	}

	private async getCounts(guildId: GuildID): Promise<CrosspostSourceCounts | null> {
		try {
			return await this.cacheService.getOrSet<CrosspostSourceCounts>(
				crosspostSourceCountsCacheKey(guildId),
				() => this.gatewayService.getGuildCounts(guildId),
				CROSSPOST_SOURCE_COUNTS_TTL_SECONDS,
			);
		} catch (error) {
			Logger.warn({error, guildId: guildId.toString()}, 'Failed to load crosspost source community counts');
			return null;
		}
	}
}
