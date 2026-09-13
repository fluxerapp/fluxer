// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import type {GuildDataService} from '@app/api/guild/services/GuildDataService';
import type {GuildMemberService} from '@app/api/guild/services/GuildMemberService';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';

export class SingleCommunityService {
	constructor(
		private readonly instanceConfigRepository: InstanceConfigRepository,
		private readonly guildDataService: GuildDataService,
		private readonly guildMemberService: GuildMemberService,
	) {}

	async getStockCommunityId(): Promise<GuildID | null> {
		const policy = await this.instanceConfigRepository.getInstancePolicyConfig();
		if (!policy.single_community_enabled || policy.single_community_guild_id === null) {
			return null;
		}
		return createGuildID(BigInt(policy.single_community_guild_id));
	}

	async joinStockCommunity(userId: UserID, requestCache: RequestCache): Promise<void> {
		const guildId = await this.getStockCommunityId();
		if (guildId === null) {
			return;
		}
		try {
			await this.guildMemberService.addUserToGuild({
				skipRiskGate: true,
				userId,
				guildId,
				skipGuildLimitCheck: true,
				joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
				requestCache,
			});
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), guildId: guildId.toString(), error},
				'Failed to auto-join stock community',
			);
		}
	}

	async ensureStockCommunity(params: {owner: User; name: string}): Promise<GuildID> {
		const policy = await this.instanceConfigRepository.getInstancePolicyConfig();
		const designatedGuildId = await this.findDesignatedGuild(policy.single_community_guild_id);
		if (designatedGuildId != null) {
			await this.instanceConfigRepository.setInstancePolicyConfig({
				single_community_enabled: true,
				single_community_guild_id: designatedGuildId.toString(),
			});
			return designatedGuildId;
		}
		return this.createStockCommunity(params);
	}

	private async findDesignatedGuild(rawGuildId: string | null): Promise<GuildID | null> {
		if (rawGuildId === null) {
			return null;
		}
		const guildId = createGuildID(BigInt(rawGuildId));
		try {
			await this.guildDataService.getGuildSystem(guildId);
			return guildId;
		} catch (error) {
			if (error instanceof UnknownGuildError) return null;
			throw error;
		}
	}

	async createStockCommunity(params: {owner: User; name: string}): Promise<GuildID> {
		const guild = await this.guildDataService.createGuild({user: params.owner, data: {name: params.name}});
		const guildId = createGuildID(BigInt(guild.id));
		await this.instanceConfigRepository.setInstancePolicyConfig({
			single_community_enabled: true,
			single_community_guild_id: guildId.toString(),
		});
		return guildId;
	}
}
