// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GuildOperationsService} from '@app/api/guild/services/data/GuildOperationsService';
import type {GuildMemberService} from '@app/api/guild/services/GuildMemberService';
import {
	type InstanceConfigRepository,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';

const INELIGIBLE_FLAGS = UserFlags.DELETED | UserFlags.SELF_DELETED | UserFlags.DISABLED;

export function canJoinSingleCommunity(user: User): boolean {
	return (
		user.id !== SYSTEM_USER_ID &&
		!user.isBot &&
		!user.isSystem &&
		(user.flags & INELIGIBLE_FLAGS) === 0n &&
		user.pendingDeletionAt === null &&
		!user.traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT) &&
		!user.traits.has(REGISTRATION_REJECTED_TRAIT)
	);
}

export class SingleCommunityService {
	constructor(
		private readonly instanceConfigRepository: InstanceConfigRepository,
		private readonly guildOperations: GuildOperationsService,
		private readonly guildMemberService: GuildMemberService,
	) {}

	async getStockCommunityId(): Promise<GuildID | null> {
		const policy = await this.instanceConfigRepository.getInstancePolicyConfig();
		if (!policy.single_community_enabled || policy.single_community_guild_id === null) {
			return null;
		}
		return createGuildID(BigInt(policy.single_community_guild_id));
	}

	async joinStockCommunity(userId: UserID, requestCache: RequestCache): Promise<GuildID | null> {
		const guildId = await this.getStockCommunityId();
		if (guildId === null) {
			return null;
		}
		try {
			await this.guildMemberService.addUserToGuild({
				userId,
				guildId,
				skipGuildLimitCheck: true,
				skipAccountLimitCheck: true,
				joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
				requestCache,
			});
			return guildId;
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), guildId: guildId.toString(), error},
				'Failed to auto-join stock community',
			);
			return null;
		}
	}

	async ensureStockCommunity(params: {owner: User | null; name: string; guildId?: GuildID | null}): Promise<GuildID> {
		const policy = await this.instanceConfigRepository.getInstancePolicyConfig();
		const designatedGuildId = await this.findCommunity(params.guildId?.toString() ?? policy.single_community_guild_id);
		if (designatedGuildId != null) {
			await this.instanceConfigRepository.setInstancePolicyConfig({
				single_community_enabled: true,
				single_community_guild_id: designatedGuildId.toString(),
			});
			return designatedGuildId;
		}
		if (params.owner === null) {
			throw new UnknownGuildError();
		}
		return this.createStockCommunity({owner: params.owner, name: params.name});
	}

	async findCommunity(rawGuildId: string | null): Promise<GuildID | null> {
		if (rawGuildId === null) {
			return null;
		}
		const guildId = createGuildID(BigInt(rawGuildId));
		try {
			await this.guildOperations.getGuildSystem(guildId);
			return guildId;
		} catch (error) {
			if (error instanceof UnknownGuildError) return null;
			throw error;
		}
	}

	private async createStockCommunity(params: {owner: User; name: string}): Promise<GuildID> {
		const guild = await this.guildOperations.createGuild({user: params.owner, data: {name: params.name}});
		const guildId = createGuildID(BigInt(guild.id));
		await this.instanceConfigRepository.setInstancePolicyConfig({
			single_community_enabled: true,
			single_community_guild_id: guildId.toString(),
		});
		return guildId;
	}
}
