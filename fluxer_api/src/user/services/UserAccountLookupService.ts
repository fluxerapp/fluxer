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
import {ChannelTypes, RelationshipTypes, UserFlags, UserPremiumTypes} from '~/Constants';
import {MissingAccessError, UnknownUserError} from '~/Errors';
import type {GuildMemberResponse} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {GuildService} from '~/guild/services/GuildService';
import type {IDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {GuildMember, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserChannelRepository} from '../repositories/IUserChannelRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';

interface UserAccountLookupServiceDeps {
	userAccountRepository: IUserAccountRepository;
	userChannelRepository: IUserChannelRepository;
	userRelationshipRepository: IUserRelationshipRepository;
	guildRepository: IGuildRepository;
	guildService: GuildService;
	discriminatorService: IDiscriminatorService;
}

export class UserAccountLookupService {
	constructor(private readonly deps: UserAccountLookupServiceDeps) {}

	async findUnique(userId: UserID): Promise<User | null> {
		return await this.deps.userAccountRepository.findUnique(userId);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return await this.deps.userAccountRepository.findUniqueAssert(userId);
	}

	async getUserProfile(params: {
		userId: UserID;
		targetId: UserID;
		guildId?: GuildID;
		withMutualFriends?: boolean;
		withMutualGuilds?: boolean;
		requestCache: RequestCache;
	}): Promise<{
		user: User;
		guildMember?: GuildMemberResponse | null;
		guildMemberDomain?: GuildMember | null;
		premiumType?: number;
		premiumSince?: Date;
		premiumLifetimeSequence?: number;
		mutualFriends?: Array<User>;
		mutualGuilds?: Array<{id: string; nick: string | null}>;
	}> {
		const {userId, targetId, guildId, withMutualFriends, withMutualGuilds, requestCache} = params;
		const user = await this.deps.userAccountRepository.findUnique(targetId);
		if (!user) throw new UnknownUserError();

		if (userId !== targetId) {
			await this.validateProfileAccess(userId, targetId, user);
		}

		let guildMember: GuildMemberResponse | null = null;
		let guildMemberDomain: GuildMember | null = null;

		if (guildId != null) {
			guildMemberDomain = await this.deps.guildRepository.getMember(guildId, targetId);
			if (guildMemberDomain) {
				guildMember = await this.deps.guildService.getMember({
					userId,
					targetId,
					guildId,
					requestCache,
				});
			}
		}

		let premiumType = user.premiumType ?? undefined;
		let premiumSince = user.premiumSince ?? undefined;
		let premiumLifetimeSequence = user.premiumLifetimeSequence ?? undefined;

		if (user.flags & UserFlags.PREMIUM_BADGE_HIDDEN) {
			premiumType = undefined;
			premiumSince = undefined;
			premiumLifetimeSequence = undefined;
		} else {
			if (user.premiumType === UserPremiumTypes.LIFETIME) {
				if (user.flags & UserFlags.PREMIUM_BADGE_MASKED) {
					premiumType = UserPremiumTypes.SUBSCRIPTION;
				}
				if (user.flags & UserFlags.PREMIUM_BADGE_SEQUENCE_HIDDEN) {
					premiumLifetimeSequence = undefined;
				}
			}
			if (user.flags & UserFlags.PREMIUM_BADGE_TIMESTAMP_HIDDEN) {
				premiumSince = undefined;
			}
		}

		let mutualFriends: Array<User> | undefined;
		if (withMutualFriends && userId !== targetId) {
			mutualFriends = await this.getMutualFriends(userId, targetId);
		}

		let mutualGuilds: Array<{id: string; nick: string | null}> | undefined;
		if (withMutualGuilds && userId !== targetId) {
			mutualGuilds = await this.getMutualGuilds(userId, targetId);
		}

		return {
			user,
			guildMember,
			guildMemberDomain,
			premiumType,
			premiumSince,
			premiumLifetimeSequence,
			mutualFriends,
			mutualGuilds,
		};
	}

	private async validateProfileAccess(userId: UserID, targetId: UserID, targetUser: User): Promise<void> {
		if (targetUser.isBot) {
			return;
		}

		const friendship = await this.deps.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.FRIEND,
		);
		if (friendship) {
			return;
		}

		const incomingRequest = await this.deps.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.INCOMING_REQUEST,
		);
		if (incomingRequest) {
			return;
		}

		const [userGuildIds, targetGuildIds] = await Promise.all([
			this.deps.userAccountRepository.getUserGuildIds(userId),
			this.deps.userAccountRepository.getUserGuildIds(targetId),
		]);

		const userGuildIdSet = new Set(userGuildIds.map((id) => id.toString()));
		const hasMutualGuild = targetGuildIds.some((id) => userGuildIdSet.has(id.toString()));
		if (hasMutualGuild) {
			return;
		}

		if (await this.hasSharedGroupDm(userId, targetId)) {
			return;
		}

		throw new MissingAccessError();
	}

	private async hasSharedGroupDm(userId: UserID, targetId: UserID): Promise<boolean> {
		const privateChannels = await this.deps.userChannelRepository.listPrivateChannels(userId);
		return privateChannels.some(
			(channel) => channel.type === ChannelTypes.GROUP_DM && channel.recipientIds.has(targetId),
		);
	}

	private async getMutualFriends(userId: UserID, targetId: UserID): Promise<Array<User>> {
		const [userRelationships, targetRelationships] = await Promise.all([
			this.deps.userRelationshipRepository.listRelationships(userId),
			this.deps.userRelationshipRepository.listRelationships(targetId),
		]);

		const userFriendIds = new Set(
			userRelationships
				.filter((rel) => rel.type === RelationshipTypes.FRIEND)
				.map((rel) => rel.targetUserId.toString()),
		);

		const mutualFriendIds = targetRelationships
			.filter((rel) => rel.type === RelationshipTypes.FRIEND && userFriendIds.has(rel.targetUserId.toString()))
			.map((rel) => rel.targetUserId);

		if (mutualFriendIds.length === 0) {
			return [];
		}

		const users = await this.deps.userAccountRepository.listUsers(mutualFriendIds);

		return users.sort((a, b) => this.compareUsersByIdDesc(a, b));
	}

	private compareUsersByIdDesc(a: User, b: User): number {
		if (b.id > a.id) return 1;
		if (b.id < a.id) return -1;
		return 0;
	}

	private async getMutualGuilds(userId: UserID, targetId: UserID): Promise<Array<{id: string; nick: string | null}>> {
		const [userGuildIds, targetGuildIds] = await Promise.all([
			this.deps.userAccountRepository.getUserGuildIds(userId),
			this.deps.userAccountRepository.getUserGuildIds(targetId),
		]);

		const userGuildIdSet = new Set(userGuildIds.map((id) => id.toString()));
		const mutualGuildIds = targetGuildIds.filter((id) => userGuildIdSet.has(id.toString()));

		if (mutualGuildIds.length === 0) {
			return [];
		}

		const memberPromises = mutualGuildIds.map((guildId) => this.deps.guildRepository.getMember(guildId, targetId));
		const members = await Promise.all(memberPromises);

		return mutualGuildIds.map((guildId, index) => ({
			id: guildId.toString(),
			nick: members[index]?.nickname ?? null,
		}));
	}

	async generateUniqueDiscriminator(username: string): Promise<number> {
		const usedDiscriminators = await this.deps.userAccountRepository.findDiscriminatorsByUsername(username);
		for (let i = 1; i <= 9999; i++) {
			if (!usedDiscriminators.has(i)) return i;
		}
		throw new Error('No available discriminators for this username');
	}

	async checkUsernameDiscriminatorAvailability(params: {username: string; discriminator: number}): Promise<boolean> {
		const {username, discriminator} = params;
		const isAvailable = await this.deps.discriminatorService.isDiscriminatorAvailableForUsername(
			username,
			discriminator,
		);
		return !isAvailable;
	}
}
