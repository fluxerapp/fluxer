// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	PremiumFlags,
	ProfileFieldPrivacyFlags,
	ProfilePrivacyLevels,
	RelationshipTypes,
	SMALL_GUILD_MEMBER_THRESHOLD,
	type UserPremiumType,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildID, UserID} from '../../BrandedTypes';
import type {IConnectionRepository} from '../../connection/IConnectionRepository';
import type {UserConnectionRow} from '../../database/types/ConnectionTypes';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '../../guild/services/GuildService';
import type {IDiscriminatorService} from '../../infrastructure/DiscriminatorService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {GuildMember} from '../../models/GuildMember';
import type {User} from '../../models/User';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserChannelRepository} from '../repositories/IUserChannelRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '../repositories/IUserSettingsRepository';
import {canUseProfileTimezone} from '../UserHelpers';

interface ProfileFieldPrivacyContext {
	isOwner: boolean;
	isFriend: boolean;
	hasMutualGuild: boolean;
}

interface UserAccountLookupServiceDeps {
	userAccountRepository: IUserAccountRepository;
	userChannelRepository: IUserChannelRepository;
	userRelationshipRepository: IUserRelationshipRepository;
	userSettingsRepository: IUserSettingsRepository;
	guildRepository: IGuildRepositoryAggregate;
	guildService: GuildService;
	discriminatorService: IDiscriminatorService;
	connectionRepository: IConnectionRepository;
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
		premiumType?: UserPremiumType;
		premiumSince?: Date;
		premiumLifetimeSequence?: number;
		mutualFriends?: Array<User>;
		mutualGuilds?: Array<{
			id: string;
			nick: string | null;
		}>;
		connections?: Array<UserConnectionRow>;
		timezoneVisible: boolean;
		restrictProfile: boolean;
	}> {
		const {userId, targetId, guildId, withMutualFriends, withMutualGuilds, requestCache} = params;
		const user = await this.deps.userAccountRepository.findUnique(targetId);
		if (!user) throw new UnknownUserError();
		if (userId !== targetId) {
			await this.validateProfileAccess(userId, targetId, user);
		}
		const restrictProfile = await this.shouldRestrictProfile(userId, targetId, user);
		let guildMember: GuildMemberResponse | null = null;
		let guildMemberDomain: GuildMember | null = null;
		if (guildId != null) {
			const viewerMember = await this.deps.guildRepository.getMember(guildId, userId);
			if (viewerMember) {
				guildMemberDomain = await this.deps.guildRepository.getMember(guildId, targetId);
				if (guildMemberDomain) {
					guildMember = await this.deps.guildService.members.getMember({
						userId,
						targetId,
						guildId,
						requestCache,
					});
				}
			}
		}
		let premiumType = user.premiumType ?? undefined;
		let premiumSince = user.premiumSince ?? undefined;
		let premiumLifetimeSequence = user.premiumLifetimeSequence ?? undefined;
		if (restrictProfile || user.premiumFlags & PremiumFlags.BADGE_HIDDEN) {
			premiumType = undefined;
			premiumSince = undefined;
			premiumLifetimeSequence = undefined;
		} else {
			if (user.premiumType === UserPremiumTypes.LIFETIME) {
				if (user.premiumFlags & PremiumFlags.BADGE_MASKED) {
					premiumType = UserPremiumTypes.SUBSCRIPTION;
				}
				if (user.premiumFlags & PremiumFlags.BADGE_SEQUENCE_HIDDEN) {
					premiumLifetimeSequence = undefined;
				}
			}
			if (user.premiumFlags & PremiumFlags.BADGE_TIMESTAMP_HIDDEN) {
				premiumSince = undefined;
			}
		}
		const profileFieldPrivacyContext = restrictProfile
			? null
			: await this.getProfileFieldPrivacyContext(userId, targetId);
		const timezoneVisible =
			!restrictProfile &&
			canUseProfileTimezone(user) &&
			user.timezone != null &&
			profileFieldPrivacyContext != null &&
			this.canViewProfileField(user.timezonePrivacyFlags, profileFieldPrivacyContext);
		const [mutualFriends, mutualGuilds, connections] = await Promise.all([
			withMutualFriends && userId !== targetId ? this.getMutualFriends(userId, targetId) : undefined,
			withMutualGuilds && userId !== targetId ? this.getMutualGuilds(userId, targetId) : undefined,
			restrictProfile || profileFieldPrivacyContext == null
				? Promise.resolve([] as Array<UserConnectionRow>)
				: this.getVisibleConnections(targetId, profileFieldPrivacyContext),
		]);
		return {
			user,
			guildMember,
			guildMemberDomain,
			premiumType,
			premiumSince,
			premiumLifetimeSequence,
			mutualFriends,
			mutualGuilds,
			connections,
			timezoneVisible,
			restrictProfile,
		};
	}

	private async shouldRestrictProfile(viewerId: UserID, targetId: UserID, targetUser: User): Promise<boolean> {
		if (viewerId === targetId) return false;
		if (targetUser.isBot) return false;
		const targetSettings = await this.deps.userSettingsRepository.findSettings(targetId);
		const level = targetSettings?.profilePrivacy ?? ProfilePrivacyLevels.ALL_GUILDS;
		if (level === ProfilePrivacyLevels.ALL_GUILDS) {
			if (await this.areFriends(viewerId, targetId)) return false;
			return !(await this.haveMutualGuild(viewerId, targetId));
		}
		if (await this.areFriends(viewerId, targetId)) return false;
		if (level === ProfilePrivacyLevels.FRIENDS_ONLY) return true;
		const sharedGuildIds = await this.getMutualGuildIds(viewerId, targetId);
		if (sharedGuildIds.length === 0) return true;
		const guilds = await Promise.all(sharedGuildIds.map((id) => this.deps.guildRepository.findUnique(id)));
		return !guilds.some((guild) => guild != null && guild.memberCount <= SMALL_GUILD_MEMBER_THRESHOLD);
	}

	private async getMutualGuildIds(userId1: UserID, userId2: UserID): Promise<Array<GuildID>> {
		const [user1GuildIds, user2GuildIds] = await Promise.all([
			this.deps.userAccountRepository.getUserGuildIds(userId1),
			this.deps.userAccountRepository.getUserGuildIds(userId2),
		]);
		const set = new Set(user1GuildIds.map((id) => id.toString()));
		return user2GuildIds.filter((id) => set.has(id.toString()));
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
		const outgoingRequest = await this.deps.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.OUTGOING_REQUEST,
		);
		if (outgoingRequest) {
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

	private async getMutualGuilds(
		userId: UserID,
		targetId: UserID,
	): Promise<
		Array<{
			id: string;
			nick: string | null;
		}>
	> {
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

	async checkUsernameDiscriminatorAvailability(params: {username: string; discriminator: number}): Promise<boolean> {
		const {username, discriminator} = params;
		const isAvailable = await this.deps.discriminatorService.isDiscriminatorAvailableForUsername(
			username,
			discriminator,
		);
		return !isAvailable;
	}

	private async getVisibleConnections(
		targetId: UserID,
		privacyContext: ProfileFieldPrivacyContext,
	): Promise<Array<UserConnectionRow>> {
		const connections = await this.deps.connectionRepository.findByUserId(targetId);
		const verified = connections.filter((connection) => connection.verified);
		return verified.filter((connection) => this.canViewProfileField(connection.visibility_flags, privacyContext));
	}

	private async getProfileFieldPrivacyContext(viewerId: UserID, targetId: UserID): Promise<ProfileFieldPrivacyContext> {
		if (viewerId === targetId) {
			return {isOwner: true, isFriend: true, hasMutualGuild: true};
		}
		const [isFriend, hasMutualGuild] = await Promise.all([
			this.areFriends(viewerId, targetId),
			this.haveMutualGuild(viewerId, targetId),
		]);
		return {isOwner: false, isFriend, hasMutualGuild};
	}

	private canViewProfileField(flags: number, privacyContext: ProfileFieldPrivacyContext): boolean {
		if (privacyContext.isOwner) {
			return flags !== 0;
		}
		if (flags & ProfileFieldPrivacyFlags.EVERYONE) {
			return true;
		}
		if (flags & ProfileFieldPrivacyFlags.FRIENDS && privacyContext.isFriend) {
			return true;
		}
		if (flags & ProfileFieldPrivacyFlags.MUTUAL_GUILDS && privacyContext.hasMutualGuild) {
			return true;
		}
		return false;
	}

	private async areFriends(userId1: UserID, userId2: UserID): Promise<boolean> {
		const friendship = await this.deps.userRelationshipRepository.getRelationship(
			userId1,
			userId2,
			RelationshipTypes.FRIEND,
		);
		return friendship !== null;
	}

	private async haveMutualGuild(userId1: UserID, userId2: UserID): Promise<boolean> {
		const [user1GuildIds, user2GuildIds] = await Promise.all([
			this.deps.userAccountRepository.getUserGuildIds(userId1),
			this.deps.userAccountRepository.getUserGuildIds(userId2),
		]);
		const user1GuildIdSet = new Set(user1GuildIds.map((id) => id.toString()));
		return user2GuildIds.some((id) => user1GuildIdSet.has(id.toString()));
	}
}
