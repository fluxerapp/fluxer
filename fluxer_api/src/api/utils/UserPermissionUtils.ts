// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {Guild} from '@app/api/models/Guild';
import type {Relationship} from '@app/api/models/Relationship';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {hasMutualGuildForDmAccess} from '@app/api/utils/MutualGuildDmAccess';
import {
	FriendSourceFlags,
	GroupDmAddPermissionFlags,
	IncomingCallFlags,
	RelationshipTypes,
} from '@fluxer/constants/src/UserConstants';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {FriendRequestBlockedError} from '@fluxer/errors/src/domains/user/FriendRequestBlockedError';

type UserPermissionRepository = Pick<IUserRepository, 'findSettings' | 'listRelationships' | 'getRelationship'>;
type GuildPermissionRepository = Pick<IGuildRepositoryAggregate, 'listUserGuilds'>;

export class UserPermissionUtils {
	constructor(
		private userRepository: UserPermissionRepository,
		private guildRepository: GuildPermissionRepository,
	) {}

	async validateFriendSourcePermissions({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		const targetSettings = await this.userRepository.findSettings(targetId);
		if (!targetSettings) return;
		const friendSourceFlags = targetSettings.friendSourceFlags;
		if ((friendSourceFlags & FriendSourceFlags.NO_RELATION) === FriendSourceFlags.NO_RELATION) {
			return;
		}
		if ((friendSourceFlags & FriendSourceFlags.MUTUAL_FRIENDS) === FriendSourceFlags.MUTUAL_FRIENDS) {
			const hasMutualFriends = await this.checkMutualFriends({userId, targetId});
			if (hasMutualFriends) {
				return;
			}
		}
		if ((friendSourceFlags & FriendSourceFlags.MUTUAL_GUILDS) === FriendSourceFlags.MUTUAL_GUILDS) {
			const hasMutualGuilds = await this.checkMutualGuildsAsync({userId, targetId});
			if (hasMutualGuilds) {
				return;
			}
		}
		throw new FriendRequestBlockedError();
	}

	async checkGroupDmAddPermission({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<boolean> {
		const targetSettings = await this.userRepository.findSettings(targetId);
		if (!targetSettings) return true;
		const groupDmAddPermissionFlags = targetSettings.groupDmAddPermissionFlags;
		if ((groupDmAddPermissionFlags & GroupDmAddPermissionFlags.NOBODY) === GroupDmAddPermissionFlags.NOBODY) {
			return false;
		}
		if ((groupDmAddPermissionFlags & GroupDmAddPermissionFlags.EVERYONE) === GroupDmAddPermissionFlags.EVERYONE) {
			return true;
		}
		if (
			(groupDmAddPermissionFlags & GroupDmAddPermissionFlags.FRIENDS_ONLY) ===
			GroupDmAddPermissionFlags.FRIENDS_ONLY
		) {
			const friendship = await this.userRepository.getRelationship(targetId, userId, RelationshipTypes.FRIEND);
			return Boolean(friendship);
		}
		const friendship = await this.userRepository.getRelationship(targetId, userId, RelationshipTypes.FRIEND);
		if (friendship) return true;
		if (
			(groupDmAddPermissionFlags & GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS) ===
			GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS
		) {
			if (await this.checkMutualFriends({userId, targetId})) return true;
		}
		if (
			(groupDmAddPermissionFlags & GroupDmAddPermissionFlags.GUILD_MEMBERS) ===
			GroupDmAddPermissionFlags.GUILD_MEMBERS
		) {
			if (await this.checkMutualGuildsAsync({userId, targetId})) return true;
		}
		return false;
	}

	async validateGroupDmAddPermissions({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		const allowed = await this.checkGroupDmAddPermission({userId, targetId});
		if (!allowed) {
			throw new MissingAccessError();
		}
	}

	async validateIncomingCallPermissions({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		const targetSettings = await this.userRepository.findSettings(targetId);
		if (!targetSettings) return;
		const incomingCallFlags = targetSettings.incomingCallFlags;
		if ((incomingCallFlags & IncomingCallFlags.NOBODY) === IncomingCallFlags.NOBODY) {
			throw new MissingAccessError();
		}
		if ((incomingCallFlags & IncomingCallFlags.EVERYONE) === IncomingCallFlags.EVERYONE) {
			return;
		}
		if ((incomingCallFlags & IncomingCallFlags.FRIENDS_ONLY) === IncomingCallFlags.FRIENDS_ONLY) {
			const friendship = await this.userRepository.getRelationship(targetId, userId, RelationshipTypes.FRIEND);
			if (friendship) {
				return;
			}
			throw new MissingAccessError();
		}
		let hasPermission = false;
		const friendship = await this.userRepository.getRelationship(targetId, userId, RelationshipTypes.FRIEND);
		if (friendship) {
			hasPermission = true;
		}
		if (
			!hasPermission &&
			(incomingCallFlags & IncomingCallFlags.FRIENDS_OF_FRIENDS) === IncomingCallFlags.FRIENDS_OF_FRIENDS
		) {
			const hasMutualFriends = await this.checkMutualFriends({userId, targetId});
			if (hasMutualFriends) hasPermission = true;
		}
		if (!hasPermission && (incomingCallFlags & IncomingCallFlags.GUILD_MEMBERS) === IncomingCallFlags.GUILD_MEMBERS) {
			const hasMutualGuilds = await this.checkMutualGuildsAsync({userId, targetId});
			if (hasMutualGuilds) hasPermission = true;
		}
		if (!hasPermission) {
			throw new MissingAccessError();
		}
	}

	async checkMutualFriends({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<boolean> {
		const userFriends = await this.userRepository.listRelationships(userId);
		const targetFriends = await this.userRepository.listRelationships(targetId);
		const userFriendIds = new Set(
			userFriends.filter((rel) => rel.type === RelationshipTypes.FRIEND).map((rel) => rel.targetUserId.toString()),
		);
		const targetFriendIds = targetFriends
			.filter((rel) => rel.type === RelationshipTypes.FRIEND)
			.map((rel) => rel.targetUserId.toString());
		return targetFriendIds.some((friendId) => userFriendIds.has(friendId));
	}

	private async fetchGuildsForUsers({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<{
		userGuilds: Array<Guild>;
		targetGuilds: Array<Guild>;
	}> {
		const [userGuilds, targetGuilds] = await Promise.all([
			this.guildRepository.listUserGuilds(userId),
			this.guildRepository.listUserGuilds(targetId),
		]);
		return {
			userGuilds,
			targetGuilds,
		};
	}

	private async fetchGuildIdsForUsers({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<{
		userGuildIds: Array<GuildID>;
		targetGuildIds: Array<GuildID>;
	}> {
		const {userGuilds, targetGuilds} = await this.fetchGuildsForUsers({userId, targetId});
		return {
			userGuildIds: userGuilds.map((g) => g.id),
			targetGuildIds: targetGuilds.map((g) => g.id),
		};
	}

	async checkMutualGuildsAsync({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<boolean> {
		const {userGuildIds, targetGuildIds} = await this.fetchGuildIdsForUsers({userId, targetId});
		return this.checkMutualGuilds(userGuildIds, targetGuildIds);
	}

	async checkMutualGuildsForDmAccessAsync({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<boolean> {
		const {userGuilds, targetGuilds} = await this.fetchGuildsForUsers({userId, targetId});
		return hasMutualGuildForDmAccess({userGuilds, targetGuilds});
	}

	checkMutualGuilds(userGuildIds: Array<GuildID>, targetGuildIds: Array<GuildID>): boolean {
		const userGuildIdSet = new Set(userGuildIds.map((id) => id.toString()));
		return targetGuildIds.some((id) => userGuildIdSet.has(id.toString()));
	}

	async getMutualFriends({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<Array<Relationship>> {
		const userFriends = await this.userRepository.listRelationships(userId);
		const targetFriends = await this.userRepository.listRelationships(targetId);
		const targetFriendIds = new Set(
			targetFriends.filter((rel) => rel.type === RelationshipTypes.FRIEND).map((rel) => rel.targetUserId.toString()),
		);
		return userFriends.filter(
			(rel) => rel.type === RelationshipTypes.FRIEND && targetFriendIds.has(rel.targetUserId.toString()),
		);
	}

	async getMutualGuildIds({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<Array<GuildID>> {
		const {userGuildIds, targetGuildIds} = await this.fetchGuildIdsForUsers({userId, targetId});
		const targetGuildIdSet = new Set(targetGuildIds.map((guildId) => guildId.toString()));
		return userGuildIds.filter((guildId) => targetGuildIdSet.has(guildId.toString()));
	}
}
