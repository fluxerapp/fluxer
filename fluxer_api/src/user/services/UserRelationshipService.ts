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

import type {UserID} from '~/BrandedTypes';
import {MAX_RELATIONSHIPS, RelationshipTypes, UserFlags} from '~/Constants';
import {
	AlreadyFriendsError,
	BotsCannotHaveFriendsError,
	CannotSendFriendRequestToBlockedUserError,
	CannotSendFriendRequestToSelfError,
	FriendRequestBlockedError,
	InvalidDiscriminatorError,
	MaxRelationshipsError,
	NoUsersWithFluxertagError,
	UnclaimedAccountRestrictedError,
	UnknownUserError,
} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Relationship} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {type FriendRequestByTagRequest, mapRelationshipToResponse} from '~/user/UserModel';
import type {UserPermissionUtils} from '~/utils/UserPermissionUtils';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import {getCachedUserPartialResponse} from '../UserCacheHelpers';

export class UserRelationshipService {
	constructor(
		private userAccountRepository: IUserAccountRepository,
		private userRelationshipRepository: IUserRelationshipRepository,
		private gatewayService: IGatewayService,
		private userPermissionUtils: UserPermissionUtils,
	) {}

	async getRelationships(userId: UserID): Promise<Array<Relationship>> {
		return await this.userRelationshipRepository.listRelationships(userId);
	}

	async sendFriendRequestByTag({
		userId,
		data,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		data: FriendRequestByTagRequest;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		const {username, discriminator} = data;
		const discrimValue = Number.parseInt(discriminator, 10);
		if (Number.isNaN(discrimValue) || discrimValue < 0 || discrimValue > 9999) {
			throw new InvalidDiscriminatorError();
		}
		const targetUser = await this.userAccountRepository.findByUsernameDiscriminator(username, discrimValue);
		if (!targetUser) {
			throw new NoUsersWithFluxertagError();
		}
		const existingRelationship = await this.userRelationshipRepository.getRelationship(
			userId,
			targetUser.id,
			RelationshipTypes.FRIEND,
		);
		if (existingRelationship) {
			throw new AlreadyFriendsError();
		}
		return this.sendFriendRequest({userId, targetId: targetUser.id, userCacheService, requestCache});
	}

	async sendFriendRequest({
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		await this.validateFriendRequest({userId, targetId});
		const pendingIncoming = await this.userRelationshipRepository.getRelationship(
			targetId,
			userId,
			RelationshipTypes.OUTGOING_REQUEST,
		);
		if (pendingIncoming) {
			return this.acceptFriendRequest({userId, targetId, userCacheService, requestCache});
		}
		const existingFriendship = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.FRIEND,
		);
		const existingOutgoingRequest = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.OUTGOING_REQUEST,
		);
		if (existingFriendship || existingOutgoingRequest) {
			const relationships = await this.userRelationshipRepository.listRelationships(userId);
			const relationship = relationships.find((r) => r.targetUserId === targetId);
			if (relationship) {
				return relationship;
			}
		}
		await this.validateRelationshipCounts({userId, targetId});
		return await this.createFriendRequest({userId, targetId, userCacheService, requestCache});
	}

	async acceptFriendRequest({
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		const user = await this.userAccountRepository.findUnique(userId);
		if (user && !user.passwordHash) {
			throw new UnclaimedAccountRestrictedError('accept friend requests');
		}

		const incomingRequest = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.INCOMING_REQUEST,
		);
		if (!incomingRequest) {
			throw new UnknownUserError();
		}
		await this.validateRelationshipCounts({userId, targetId});

		await this.userRelationshipRepository.deleteRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST);
		await this.userRelationshipRepository.deleteRelationship(targetId, userId, RelationshipTypes.OUTGOING_REQUEST);

		const now = new Date();
		const userRelationship = await this.userRelationshipRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.FRIEND,
			nickname: null,
			since: now,
			version: 1,
		});
		const targetRelationship = await this.userRelationshipRepository.upsertRelationship({
			source_user_id: targetId,
			target_user_id: userId,
			type: RelationshipTypes.FRIEND,
			nickname: null,
			since: now,
			version: 1,
		});
		await this.dispatchRelationshipUpdate({
			userId,
			relationship: userRelationship,
			userCacheService,
			requestCache,
		});
		await this.dispatchRelationshipUpdate({
			userId: targetId,
			relationship: targetRelationship,
			userCacheService,
			requestCache,
		});

		return userRelationship;
	}

	async blockUser({
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		const targetUser = await this.userAccountRepository.findUnique(targetId);
		if (!targetUser) {
			throw new UnknownUserError();
		}

		const existingBlocked = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.BLOCKED,
		);
		if (existingBlocked) {
			return existingBlocked;
		}

		const existingFriend = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.FRIEND,
		);
		const existingIncomingRequest = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.INCOMING_REQUEST,
		);
		const existingOutgoingRequest = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.OUTGOING_REQUEST,
		);

		if (existingFriend) {
			await this.userRelationshipRepository.deleteRelationship(userId, targetId, RelationshipTypes.FRIEND);
			await this.userRelationshipRepository.deleteRelationship(targetId, userId, RelationshipTypes.FRIEND);
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
		} else if (existingOutgoingRequest) {
			await this.userRelationshipRepository.deleteRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST);
			await this.userRelationshipRepository.deleteRelationship(targetId, userId, RelationshipTypes.INCOMING_REQUEST);
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
		} else if (existingIncomingRequest) {
			await this.userRelationshipRepository.deleteRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST);
		}

		const now = new Date();
		const blockRelationship = await this.userRelationshipRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.BLOCKED,
			nickname: null,
			since: now,
			version: 1,
		});

		await this.dispatchRelationshipCreate({
			userId,
			relationship: blockRelationship,
			userCacheService,
			requestCache,
		});

		return blockRelationship;
	}

	async removeRelationship({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		const existingRelationship =
			(await this.userRelationshipRepository.getRelationship(userId, targetId, RelationshipTypes.FRIEND)) ||
			(await this.userRelationshipRepository.getRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST)) ||
			(await this.userRelationshipRepository.getRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST)) ||
			(await this.userRelationshipRepository.getRelationship(userId, targetId, RelationshipTypes.BLOCKED));
		if (!existingRelationship) throw new UnknownUserError();
		const relationshipType = existingRelationship.type;
		if (relationshipType === RelationshipTypes.INCOMING_REQUEST || relationshipType === RelationshipTypes.BLOCKED) {
			await this.userRelationshipRepository.deleteRelationship(userId, targetId, relationshipType);
			await this.dispatchRelationshipRemove({
				userId,
				targetId: targetId.toString(),
			});
			return;
		}
		if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
			await this.userRelationshipRepository.deleteRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST);
			await this.userRelationshipRepository.deleteRelationship(targetId, userId, RelationshipTypes.INCOMING_REQUEST);
			await this.dispatchRelationshipRemove({userId, targetId: targetId.toString()});
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
			return;
		}
		if (relationshipType === RelationshipTypes.FRIEND) {
			await this.userRelationshipRepository.deleteRelationship(userId, targetId, RelationshipTypes.FRIEND);
			await this.userRelationshipRepository.deleteRelationship(targetId, userId, RelationshipTypes.FRIEND);
			await this.dispatchRelationshipRemove({userId, targetId: targetId.toString()});
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
			return;
		}
		await this.userRelationshipRepository.deleteRelationship(userId, targetId, relationshipType);
		await this.dispatchRelationshipRemove({userId, targetId: targetId.toString()});
	}

	async updateFriendNickname({
		userId,
		targetId,
		nickname,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		nickname: string | null;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		const relationship = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.FRIEND,
		);
		if (!relationship) {
			throw new UnknownUserError();
		}

		const updatedRelationship = await this.userRelationshipRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.FRIEND,
			nickname,
			since: relationship.since ?? new Date(),
			version: 1,
		});

		await this.dispatchRelationshipUpdate({
			userId,
			relationship: updatedRelationship,
			userCacheService,
			requestCache,
		});

		return updatedRelationship;
	}

	private async validateFriendRequest({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		if (userId === targetId) {
			throw new CannotSendFriendRequestToSelfError();
		}

		const requesterUser = await this.userAccountRepository.findUnique(userId);
		if (requesterUser && !requesterUser.passwordHash) {
			throw new UnclaimedAccountRestrictedError('send friend requests');
		}

		const targetUser = await this.userAccountRepository.findUnique(targetId);
		if (!targetUser) throw new UnknownUserError();
		if (targetUser.isBot) {
			throw new BotsCannotHaveFriendsError();
		}
		if (targetUser.flags & UserFlags.APP_STORE_REVIEWER) {
			throw new FriendRequestBlockedError();
		}
		const requesterBlockedTarget = await this.userRelationshipRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.BLOCKED,
		);
		if (requesterBlockedTarget) {
			throw new CannotSendFriendRequestToBlockedUserError();
		}
		const targetBlockedRequester = await this.userRelationshipRepository.getRelationship(
			targetId,
			userId,
			RelationshipTypes.BLOCKED,
		);
		if (targetBlockedRequester) {
			throw new FriendRequestBlockedError();
		}
		await this.userPermissionUtils.validateFriendSourcePermissions({userId, targetId});
	}

	private async validateRelationshipCounts({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		const relationships = await this.userRelationshipRepository.listRelationships(userId);
		if (relationships.length >= MAX_RELATIONSHIPS) {
			throw new MaxRelationshipsError();
		}
		const targetRelationships = await this.userRelationshipRepository.listRelationships(targetId);
		if (targetRelationships.length >= MAX_RELATIONSHIPS) {
			throw new MaxRelationshipsError();
		}
	}

	private async createFriendRequest({
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		const now = new Date();
		const userRelationship = await this.userRelationshipRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.OUTGOING_REQUEST,
			nickname: null,
			since: now,
			version: 1,
		});
		const targetRelationship = await this.userRelationshipRepository.upsertRelationship({
			source_user_id: targetId,
			target_user_id: userId,
			type: RelationshipTypes.INCOMING_REQUEST,
			nickname: null,
			since: now,
			version: 1,
		});
		await this.dispatchRelationshipCreate({userId, relationship: userRelationship, userCacheService, requestCache});
		await this.dispatchRelationshipCreate({
			userId: targetId,
			relationship: targetRelationship,
			userCacheService,
			requestCache,
		});
		return userRelationship;
	}

	async dispatchRelationshipCreate({
		userId,
		relationship,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		relationship: Relationship;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		const userPartialResolver = (userId: UserID) =>
			getCachedUserPartialResponse({userId, userCacheService, requestCache});
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'RELATIONSHIP_ADD',
			data: await mapRelationshipToResponse({relationship, userPartialResolver}),
		});
	}

	async dispatchRelationshipUpdate({
		userId,
		relationship,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		relationship: Relationship;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		const userPartialResolver = (userId: UserID) =>
			getCachedUserPartialResponse({userId, userCacheService, requestCache});
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'RELATIONSHIP_UPDATE',
			data: await mapRelationshipToResponse({relationship, userPartialResolver}),
		});
	}

	async dispatchRelationshipRemove({userId, targetId}: {userId: UserID; targetId: string}): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'RELATIONSHIP_REMOVE',
			data: {id: targetId},
		});
	}
}
