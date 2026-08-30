// SPDX-License-Identifier: AGPL-3.0-or-later

import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {
	BulkIgnoreFriendRequestsRequest,
	FriendRequestByTagRequest,
	FriendRequestCreateRequest,
	RelationshipNicknameUpdateRequest,
	RelationshipTypePutRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {
	BulkIgnoreFriendRequestsResponse,
	RelationshipResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {UserID} from '../../BrandedTypes';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {Relationship} from '../../models/Relationship';
import {getCachedUserPartialResponse, getCachedUserPartialResponses} from '../UserCacheHelpers';
import {mapRelationshipToResponse} from '../UserMappers';
import type {UserChannelService} from './UserChannelService';
import type {UserRelationshipService} from './UserRelationshipService';

interface RelationshipListParams {
	userId: UserID;
	requestCache: RequestCache;
}

interface RelationshipSendByTagParams {
	userId: UserID;
	data: FriendRequestByTagRequest;
	requestCache: RequestCache;
}

interface RelationshipSendParams {
	userId: UserID;
	targetId: UserID;
	data: FriendRequestCreateRequest;
	requestCache: RequestCache;
}

interface RelationshipUpdateTypeParams {
	userId: UserID;
	targetId: UserID;
	data: RelationshipTypePutRequest;
	requestCache: RequestCache;
}

interface RelationshipDeleteParams {
	userId: UserID;
	targetId: UserID;
}

interface RelationshipNicknameParams {
	userId: UserID;
	targetId: UserID;
	data: RelationshipNicknameUpdateRequest;
	requestCache: RequestCache;
}

export class UserRelationshipRequestService {
	constructor(
		private readonly userRelationshipService: UserRelationshipService,
		private readonly userChannelService: UserChannelService,
		private readonly userCacheService: UserCacheService,
	) {}

	async listRelationships(params: RelationshipListParams): Promise<Array<RelationshipResponse>> {
		const userPartialResolver = this.createUserPartialResolver(params.requestCache);
		const inverseRelationshipResolver = this.createInverseRelationshipResolver(params.userId);
		const relationships = await this.userRelationshipService.getRelationships(params.userId);
		await getCachedUserPartialResponses({
			userIds: relationships.map((relationship) => relationship.targetUserId),
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
		return Promise.all(
			relationships.map((relationship) =>
				mapRelationshipToResponse({relationship, userPartialResolver, inverseRelationshipResolver}),
			),
		);
	}

	async sendFriendRequestByTag(params: RelationshipSendByTagParams): Promise<RelationshipResponse> {
		const userPartialResolver = this.createUserPartialResolver(params.requestCache);
		const relationship = await this.userRelationshipService.sendFriendRequestByTag({
			userId: params.userId,
			data: params.data,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
		return mapRelationshipToResponse({relationship, userPartialResolver});
	}

	async sendFriendRequest(params: RelationshipSendParams): Promise<RelationshipResponse> {
		const userPartialResolver = this.createUserPartialResolver(params.requestCache);
		const relationship = await this.userRelationshipService.sendFriendRequest({
			userId: params.userId,
			targetId: params.targetId,
			staffForceAccept: params.data.staff_force_accept === true,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
		return mapRelationshipToResponse({relationship, userPartialResolver});
	}

	async updateRelationshipType(params: RelationshipUpdateTypeParams): Promise<RelationshipResponse> {
		const userPartialResolver = this.createUserPartialResolver(params.requestCache);
		if (params.data?.type === RelationshipTypes.BLOCKED) {
			const relationship = await this.userRelationshipService.blockUser({
				userId: params.userId,
				targetId: params.targetId,
				userCacheService: this.userCacheService,
				requestCache: params.requestCache,
			});
			return mapRelationshipToResponse({relationship, userPartialResolver});
		}
		const relationship = await this.userRelationshipService.acceptFriendRequest({
			userId: params.userId,
			targetId: params.targetId,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
		await this.userChannelService.ensureDmOpenForBothUsers({
			userId: params.userId,
			recipientId: params.targetId,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
		return mapRelationshipToResponse({relationship, userPartialResolver});
	}

	async removeRelationship(params: RelationshipDeleteParams): Promise<void> {
		await this.userRelationshipService.removeRelationship({userId: params.userId, targetId: params.targetId});
	}

	async updateNickname(params: RelationshipNicknameParams): Promise<RelationshipResponse> {
		const userPartialResolver = this.createUserPartialResolver(params.requestCache);
		const relationship = await this.userRelationshipService.updateFriendNickname({
			userId: params.userId,
			targetId: params.targetId,
			nickname: params.data.nickname ?? null,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
		return mapRelationshipToResponse({relationship, userPartialResolver});
	}

	async bulkIgnoreIncomingRequests(params: {
		userId: UserID;
		data: BulkIgnoreFriendRequestsRequest;
	}): Promise<BulkIgnoreFriendRequestsResponse> {
		const result = await this.userRelationshipService.bulkIgnoreIncomingRequests({
			userId: params.userId,
			data: params.data,
		});
		return {ignored_count: result.ignoredCount};
	}

	private createUserPartialResolver(requestCache: RequestCache) {
		return (userId: UserID) =>
			getCachedUserPartialResponse({userId, userCacheService: this.userCacheService, requestCache});
	}

	private createInverseRelationshipResolver(viewerId: UserID) {
		return (relationship: Relationship) =>
			this.userRelationshipService.getRelationship({
				userId: relationship.targetUserId,
				targetId: viewerId,
				type: relationship.type,
			});
	}
}
