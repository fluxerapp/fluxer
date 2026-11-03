// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {resolveAdminUsers} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {Relationship} from '@app/api/models/Relationship';
import type {UserRelationshipService} from '@app/api/user/services/UserRelationshipService';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	AdminRelationshipEntry,
	ListUserRelationshipsRequest,
	ListUserRelationshipsResponse,
	RelationshipCategory,
	RemoveUserRelationshipRequest,
	RemoveUserRelationshipsByCategoryRequest,
	RemoveUserRelationshipsResponse,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';

interface AdminUserRelationshipServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	userRelationshipService: Pick<UserRelationshipService, 'endFriendship'>;
}

const CATEGORY_TO_TYPE: Record<RelationshipCategory, number> = {
	friend: RelationshipTypes.FRIEND,
	incoming_request: RelationshipTypes.INCOMING_REQUEST,
	outgoing_request: RelationshipTypes.OUTGOING_REQUEST,
	blocked: RelationshipTypes.BLOCKED,
};
const TYPE_TO_CATEGORY: Record<number, RelationshipCategory> = {
	[RelationshipTypes.FRIEND]: 'friend',
	[RelationshipTypes.INCOMING_REQUEST]: 'incoming_request',
	[RelationshipTypes.OUTGOING_REQUEST]: 'outgoing_request',
	[RelationshipTypes.BLOCKED]: 'blocked',
};

export class AdminUserRelationshipService {
	constructor(private readonly deps: AdminUserRelationshipServiceDeps) {}

	async listRelationships(data: ListUserRelationshipsRequest): Promise<ListUserRelationshipsResponse> {
		const {users: userRepository} = this.deps.apiContext.services;
		const userId = createUserID(data.user_id);
		await userRepository.findUniqueAssert(userId);
		const relationships = await userRepository.listRelationships(userId);
		const targetUserIds = new Set<UserID>();
		for (const rel of relationships) {
			targetUserIds.add(rel.targetUserId);
		}
		const resolvedUsers = await resolveAdminUsers(this.deps.apiContext.services.users, [...targetUserIds]);
		const friends: Array<AdminRelationshipEntry> = [];
		const incomingRequests: Array<AdminRelationshipEntry> = [];
		const outgoingRequests: Array<AdminRelationshipEntry> = [];
		const blocked: Array<AdminRelationshipEntry> = [];
		for (const rel of relationships) {
			const category = TYPE_TO_CATEGORY[rel.type];
			if (!category) continue;
			const entry: AdminRelationshipEntry = {
				target_user_id: rel.targetUserId.toString(),
				category,
				nickname: rel.nickname,
				since: rel.since ? rel.since.toISOString() : null,
				target: resolvedUsers.get(rel.targetUserId) ?? null,
			};
			switch (rel.type) {
				case RelationshipTypes.FRIEND:
					friends.push(entry);
					break;
				case RelationshipTypes.INCOMING_REQUEST:
					incomingRequests.push(entry);
					break;
				case RelationshipTypes.OUTGOING_REQUEST:
					outgoingRequests.push(entry);
					break;
				case RelationshipTypes.BLOCKED:
					blocked.push(entry);
					break;
			}
		}
		return {
			friends,
			incoming_requests: incomingRequests,
			outgoing_requests: outgoingRequests,
			blocked,
		};
	}

	async removeRelationship(
		data: RemoveUserRelationshipRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<void> {
		const {auditService} = this.deps;
		const {users: userRepository} = this.deps.apiContext.services;
		const userId = createUserID(data.user_id);
		const targetUserId = createUserID(data.target_user_id);
		await userRepository.findUniqueAssert(userId);
		const type = CATEGORY_TO_TYPE[data.category];
		const relationship = await userRepository.getRelationship(userId, targetUserId, type);
		if (!relationship) {
			throw new UnknownUserError();
		}
		await this.removePair({userId, targetUserId, category: data.category});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'remove_relationship',
			auditLogReason,
			metadata: new Map([
				['target_user_id', targetUserId.toString()],
				['category', data.category],
			]),
		});
	}

	async removeRelationshipsByCategory(
		data: RemoveUserRelationshipsByCategoryRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<RemoveUserRelationshipsResponse> {
		const {auditService} = this.deps;
		const {users: userRepository} = this.deps.apiContext.services;
		const userId = createUserID(data.user_id);
		await userRepository.findUniqueAssert(userId);
		const type = CATEGORY_TO_TYPE[data.category];
		const allRelationships = await userRepository.listRelationships(userId);
		const matching: Array<Relationship> = allRelationships.filter((rel) => rel.type === type);
		for (const rel of matching) {
			await this.removePair({userId, targetUserId: rel.targetUserId, category: data.category});
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'remove_relationships_by_category',
			auditLogReason,
			metadata: new Map([
				['category', data.category],
				['removed_count', matching.length.toString()],
			]),
		});
		return {removed_count: matching.length};
	}

	private async removePair({
		userId,
		targetUserId,
		category,
	}: {
		userId: UserID;
		targetUserId: UserID;
		category: RelationshipCategory;
	}): Promise<void> {
		const {users: userRepository, gateway: gatewayService} = this.deps.apiContext.services;
		switch (category) {
			case 'friend':
				await this.deps.userRelationshipService.endFriendship(userId, targetUserId);
				break;
			case 'outgoing_request':
				await userRepository.deleteRelationship(userId, targetUserId, RelationshipTypes.OUTGOING_REQUEST);
				await userRepository.deleteRelationship(targetUserId, userId, RelationshipTypes.INCOMING_REQUEST);
				break;
			case 'incoming_request':
				await userRepository.deleteRelationship(userId, targetUserId, RelationshipTypes.INCOMING_REQUEST);
				await userRepository.deleteRelationship(targetUserId, userId, RelationshipTypes.OUTGOING_REQUEST);
				break;
			case 'blocked':
				await userRepository.deleteRelationship(userId, targetUserId, RelationshipTypes.BLOCKED);
				break;
		}
		await gatewayService.dispatchPresence({
			userId,
			event: 'RELATIONSHIP_REMOVE',
			data: {id: targetUserId.toString()},
		});
		if (category !== 'blocked') {
			await gatewayService.dispatchPresence({
				userId: targetUserId,
				event: 'RELATIONSHIP_REMOVE',
				data: {id: userId.toString()},
			});
		}
	}
}
