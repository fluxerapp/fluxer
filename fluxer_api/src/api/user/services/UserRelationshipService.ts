// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_RELATIONSHIPS} from '@fluxer/constants/src/LimitConstants';
import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {DirectMessagesDisabledError} from '@fluxer/errors/src/domains/channel/DirectMessagesDisabledError';
import {BotsCannotSendFriendRequestsError} from '@fluxer/errors/src/domains/oauth/BotsCannotSendFriendRequestsError';
import {AlreadyFriendsError} from '@fluxer/errors/src/domains/user/AlreadyFriendsError';
import {CannotBlockSystemUserError} from '@fluxer/errors/src/domains/user/CannotBlockSystemUserError';
import {CannotSendFriendRequestToBlockedUserError} from '@fluxer/errors/src/domains/user/CannotSendFriendRequestToBlockedUserError';
import {CannotSendFriendRequestToSelfError} from '@fluxer/errors/src/domains/user/CannotSendFriendRequestToSelfError';
import {FriendRequestBlockedError} from '@fluxer/errors/src/domains/user/FriendRequestBlockedError';
import {InvalidDiscriminatorError} from '@fluxer/errors/src/domains/user/InvalidDiscriminatorError';
import {MaxRelationshipsError} from '@fluxer/errors/src/domains/user/MaxRelationshipsError';
import {NoUsersWithFluxertagError} from '@fluxer/errors/src/domains/user/NoUsersWithFluxertagError';
import {UnclaimedAccountCannotAcceptFriendRequestsError} from '@fluxer/errors/src/domains/user/UnclaimedAccountCannotAcceptFriendRequestsError';
import {UnclaimedAccountCannotSendFriendRequestsError} from '@fluxer/errors/src/domains/user/UnclaimedAccountCannotSendFriendRequestsError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	BulkIgnoreFriendRequestsRequest,
	FriendRequestByTagRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import type {ApiContext} from '../../ApiContext';
import {requireEmailVerified} from '../../auth/EmailVerificationUtils';
import type {UserID} from '../../BrandedTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import type {Relationship} from '../../models/Relationship';
import type {User} from '../../models/User';
import type {UserPermissionUtils} from '../../utils/UserPermissionUtils';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '../repositories/IUserSettingsRepository';
import {getCachedUserPartialResponse} from '../UserCacheHelpers';
import {mapRelationshipToResponse} from '../UserMappers';
import type {DirectMessageSpamMitigationService} from './DirectMessageSpamMitigationService';
import {createDirectMessageSpamMitigationService} from './DirectMessageSpamMitigationService';

interface UserRelationshipRepository
	extends IUserAccountRepository,
		IUserRelationshipRepository,
		IUserSettingsRepository {}

export class UserRelationshipService {
	private readonly userRepository: UserRelationshipRepository;
	private readonly gatewayService: IGatewayService;
	private readonly dmSpamMitigationService: DirectMessageSpamMitigationService;

	constructor(
		apiContext: ApiContext,
		private userPermissionUtils: UserPermissionUtils,
		private readonly limitConfigService: LimitConfigService,
	) {
		const {users, gateway} = apiContext.services;
		this.userRepository = users;
		this.gatewayService = gateway;
		this.dmSpamMitigationService = createDirectMessageSpamMitigationService(apiContext, this.userRepository);
	}

	async getRelationship(params: {userId: UserID; targetId: UserID; type: number}): Promise<Relationship | null> {
		return await this.userRepository.getRelationship(params.userId, params.targetId, params.type);
	}

	async getRelationships(userId: UserID): Promise<Array<Relationship>> {
		return await this.userRepository.listRelationships(userId);
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
		const discrimValue = discriminator;
		if (!Number.isInteger(discrimValue) || discrimValue < 0 || discrimValue > 9999) {
			throw new InvalidDiscriminatorError();
		}
		const targetUser = await this.userRepository.findByUsernameDiscriminator(username, discrimValue);
		if (!targetUser) {
			throw new NoUsersWithFluxertagError();
		}
		if (this.isDeletedUser(targetUser)) {
			throw new FriendRequestBlockedError();
		}
		const existingRelationship = await this.userRepository.getRelationship(
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
		staffForceAccept,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		staffForceAccept?: boolean;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		if (!staffForceAccept && (await getInstanceConfigRepository().getInstancePolicyConfig()).direct_messages_disabled) {
			throw new DirectMessagesDisabledError();
		}
		const requesterUser = await this.userRepository.findUnique(userId);
		if (staffForceAccept) {
			const requesterIsStaff = requesterUser != null && (requesterUser.flags & UserFlags.STAFF) === UserFlags.STAFF;
			if (requesterIsStaff) {
				return await this.forceCreateFriendship({userId, targetId, userCacheService, requestCache});
			}
		}
		if (!requesterUser) {
			throw new UnknownUserError();
		}
		if (this.dmSpamMitigationService.shouldSuppressDirectMessageDelivery(requesterUser)) {
			return await this.createShadowFriendRequest({
				requesterUser,
				userId,
				targetId,
				userCacheService,
				requestCache,
			});
		}
		if (userId === targetId) {
			throw new CannotSendFriendRequestToSelfError();
		}
		const targetUserForAttempt = await this.userRepository.findUnique(targetId);
		if (!targetUserForAttempt) {
			throw new UnknownUserError();
		}
		if (this.isDeletedUser(targetUserForAttempt)) {
			throw new FriendRequestBlockedError();
		}
		const pendingIncoming = await this.userRepository.getRelationship(
			targetId,
			userId,
			RelationshipTypes.OUTGOING_REQUEST,
		);
		if (pendingIncoming) {
			return this.acceptFriendRequest({userId, targetId, userCacheService, requestCache});
		}
		const existingFriendship = await this.userRepository.getRelationship(userId, targetId, RelationshipTypes.FRIEND);
		const existingOutgoingRequest = await this.userRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.OUTGOING_REQUEST,
		);
		if (existingFriendship || existingOutgoingRequest) {
			const relationships = await this.userRepository.listRelationships(userId);
			const relationship = relationships.find((r) => r.targetUserId === targetId);
			if (relationship) {
				return relationship;
			}
		}
		const spamDecision = await this.dmSpamMitigationService.recordFriendRequestSend({
			requester: requesterUser,
			targetId,
		});
		if (spamDecision.shouldSuppressRecipientDelivery) {
			return await this.createShadowFriendRequest({
				requesterUser,
				userId,
				targetId,
				userCacheService,
				requestCache,
			});
		}
		const targetUser = await this.validateFriendRequest({userId, targetId});
		await this.validateRelationshipCounts({userId, targetId});
		const requestRelationship = await this.createFriendRequest({userId, targetId, userCacheService, requestCache});
		const targetIsFriendlyBot =
			targetUser.isBot && (targetUser.flags & UserFlags.FRIENDLY_BOT) === UserFlags.FRIENDLY_BOT;
		const manualApprovalFlag = UserFlags.FRIENDLY_BOT_MANUAL_APPROVAL;
		const manualApprovalRequired = targetUser.isBot && (targetUser.flags & manualApprovalFlag) === manualApprovalFlag;
		if (targetIsFriendlyBot && !manualApprovalRequired) {
			const finalFriendship = await this.acceptFriendRequest({
				userId: targetId,
				targetId: userId,
				userCacheService,
				requestCache,
			});
			return finalFriendship;
		}
		return requestRelationship;
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
		if ((await getInstanceConfigRepository().getInstancePolicyConfig()).direct_messages_disabled) {
			throw new DirectMessagesDisabledError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (this.isDeletedUser(user)) {
			throw new FriendRequestBlockedError();
		}
		if (user?.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotAcceptFriendRequestsError();
		}
		const requesterUser = await this.userRepository.findUnique(targetId);
		if (!requesterUser) {
			throw new UnknownUserError();
		}
		if (this.isDeletedUser(requesterUser)) {
			throw new FriendRequestBlockedError();
		}
		const incomingRequest = await this.userRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.INCOMING_REQUEST,
		);
		if (!incomingRequest) {
			throw new UnknownUserError();
		}
		await this.validateRelationshipCounts({userId, targetId});
		await this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST);
		await this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.OUTGOING_REQUEST);
		const now = new Date();
		const [userDefault, targetDefault] = await Promise.all([
			this.userRepository.findSettings(userId).then((s) => s?.defaultShareVoiceActivity ?? true),
			this.userRepository.findSettings(targetId).then((s) => s?.defaultShareVoiceActivity ?? true),
		]);
		const userRelationship = await this.userRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.FRIEND,
			nickname: null,
			since: now,
			share_voice_activity: userDefault,
			version: 1,
		});
		const targetRelationship = await this.userRepository.upsertRelationship({
			source_user_id: targetId,
			target_user_id: userId,
			type: RelationshipTypes.FRIEND,
			nickname: null,
			since: now,
			share_voice_activity: targetDefault,
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
		const targetUser = await this.userRepository.findUnique(targetId);
		if (!targetUser) {
			throw new UnknownUserError();
		}
		if (targetUser.isSystem) {
			throw new CannotBlockSystemUserError();
		}
		const existingBlocked = await this.userRepository.getRelationship(userId, targetId, RelationshipTypes.BLOCKED);
		if (existingBlocked) {
			return existingBlocked;
		}
		const existingFriend = await this.userRepository.getRelationship(userId, targetId, RelationshipTypes.FRIEND);
		const existingIncomingRequest = await this.userRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.INCOMING_REQUEST,
		);
		const existingOutgoingRequest = await this.userRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.OUTGOING_REQUEST,
		);
		if (existingFriend) {
			await this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.FRIEND);
			await this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.FRIEND);
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
		} else if (existingOutgoingRequest) {
			await this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST);
			await this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.INCOMING_REQUEST);
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
		} else if (existingIncomingRequest) {
			await this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST);
		}
		const now = new Date();
		const blockRelationship = await this.userRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.BLOCKED,
			nickname: null,
			since: now,
			share_voice_activity: true,
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
		const [friend, incoming, outgoing, blocked] = await Promise.all([
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.FRIEND),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.BLOCKED),
		]);
		const existingRelationship = friend || incoming || outgoing || blocked;
		if (!existingRelationship) throw new UnknownUserError();
		const relationshipType = existingRelationship.type;
		if (relationshipType === RelationshipTypes.INCOMING_REQUEST || relationshipType === RelationshipTypes.BLOCKED) {
			await this.userRepository.deleteRelationship(userId, targetId, relationshipType);
			await this.dispatchRelationshipRemove({
				userId,
				targetId: targetId.toString(),
			});
			return;
		}
		if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
			await this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST);
			await this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.INCOMING_REQUEST);
			await this.dispatchRelationshipRemove({userId, targetId: targetId.toString()});
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
			return;
		}
		if (relationshipType === RelationshipTypes.FRIEND) {
			await this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.FRIEND);
			await this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.FRIEND);
			await this.dispatchRelationshipRemove({userId, targetId: targetId.toString()});
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
			return;
		}
		await this.userRepository.deleteRelationship(userId, targetId, relationshipType);
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
		const relationship = await this.userRepository.getRelationship(userId, targetId, RelationshipTypes.FRIEND);
		if (!relationship) {
			throw new UnknownUserError();
		}
		const updatedRelationship = await this.userRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.FRIEND,
			nickname,
			since: relationship.since ?? new Date(),
			share_voice_activity: relationship.shareVoiceActivity,
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

	async bulkIgnoreIncomingRequests({userId, data}: {userId: UserID; data: BulkIgnoreFriendRequestsRequest}): Promise<{
		ignoredCount: number;
	}> {
		const incomingRequests = await this.userRepository.listIncomingRequests(userId);
		let requestsToIgnore = incomingRequests;
		if (data.filter === 'new_accounts' && data.max_account_age_seconds != null) {
			const cutoffMs = data.max_account_age_seconds * 1000;
			const now = Date.now();
			requestsToIgnore = incomingRequests.filter((rel) => {
				const senderCreatedAt = extractTimestamp(rel.targetUserId.toString());
				return now - senderCreatedAt < cutoffMs;
			});
		}
		for (const relationship of requestsToIgnore) {
			await this.userRepository.deleteRelationship(
				userId,
				relationship.targetUserId,
				RelationshipTypes.INCOMING_REQUEST,
			);
			await this.userRepository.deleteRelationship(
				relationship.targetUserId,
				userId,
				RelationshipTypes.OUTGOING_REQUEST,
			);
			await this.dispatchRelationshipRemove({userId, targetId: relationship.targetUserId.toString()});
			await this.dispatchRelationshipRemove({
				userId: relationship.targetUserId,
				targetId: userId.toString(),
			});
		}
		return {ignoredCount: requestsToIgnore.length};
	}

	private async forceCreateFriendship({
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
		if (userId === targetId) {
			throw new CannotSendFriendRequestToSelfError();
		}
		const requesterUser = await this.userRepository.findUnique(userId);
		if (!requesterUser) {
			throw new UnknownUserError();
		}
		if (this.isDeletedUser(requesterUser)) {
			throw new FriendRequestBlockedError();
		}
		if (requesterUser.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotSendFriendRequestsError();
		}
		requireEmailVerified(requesterUser, 'friend_request');
		if (requesterUser.isBot) {
			throw new BotsCannotSendFriendRequestsError();
		}
		const targetUser = await this.userRepository.findUnique(targetId);
		if (!targetUser) {
			throw new UnknownUserError();
		}
		if (this.isDeletedUser(targetUser)) {
			throw new FriendRequestBlockedError();
		}
		const [
			userFriend,
			userBlocked,
			userIncoming,
			userOutgoing,
			targetFriend,
			targetBlocked,
			targetIncoming,
			targetOutgoing,
		] = await Promise.all([
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.FRIEND),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.BLOCKED),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST),
			this.userRepository.getRelationship(targetId, userId, RelationshipTypes.FRIEND),
			this.userRepository.getRelationship(targetId, userId, RelationshipTypes.BLOCKED),
			this.userRepository.getRelationship(targetId, userId, RelationshipTypes.INCOMING_REQUEST),
			this.userRepository.getRelationship(targetId, userId, RelationshipTypes.OUTGOING_REQUEST),
		]);
		const userAlreadyFriends = userFriend != null && targetFriend != null;
		const hasExtraUserRelationship = userBlocked != null || userIncoming != null || userOutgoing != null;
		const hasExtraTargetRelationship = targetBlocked != null || targetIncoming != null || targetOutgoing != null;
		if (userAlreadyFriends && !hasExtraUserRelationship && !hasExtraTargetRelationship) {
			return userFriend;
		}
		await Promise.all([
			...(userFriend ? [this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.FRIEND)] : []),
			...(userBlocked ? [this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.BLOCKED)] : []),
			...(userIncoming
				? [this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.INCOMING_REQUEST)]
				: []),
			...(userOutgoing
				? [this.userRepository.deleteRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST)]
				: []),
			...(targetFriend ? [this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.FRIEND)] : []),
			...(targetBlocked ? [this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.BLOCKED)] : []),
			...(targetIncoming
				? [this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.INCOMING_REQUEST)]
				: []),
			...(targetOutgoing
				? [this.userRepository.deleteRelationship(targetId, userId, RelationshipTypes.OUTGOING_REQUEST)]
				: []),
		]);
		const now = new Date();
		const [userDefaultStaff, targetDefaultStaff] = await Promise.all([
			this.userRepository.findSettings(userId).then((s) => s?.defaultShareVoiceActivity ?? true),
			this.userRepository.findSettings(targetId).then((s) => s?.defaultShareVoiceActivity ?? true),
		]);
		const userRelationship = await this.userRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.FRIEND,
			nickname: null,
			since: now,
			share_voice_activity: userDefaultStaff,
			version: 1,
		});
		const targetRelationship = await this.userRepository.upsertRelationship({
			source_user_id: targetId,
			target_user_id: userId,
			type: RelationshipTypes.FRIEND,
			nickname: null,
			since: now,
			share_voice_activity: targetDefaultStaff,
			version: 1,
		});
		const hadUserRelationship = userFriend != null || hasExtraUserRelationship;
		const hadTargetRelationship = targetFriend != null || hasExtraTargetRelationship;
		if (hadUserRelationship) {
			await this.dispatchRelationshipRemove({userId, targetId: targetId.toString()});
		}
		if (hadTargetRelationship) {
			await this.dispatchRelationshipRemove({userId: targetId, targetId: userId.toString()});
		}
		await this.dispatchRelationshipCreate({
			userId,
			relationship: userRelationship,
			userCacheService,
			requestCache,
		});
		await this.dispatchRelationshipCreate({
			userId: targetId,
			relationship: targetRelationship,
			userCacheService,
			requestCache,
		});
		return userRelationship;
	}

	private async validateFriendRequest({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<User> {
		if (userId === targetId) {
			throw new CannotSendFriendRequestToSelfError();
		}
		const requesterUser = await this.userRepository.findUnique(userId);
		if (!requesterUser) {
			throw new UnknownUserError();
		}
		if (this.isDeletedUser(requesterUser)) {
			throw new FriendRequestBlockedError();
		}
		if (requesterUser?.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotSendFriendRequestsError();
		}
		requireEmailVerified(requesterUser, 'friend_request');
		if (requesterUser?.isBot) {
			throw new BotsCannotSendFriendRequestsError();
		}
		const targetUser = await this.userRepository.findUnique(targetId);
		if (!targetUser) throw new UnknownUserError();
		if (this.isDeletedUser(targetUser)) {
			throw new FriendRequestBlockedError();
		}
		const targetIsFriendlyBot =
			targetUser.isBot && (targetUser.flags & UserFlags.FRIENDLY_BOT) === UserFlags.FRIENDLY_BOT;
		if (targetUser.isBot && !targetIsFriendlyBot) {
			throw new FriendRequestBlockedError();
		}
		if (targetUser.flags & UserFlags.APP_STORE_REVIEWER) {
			throw new FriendRequestBlockedError();
		}
		const requesterBlockedTarget = await this.userRepository.getRelationship(
			userId,
			targetId,
			RelationshipTypes.BLOCKED,
		);
		if (requesterBlockedTarget) {
			throw new CannotSendFriendRequestToBlockedUserError();
		}
		const targetBlockedRequester = await this.userRepository.getRelationship(
			targetId,
			userId,
			RelationshipTypes.BLOCKED,
		);
		if (targetBlockedRequester) {
			throw new FriendRequestBlockedError();
		}
		await this.userPermissionUtils.validateFriendSourcePermissions({userId, targetId});
		return targetUser;
	}

	private async validateRelationshipCounts({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		const targetUser = await this.userRepository.findUnique(targetId);
		if (!user?.isBot) {
			const userLimit = this.resolveLimitForUser(user ?? null, 'max_relationships', MAX_RELATIONSHIPS);
			const hasReachedLimit = await this.userRepository.hasReachedRelationshipLimit(userId, userLimit);
			if (hasReachedLimit) {
				throw new MaxRelationshipsError(userLimit);
			}
		}
		if (!targetUser?.isBot) {
			const targetLimit = this.resolveLimitForUser(targetUser ?? null, 'max_relationships', MAX_RELATIONSHIPS);
			const hasReachedLimit = await this.userRepository.hasReachedRelationshipLimit(targetId, targetLimit);
			if (hasReachedLimit) {
				throw new MaxRelationshipsError(targetLimit);
			}
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
		const userRelationship = await this.userRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.OUTGOING_REQUEST,
			nickname: null,
			since: now,
			share_voice_activity: true,
			version: 1,
		});
		const targetRelationship = await this.userRepository.upsertRelationship({
			source_user_id: targetId,
			target_user_id: userId,
			type: RelationshipTypes.INCOMING_REQUEST,
			nickname: null,
			since: now,
			share_voice_activity: true,
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

	private async createShadowFriendRequest({
		requesterUser,
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		requesterUser: User;
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		if (userId === targetId) {
			throw new CannotSendFriendRequestToSelfError();
		}
		if (this.isDeletedUser(requesterUser)) {
			throw new FriendRequestBlockedError();
		}
		if (requesterUser.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotSendFriendRequestsError();
		}
		requireEmailVerified(requesterUser, 'friend_request');
		if (requesterUser.isBot) {
			throw new BotsCannotSendFriendRequestsError();
		}
		const targetUser = await this.userRepository.findUnique(targetId);
		if (!targetUser) {
			throw new UnknownUserError();
		}
		if (this.isDeletedUser(targetUser)) {
			throw new FriendRequestBlockedError();
		}
		const targetIsFriendlyBot =
			targetUser.isBot && (targetUser.flags & UserFlags.FRIENDLY_BOT) === UserFlags.FRIENDLY_BOT;
		if (targetUser.isBot && !targetIsFriendlyBot) {
			throw new FriendRequestBlockedError();
		}
		const [requesterBlockedTarget, existingFriendship, existingOutgoingRequest] = await Promise.all([
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.BLOCKED),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.FRIEND),
			this.userRepository.getRelationship(userId, targetId, RelationshipTypes.OUTGOING_REQUEST),
		]);
		if (requesterBlockedTarget) {
			throw new CannotSendFriendRequestToBlockedUserError();
		}
		if (existingFriendship || existingOutgoingRequest) {
			const relationships = await this.userRepository.listRelationships(userId);
			const relationship = relationships.find((r) => r.targetUserId === targetId);
			if (relationship) {
				return relationship;
			}
		}
		const now = new Date();
		const userRelationship = await this.userRepository.upsertRelationship({
			source_user_id: userId,
			target_user_id: targetId,
			type: RelationshipTypes.OUTGOING_REQUEST,
			nickname: null,
			since: now,
			share_voice_activity: true,
			version: 1,
		});
		await this.dispatchRelationshipCreate({userId, relationship: userRelationship, userCacheService, requestCache});
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

	private resolveLimitForUser(user: User | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}

	private isDeletedUser(user: User | null | undefined): boolean {
		if (!user) {
			return false;
		}
		if (user.pendingDeletionAt !== null) {
			return false;
		}
		return (user.flags & UserFlags.DELETED) === UserFlags.DELETED;
	}
}
