// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {DELETED_USER_GLOBAL_NAME, DELETED_USER_USERNAME, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {describe, expect, test} from 'vitest';
import {type ChannelID, createChannelID, createUserID, type UserID} from '../../BrandedTypes';
import type {ChannelRow} from '../../database/types/ChannelTypes';
import type {RelationshipRow} from '../../database/types/UserTypes';
import {UserCacheService} from '../../infrastructure/UserCacheService';
import type {IUsersServiceClient} from '../../infrastructure/UsersServiceClient';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import {Channel} from '../../models/Channel';
import {Relationship} from '../../models/Relationship';
import {UserChannelRequestService} from '../services/UserChannelRequestService';
import type {UserChannelService} from '../services/UserChannelService';
import {UserRelationshipRequestService} from '../services/UserRelationshipRequestService';
import type {UserRelationshipService} from '../services/UserRelationshipService';

class RecordingUsersServiceClient implements IUsersServiceClient {
	readonly requests: Array<Array<UserID>> = [];

	constructor(private readonly partialsById: Map<UserID, UserPartialResponse>) {}

	async getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>> {
		this.requests.push([...userIds]);
		const result = new Map<UserID, UserPartialResponse>();
		for (const userId of userIds) {
			const partial = this.partialsById.get(userId);
			if (partial) {
				result.set(userId, partial);
			}
		}
		return result;
	}

	async invalidateUserCache(_userId: UserID): Promise<void> {}
}

function createPartial(userId: UserID, username: string): UserPartialResponse {
	return {
		id: userId.toString(),
		username,
		discriminator: '0001',
		global_name: null,
		avatar: null,
		avatar_color: null,
		flags: 0,
	};
}

function createPartials(userIds: Array<UserID>): Map<UserID, UserPartialResponse> {
	return new Map(userIds.map((userId, index) => [userId, createPartial(userId, `Partial${index}`)]));
}

function createRelationship(sourceUserId: UserID, targetUserId: UserID, shareVoiceActivity: boolean): Relationship {
	return new Relationship({
		source_user_id: sourceUserId,
		target_user_id: targetUserId,
		type: RelationshipTypes.FRIEND,
		nickname: null,
		since: null,
		share_voice_activity: shareVoiceActivity,
		version: 1,
	} satisfies RelationshipRow);
}

function createPrivateChannel(channelId: ChannelID, type: number, recipientIds: Set<UserID>): Channel {
	return new Channel({
		channel_id: channelId,
		guild_id: null,
		type,
		name: null,
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: null,
		position: null,
		owner_id: null,
		recipient_ids: recipientIds,
		nsfw: null,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: null,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 1,
	} satisfies ChannelRow);
}

function createRelationshipRequestService(
	relationships: Array<Relationship>,
	inverseRelationships: Map<UserID, Relationship>,
	userCacheService: UserCacheService,
): UserRelationshipRequestService {
	return new UserRelationshipRequestService(
		{
			getRelationships: async () => relationships,
			getRelationship: async (params: {userId: UserID}) => inverseRelationships.get(params.userId) ?? null,
		} as unknown as UserRelationshipService,
		{} as UserChannelService,
		userCacheService,
	);
}

describe('list endpoint user partial prefetch', () => {
	test('resolves every relationship from a single batched user partial fetch', async () => {
		const viewerId = createUserID(9000n);
		const targetIds = [createUserID(9001n), createUserID(9002n), createUserID(9003n), createUserID(9004n)];
		const partials = createPartials(targetIds);
		const usersServiceClient = new RecordingUsersServiceClient(partials);
		const relationships = targetIds.map((targetId) => createRelationship(viewerId, targetId, true));
		const inverseRelationships = new Map(
			targetIds.map((targetId) => [targetId, createRelationship(targetId, viewerId, false)]),
		);
		const service = createRelationshipRequestService(
			relationships,
			inverseRelationships,
			new UserCacheService(usersServiceClient),
		);

		const response = await service.listRelationships({userId: viewerId, requestCache: createRequestCache()});

		expect(usersServiceClient.requests).toEqual([targetIds]);
		expect(response).toEqual(
			targetIds.map((targetId) => ({
				id: targetId.toString(),
				type: RelationshipTypes.FRIEND,
				user: partials.get(targetId),
				nickname: null,
				share_voice_activity: true,
				friend_shares_voice_activity: false,
			})),
		);
	});

	test('keeps the deleted-user fallback for relationship targets the users service drops', async () => {
		const viewerId = createUserID(9100n);
		const knownId = createUserID(9101n);
		const missingId = createUserID(9102n);
		const usersServiceClient = new RecordingUsersServiceClient(createPartials([knownId]));
		const relationships = [createRelationship(viewerId, knownId, true), createRelationship(viewerId, missingId, true)];
		const service = createRelationshipRequestService(
			relationships,
			new Map(),
			new UserCacheService(usersServiceClient),
		);

		const response = await service.listRelationships({userId: viewerId, requestCache: createRequestCache()});

		expect(usersServiceClient.requests).toEqual([[knownId, missingId]]);
		expect(response[1]?.user).toMatchObject({
			id: missingId.toString(),
			username: DELETED_USER_USERNAME,
			global_name: DELETED_USER_GLOBAL_NAME,
		});
		expect(response[1]?.friend_shares_voice_activity).toBe(true);
	});

	test('resolves every private channel recipient from a single batched user partial fetch', async () => {
		const viewerId = createUserID(9200n);
		const friendId = createUserID(9201n);
		const groupMemberId = createUserID(9202n);
		const partials = createPartials([friendId, groupMemberId]);
		const usersServiceClient = new RecordingUsersServiceClient(partials);
		const channels = [
			createPrivateChannel(createChannelID(9300n), ChannelTypes.DM, new Set([viewerId, friendId])),
			createPrivateChannel(createChannelID(9301n), ChannelTypes.GROUP_DM, new Set([viewerId, friendId, groupMemberId])),
			createPrivateChannel(createChannelID(9302n), ChannelTypes.DM_PERSONAL_NOTES, new Set([viewerId])),
		];
		const service = new UserChannelRequestService(
			{getPrivateChannels: async () => channels} as unknown as UserChannelService,
			new UserCacheService(usersServiceClient),
		);

		const response = await service.listPrivateChannels({userId: viewerId, requestCache: createRequestCache()});

		expect(usersServiceClient.requests).toEqual([[friendId, groupMemberId]]);
		expect(response[0]?.recipients).toEqual([partials.get(friendId)]);
		expect(response[1]?.recipients).toEqual([partials.get(friendId), partials.get(groupMemberId)]);
		expect(response[2]?.recipients).toBeUndefined();
	});
});
