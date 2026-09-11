// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	BulkIgnoreFriendRequestsRequest,
	FriendRequestByTagRequest,
	FriendRequestCreateRequest,
	RelationshipNicknameUpdateRequest,
	RelationshipTypePutRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {
	BulkIgnoreFriendRequestsResponse,
	RelationshipListResponse,
	RelationshipResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

import {createUserID} from '../../BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function UserRelationshipController(app: HonoApp) {
	app.get(
		'/users/@me/relationships',
		RateLimitMiddleware(RateLimitConfigs.USER_RELATIONSHIPS_LIST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_user_relationships',
			summary: 'List user relationships',
			responseSchema: RelationshipListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves all relationships for the current user, including friends, friend requests (incoming and outgoing), and blocked users. Returns list of relationship objects with type and metadata.',
		}),
		async (ctx) => {
			const response = await ctx.get('userRelationshipRequestService').listRelationships({
				userId: ctx.get('user').id,
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/users/@me/relationships',
		RateLimitMiddleware(RateLimitConfigs.USER_FRIEND_REQUEST_SEND),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', FriendRequestByTagRequest),
		OpenAPI({
			operationId: 'send_friend_request_by_tag',
			summary: 'Send friend request by tag',
			responseSchema: RelationshipResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Sends a friend request to a user identified by username tag (username#discriminator). Returns the new relationship object. Can fail if user not found or request already sent.',
		}),
		async (ctx) => {
			const response = await ctx.get('userRelationshipRequestService').sendFriendRequestByTag({
				userId: ctx.get('user').id,
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/users/@me/relationships/bulk-ignore',
		RateLimitMiddleware(RateLimitConfigs.USER_BULK_IGNORE_FRIEND_REQUESTS),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', BulkIgnoreFriendRequestsRequest),
		OpenAPI({
			operationId: 'bulk_ignore_friend_requests',
			summary: 'Bulk ignore friend requests',
			responseSchema: BulkIgnoreFriendRequestsResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Ignores (removes) multiple incoming friend requests at once. Optionally filters by sender account age to target requests from new accounts.',
		}),
		async (ctx) => {
			const response = await ctx.get('userRelationshipRequestService').bulkIgnoreIncomingRequests({
				userId: ctx.get('user').id,
				data: ctx.req.valid('json'),
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_FRIEND_REQUEST_SEND),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', UserIdParam),
		Validator('json', FriendRequestCreateRequest),
		OpenAPI({
			operationId: 'send_friend_request',
			summary: 'Send friend request',
			responseSchema: RelationshipResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Sends a friend request to a user identified by user ID. Returns the new relationship object. Can fail if user not found or request already sent.',
		}),
		async (ctx) => {
			const response = await ctx.get('userRelationshipRequestService').sendFriendRequest({
				userId: ctx.get('user').id,
				targetId: createUserID(ctx.req.valid('param').user_id),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
	app.put(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_FRIEND_REQUEST_ACCEPT),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', UserIdParam),
		Validator('json', RelationshipTypePutRequest),
		OpenAPI({
			operationId: 'accept_or_update_friend_request',
			summary: 'Accept or update friend request',
			responseSchema: RelationshipResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Accepts a pending incoming friend request from a user or updates the relationship type. Can also be used to change friend relationship to blocked status. Returns updated relationship object.',
		}),
		async (ctx) => {
			const response = await ctx.get('userRelationshipRequestService').updateRelationshipType({
				userId: ctx.get('user').id,
				targetId: createUserID(ctx.req.valid('param').user_id),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
	app.delete(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_RELATIONSHIP_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'remove_relationship',
			summary: 'Remove relationship',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Removes a relationship with another user by ID. Removes friends, cancels friend requests (incoming or outgoing), or unblocks a blocked user depending on current relationship type.',
		}),
		async (ctx) => {
			await ctx.get('userRelationshipRequestService').removeRelationship({
				userId: ctx.get('user').id,
				targetId: createUserID(ctx.req.valid('param').user_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.patch(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_RELATIONSHIP_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', UserIdParam),
		Validator('json', RelationshipNicknameUpdateRequest),
		OpenAPI({
			operationId: 'update_relationship_nickname',
			summary: 'Update relationship nickname',
			responseSchema: RelationshipResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				"Updates the nickname associated with a relationship (friend or blocked user). Nicknames are personal labels that override the user's display name in the current user's view. Returns updated relationship object.",
		}),
		async (ctx) => {
			const response = await ctx.get('userRelationshipRequestService').updateNickname({
				userId: ctx.get('user').id,
				targetId: createUserID(ctx.req.valid('param').user_id),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
}
