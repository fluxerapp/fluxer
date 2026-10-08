// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import type {MessageRequest} from '@app/api/channel/MessageTypes';
import {normalizeMessageRequestPayload} from '@app/api/channel/services/message/MessageRequestCompatibility';
import {parseMultipartMessageData} from '@app/api/channel/services/message/MessageRequestParser';
import type {ForumPostInput} from '@app/api/channel/services/thread/ThreadCreationService';
import {ChannelThreadsRouteGuard} from '@app/api/channel/threads/ChannelThreadsRouteGuard';
import {viewerFromCtx} from '@app/api/experiment/ChannelThreadsGate';
import {BotOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {parseJsonPreservingLargeIntegers} from '@app/api/utils/LosslessJsonParser';
import {inputValidationErrorFromZodIssues, Validator} from '@app/api/Validator';
import {TEXT_THREAD_PARENT_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	ForumThreadMessageRequest,
	StartForumThreadRequest,
	StartForumThreadResponse,
	StartThreadMultipartRequest,
	StartThreadRequestBody,
} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import {
	ActiveThreadsResponse,
	ArchivedThreadsQuery,
	ArchivedThreadsResponse,
	JoinedArchivedThreadsQuery,
	StartThreadFromMessageRequest,
	StartThreadRequest,
	ThreadChannelResponse,
	ThreadLocationQuery,
	ThreadMemberGetQuery,
	ThreadMemberListResponse,
	ThreadMembersListQuery,
} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {
	ChannelIdMessageIdParam,
	ChannelIdParam,
	ChannelIdUserIdParam,
	GuildIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import type {Context, MiddlewareHandler} from 'hono';
import {z} from 'zod';

const EXPERIMENT = 'channel_threads';
const TAGS = 'Channels';

function isMultipart(ctx: Context<HonoEnv>): boolean {
	return (ctx.req.header('content-type') ?? '').includes('multipart/form-data');
}

async function readPayloadJson(ctx: Context<HonoEnv>): Promise<unknown> {
	let payloadJson: unknown;
	try {
		payloadJson = (await ctx.req.parseBody())['payload_json'];
	} catch {
		throw InputValidationError.fromCode('multipart_form', ValidationErrorCodes.FAILED_TO_PARSE_MULTIPART_FORM_DATA);
	}
	if (payloadJson === undefined) return {};
	if (typeof payloadJson === 'string') {
		try {
			return parseJsonPreservingLargeIntegers(payloadJson);
		} catch {}
	}
	throw InputValidationError.fromCode('payload_json', ValidationErrorCodes.INVALID_JSON_IN_PAYLOAD_JSON);
}

async function readJsonBody(ctx: Context<HonoEnv>): Promise<unknown> {
	if (isMultipart(ctx)) return readPayloadJson(ctx);
	try {
		const raw = await ctx.req.text();
		return raw.trim().length === 0 ? {} : parseJsonPreservingLargeIntegers(raw);
	} catch {
		throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}
}

function parseWithSchema<T extends z.ZodType>(schema: T, value: unknown): z.output<T> {
	const result = schema.safeParse(value);
	if (!result.success) throw inputValidationErrorFromZodIssues(result.error.issues);
	return result.data;
}

function SelfThreadMemberAlias(action: 'join' | 'leave'): MiddlewareHandler<HonoEnv> {
	return async (ctx, next) => {
		if (ctx.req.param('user_id') !== '@me') return next();
		const {channel_id} = parseWithSchema(ChannelIdParam, {channel_id: ctx.req.param('channel_id')});
		await ctx.get('threadService').members[action]({
			viewer: viewerFromCtx(ctx),
			user: ctx.get('user'),
			channelId: createChannelID(channel_id),
		});
		return ctx.body(null, 204);
	};
}

function normalizeForumEnvelope(value: unknown): unknown {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) return value;
	const envelope = value as Record<string, unknown>;
	return {...envelope, message: normalizeMessageRequestPayload(envelope.message)};
}

const ForumPostMultipartMessage = z.preprocess(
	(value) =>
		typeof value === 'object' && value !== null && !Array.isArray(value)
			? ((value as Record<string, unknown>).message ?? {})
			: {},
	ForumThreadMessageRequest,
);

async function parseForumPostBody(ctx: Context<HonoEnv>, channelId: ChannelID): Promise<ForumPostInput> {
	if (!isMultipart(ctx)) {
		return parseWithSchema(StartForumThreadRequest, normalizeForumEnvelope(await readJsonBody(ctx))) as ForumPostInput;
	}
	let envelope: unknown = null;
	const message = (await parseMultipartMessageData(
		ctx,
		ctx.get('user'),
		channelId,
		ForumPostMultipartMessage as unknown as z.ZodType<MessageRequest>,
		{
			onPayloadParsed(payload) {
				envelope = payload;
			},
		},
	)) as MessageRequest;
	const fields = parseWithSchema(StartForumThreadRequest.omit({message: true}), envelope ?? {});
	return {...fields, message};
}

export function ThreadController(app: HonoApp) {
	app.post(
		'/channels/:channel_id/messages/:message_id/threads',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_CREATE),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		Validator('json', StartThreadFromMessageRequest),
		OpenAPI({
			operationId: 'start_thread_from_message',
			summary: 'Start a thread from a message',
			description:
				'Creates a public thread from an existing message in a text channel. The thread shares the ID of the message, so a message can start one thread.',
			responseSchema: ThreadChannelResponse,
			statusCode: 201,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const body = ctx.req.valid('json');
			const thread = await ctx.get('threadService').creation.createFromMessage({
				viewer: viewerFromCtx(ctx),
				user: ctx.get('user'),
				channelId: createChannelID(channel_id),
				messageId: createMessageID(message_id),
				input: {
					name: body.name,
					autoArchiveDuration: body.auto_archive_duration,
					rateLimitPerUser: body.rate_limit_per_user,
				},
				requestCache: ctx.get('requestCache'),
				auditLogReason: ctx.get('auditLogReason') ?? null,
			});
			return ctx.json(thread, 201);
		},
	);
	app.post(
		'/channels/:channel_id/threads',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_CREATE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'start_thread',
			summary: 'Start a thread',
			description:
				'Creates a thread that is not attached to an existing message. In a text channel the thread type is required. In a forum or media channel this creates a post, and the body carries the first message. The body can also be sent as multipart form data with the JSON in a payload_json field, and a post can attach files as files[n] parts.',
			requestSchema: StartThreadRequestBody,
			requestFormSchema: StartThreadMultipartRequest,
			responseSchema: StartForumThreadResponse,
			statusCode: 201,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const viewer = viewerFromCtx(ctx);
			const user = ctx.get('user');
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const creation = ctx.get('threadService').creation;
			const parentAuth = await creation.authenticateParent(viewer, user.id, channelId);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			if (parentAuth.channel.isThreadOnly()) {
				const body = await parseForumPostBody(ctx, channelId);
				return ctx.json(
					await creation.createForumPost({
						viewer,
						user,
						parentAuth,
						body,
						requestCache: ctx.get('requestCache'),
						auditLogReason,
					}),
					201,
				);
			}
			if (!TEXT_THREAD_PARENT_CHANNEL_TYPES.has(parentAuth.channel.type)) throw new InvalidChannelTypeError();
			const body = parseWithSchema(StartThreadRequest, await readJsonBody(ctx));
			const thread = await creation.createTextThread({
				user,
				parentAuth,
				type: body.type,
				invitable: body.invitable,
				input: {
					name: body.name,
					autoArchiveDuration: body.auto_archive_duration,
					rateLimitPerUser: body.rate_limit_per_user,
				},
				auditLogReason,
			});
			return ctx.json(thread, 201);
		},
	);
	app.get(
		'/guilds/:guild_id/threads/active',
		ChannelThreadsRouteGuard({botOnly: true}),
		RateLimitMiddleware(RateLimitConfigs.GUILD_THREADS_ACTIVE),
		LoginRequired,
		BotOnly,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_active_threads',
			summary: 'List active guild threads',
			description:
				'Returns every active thread in the guild that the bot can view, newest first, with a thread member object for each thread the bot joined.',
			responseSchema: ActiveThreadsResponse,
			statusCode: 200,
			security: ['botToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('threadService').lists.listGuildActive({
					userId: ctx.get('user').id,
					guildId: createGuildID(ctx.req.valid('param').guild_id),
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/threads/archived/public',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREADS_ARCHIVED_LIST),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ArchivedThreadsQuery),
		OpenAPI({
			operationId: 'list_public_archived_threads',
			summary: 'List public archived threads',
			description:
				'Returns archived public threads of the channel, most recently archived first. Requires the read message history permission.',
			responseSchema: ArchivedThreadsResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {before, limit} = ctx.req.valid('query');
			return ctx.json(
				await ctx.get('threadService').lists.listPublicArchived({
					viewer: viewerFromCtx(ctx),
					userId: ctx.get('user').id,
					channelId: createChannelID(ctx.req.valid('param').channel_id),
					before: before ? new Date(before) : undefined,
					limit,
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/threads/archived/private',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREADS_ARCHIVED_LIST),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ArchivedThreadsQuery),
		OpenAPI({
			operationId: 'list_private_archived_threads',
			summary: 'List private archived threads',
			description:
				'Returns archived private threads of the text channel, most recently archived first. Requires the read message history and manage threads permissions.',
			responseSchema: ArchivedThreadsResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {before, limit} = ctx.req.valid('query');
			return ctx.json(
				await ctx.get('threadService').lists.listPrivateArchived({
					viewer: viewerFromCtx(ctx),
					userId: ctx.get('user').id,
					channelId: createChannelID(ctx.req.valid('param').channel_id),
					before: before ? new Date(before) : undefined,
					limit,
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/users/@me/threads/archived/private',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREADS_ARCHIVED_LIST),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', JoinedArchivedThreadsQuery),
		OpenAPI({
			operationId: 'list_joined_private_archived_threads',
			summary: 'List joined private archived threads',
			description:
				'Returns archived private threads of the text channel that the current user joined, newest first. Requires the read message history permission.',
			responseSchema: ArchivedThreadsResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {before, limit} = ctx.req.valid('query');
			return ctx.json(
				await ctx.get('threadService').lists.listJoinedPrivateArchived({
					viewer: viewerFromCtx(ctx),
					userId: ctx.get('user').id,
					channelId: createChannelID(ctx.req.valid('param').channel_id),
					before: before !== undefined ? createChannelID(before) : undefined,
					limit,
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/thread-members',
		ChannelThreadsRouteGuard({botOnly: true}),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_MEMBERS_LIST),
		LoginRequired,
		BotOnly,
		Validator('param', ChannelIdParam),
		Validator('query', ThreadMembersListQuery),
		OpenAPI({
			operationId: 'list_thread_members',
			summary: 'List thread members',
			description:
				'Returns thread members ordered by user ID. Paginate with after and limit. Set with_member to include the guild member object of each thread member.',
			responseSchema: ThreadMemberListResponse,
			statusCode: 200,
			security: ['botToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {with_member, after, limit} = ctx.req.valid('query');
			return ctx.json(
				await ctx.get('threadService').members.list({
					viewer: viewerFromCtx(ctx),
					user: ctx.get('user'),
					channelId: createChannelID(ctx.req.valid('param').channel_id),
					after: after !== undefined ? createUserID(after) : undefined,
					limit,
					withMember: with_member,
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);
	app.put(
		'/channels/:channel_id/thread-members/@me',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_MEMBER_PUT),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ThreadLocationQuery),
		OpenAPI({
			operationId: 'join_thread',
			summary: 'Join a thread',
			description: 'Adds the current user to the thread. The thread must not be archived.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			await ctx.get('threadService').members.join({
				viewer: viewerFromCtx(ctx),
				user: ctx.get('user'),
				channelId: createChannelID(ctx.req.valid('param').channel_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/thread-members/@me',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_MEMBER_DELETE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ThreadLocationQuery),
		OpenAPI({
			operationId: 'leave_thread',
			summary: 'Leave a thread',
			description: 'Removes the current user from the thread. The thread must not be archived.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			await ctx.get('threadService').members.leave({
				viewer: viewerFromCtx(ctx),
				user: ctx.get('user'),
				channelId: createChannelID(ctx.req.valid('param').channel_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/channels/:channel_id/thread-members/:user_id',
		ChannelThreadsRouteGuard({botOnly: true}),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_MEMBER_GET),
		LoginRequired,
		BotOnly,
		Validator('param', ChannelIdUserIdParam),
		Validator('query', ThreadMemberGetQuery),
		OpenAPI({
			operationId: 'get_thread_member',
			summary: 'Get a thread member',
			description: 'Returns the thread member object of the user when the user is a member of the thread.',
			responseSchema: ThreadMemberResponse,
			statusCode: 200,
			security: ['botToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {channel_id, user_id} = ctx.req.valid('param');
			return ctx.json(
				await ctx.get('threadService').members.get({
					viewer: viewerFromCtx(ctx),
					user: ctx.get('user'),
					channelId: createChannelID(channel_id),
					targetId: createUserID(user_id),
					withMember: ctx.req.valid('query').with_member,
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);
	app.put(
		'/channels/:channel_id/thread-members/:user_id',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_MEMBER_PUT),
		LoginRequired,
		SelfThreadMemberAlias('join'),
		Validator('param', ChannelIdUserIdParam),
		Validator('query', ThreadLocationQuery),
		OpenAPI({
			operationId: 'add_thread_member',
			summary: 'Add a thread member',
			description:
				'Adds another guild member to the thread. Requires permission to send messages in threads, and the thread must not be archived.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {channel_id, user_id} = ctx.req.valid('param');
			await ctx.get('threadService').members.add({
				viewer: viewerFromCtx(ctx),
				user: ctx.get('user'),
				channelId: createChannelID(channel_id),
				targetId: createUserID(user_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/thread-members/:user_id',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_MEMBER_DELETE),
		LoginRequired,
		SelfThreadMemberAlias('leave'),
		Validator('param', ChannelIdUserIdParam),
		Validator('query', ThreadLocationQuery),
		OpenAPI({
			operationId: 'remove_thread_member',
			summary: 'Remove a thread member',
			description:
				'Removes a member from the thread. Requires the manage threads permission, or being the creator of a private thread. The thread must not be archived.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {channel_id, user_id} = ctx.req.valid('param');
			await ctx.get('threadService').members.remove({
				viewer: viewerFromCtx(ctx),
				user: ctx.get('user'),
				channelId: createChannelID(channel_id),
				targetId: createUserID(user_id),
			});
			return ctx.body(null, 204);
		},
	);
}
