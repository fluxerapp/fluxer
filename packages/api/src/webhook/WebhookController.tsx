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

import {createChannelID, createGuildID, createWebhookID, createWebhookToken} from '@fluxer/api/src/BrandedTypes';
import type {MessageRequest} from '@fluxer/api/src/channel/MessageTypes';
import {parseMultipartMessageData} from '@fluxer/api/src/channel/services/message/MessageRequestParser';
import {LoginRequired} from '@fluxer/api/src/middleware/AuthMiddleware';
import {BlockAppOriginMiddleware} from '@fluxer/api/src/middleware/BlockAppOriginMiddleware';
import {RateLimitMiddleware} from '@fluxer/api/src/middleware/RateLimitMiddleware';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@fluxer/api/src/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@fluxer/api/src/types/HonoEnv';
import {parseJsonPreservingLargeIntegers} from '@fluxer/api/src/utils/LosslessJsonParser';
import {Validator} from '@fluxer/api/src/Validator';
import type {WebhookExecuteMessageData} from '@fluxer/api/src/webhook/WebhookService';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	ChannelIdParam,
	GuildIdParam,
	WebhookIdParam,
	WebhookIdTokenParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {MessageRequestSchema} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {MessageResponseSchema} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import {SentryWebhook} from '@fluxer/schema/src/domains/webhook/SentryWebhookSchemas';
import {
	SlackWebhookRequest,
	WebhookCreateRequest,
	WebhookExecuteQueryRequest,
	WebhookMessageRequest,
	WebhookTokenUpdateRequest,
	WebhookUpdateRequest,
} from '@fluxer/schema/src/domains/webhook/WebhookRequestSchemas';
import {WebhookResponse, WebhookTokenResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import type {Context} from 'hono';
import {z} from 'zod';

function validateWebhookMessagePayload(data: unknown): WebhookMessageRequest {
	const validationResult = WebhookMessageRequest.safeParse(data);
	if (!validationResult.success) {
		throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}
	return validationResult.data;
}

async function parseWebhookJsonMessageData(ctx: Context<HonoEnv>): Promise<WebhookExecuteMessageData> {
	let data: unknown;
	try {
		const raw = await ctx.req.text();
		data = raw.trim().length === 0 ? {} : parseJsonPreservingLargeIntegers(raw);
	} catch {
		data = {};
	}
	return validateWebhookMessagePayload(data);
}

async function parseWebhookMultipartMessageData(
	ctx: Context<HonoEnv>,
	webhookId: bigint,
	token: string,
): Promise<WebhookExecuteMessageData> {
	const webhook = await ctx.get('webhookService').getWebhookByToken({
		webhookId: createWebhookID(webhookId),
		token: createWebhookToken(token),
	});

	if (!webhook.creatorId) {
		throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}

	const creator = await ctx.get('userRepository').findUnique(webhook.creatorId);
	if (!creator) {
		throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}

	let parsedPayload: unknown = null;
	const messageData: MessageRequest = await parseMultipartMessageData(
		ctx,
		creator,
		webhook.channelId!,
		MessageRequestSchema,
		{
			onPayloadParsed(payload) {
				parsedPayload = payload;
			},
		},
	);

	if (!parsedPayload) {
		throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}

	const webhookData = validateWebhookMessagePayload(parsedPayload);
	return {
		...webhookData,
		...messageData,
		username: webhookData.username,
		avatar_url: webhookData.avatar_url,
	};
}

export function WebhookController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/webhooks',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_LIST_GUILD),
		LoginRequired,
		OpenAPI({
			operationId: 'list_guild_webhooks',
			summary: 'List guild webhooks',
			description:
				'Returns a list of all webhooks configured in the specified guild. Requires the user to have appropriate permissions to view webhooks in the guild.',
			responseSchema: z.array(WebhookResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Webhooks'],
		}),
		Validator('param', GuildIdParam),
		async (ctx) => {
			const response = await ctx.get('webhookRequestService').listGuildWebhooks({
				userId: ctx.get('user').id,
				guildId: createGuildID(ctx.req.valid('param').guild_id),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);

	app.get(
		'/channels/:channel_id/webhooks',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_LIST_CHANNEL),
		LoginRequired,
		OpenAPI({
			operationId: 'list_channel_webhooks',
			summary: 'List channel webhooks',
			description:
				'Returns a list of all webhooks configured in the specified channel. Requires the user to have appropriate permissions to view webhooks in the channel.',
			responseSchema: z.array(WebhookResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Webhooks'],
		}),
		Validator('param', ChannelIdParam),
		async (ctx) => {
			const response = await ctx.get('webhookRequestService').listChannelWebhooks({
				userId: ctx.get('user').id,
				channelId: createChannelID(ctx.req.valid('param').channel_id),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);

	app.post(
		'/channels/:channel_id/webhooks',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_CREATE),
		LoginRequired,
		OpenAPI({
			operationId: 'create_webhook',
			summary: 'Create webhook',
			description:
				'Creates a new webhook in the specified channel with the provided name and optional avatar. Returns the newly created webhook object including its ID and token.',
			responseSchema: WebhookResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Webhooks'],
		}),
		Validator('param', ChannelIdParam),
		Validator('json', WebhookCreateRequest),
		async (ctx) => {
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const response = await ctx.get('webhookRequestService').createWebhook({
				userId: ctx.get('user').id,
				channelId: createChannelID(ctx.req.valid('param').channel_id),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
				auditLogReason,
			});
			return ctx.json(response);
		},
	);

	app.get(
		'/webhooks/:webhook_id',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_GET),
		LoginRequired,
		OpenAPI({
			operationId: 'get_webhook',
			summary: 'Get webhook',
			description:
				'Retrieves detailed information about a specific webhook by its ID. Requires authentication and appropriate permissions to access the webhook.',
			responseSchema: WebhookResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdParam),
		async (ctx) => {
			const response = await ctx.get('webhookRequestService').getWebhook({
				userId: ctx.get('user').id,
				webhookId: createWebhookID(ctx.req.valid('param').webhook_id),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);

	app.patch(
		'/webhooks/:webhook_id',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_UPDATE),
		LoginRequired,
		OpenAPI({
			operationId: 'update_webhook',
			summary: 'Update webhook',
			description:
				'Updates the specified webhook with new settings such as name, avatar, or target channel. All fields are optional. Returns the updated webhook object.',
			responseSchema: WebhookResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdParam),
		Validator('json', WebhookUpdateRequest),
		async (ctx) => {
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const response = await ctx.get('webhookRequestService').updateWebhook({
				userId: ctx.get('user').id,
				webhookId: createWebhookID(ctx.req.valid('param').webhook_id),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
				auditLogReason,
			});
			return ctx.json(response);
		},
	);

	app.delete(
		'/webhooks/:webhook_id',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_DELETE),
		LoginRequired,
		OpenAPI({
			operationId: 'delete_webhook',
			summary: 'Delete webhook',
			description:
				'Permanently deletes the specified webhook. This action cannot be undone. Returns a 204 status code on successful deletion.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdParam),
		async (ctx) => {
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('webhookRequestService').deleteWebhook({
				userId: ctx.get('user').id,
				webhookId: createWebhookID(ctx.req.valid('param').webhook_id),
				auditLogReason,
			});
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/webhooks/:webhook_id/:token',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_GET),
		OpenAPI({
			operationId: 'get_webhook_with_token',
			summary: 'Get webhook with token',
			description:
				'Retrieves detailed information about a specific webhook using its ID and token. No authentication required as the token serves as the credential. Returns the webhook object without creator user data.',
			responseSchema: WebhookTokenResponse,
			statusCode: 200,
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdTokenParam),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			const response = await ctx.get('webhookRequestService').getWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);

	app.patch(
		'/webhooks/:webhook_id/:token',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_UPDATE),
		OpenAPI({
			operationId: 'update_webhook_with_token',
			summary: 'Update webhook with token',
			description:
				'Updates the specified webhook using its ID and token for authentication. Allows modification of name or avatar. Returns the updated webhook object without creator user data.',
			responseSchema: WebhookTokenResponse,
			statusCode: 200,
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdTokenParam),
		Validator('json', WebhookTokenUpdateRequest),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			const response = await ctx.get('webhookRequestService').updateWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);

	app.delete(
		'/webhooks/:webhook_id/:token',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_DELETE),
		OpenAPI({
			operationId: 'delete_webhook_with_token',
			summary: 'Delete webhook with token',
			description:
				'Permanently deletes the specified webhook using its ID and token for authentication. This action cannot be undone. Returns a 204 status code on successful deletion.',
			responseSchema: null,
			statusCode: 204,
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdTokenParam),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			await ctx.get('webhookRequestService').deleteWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
			});
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/webhooks/:webhook_id/:token',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_EXECUTE),
		BlockAppOriginMiddleware,
		OpenAPI({
			operationId: 'execute_webhook',
			summary: 'Execute webhook',
			description:
				'Executes the webhook by sending a message to its configured channel. If the wait query parameter is true, returns the created message object; otherwise returns a 204 status with no content.',
			responseSchema: MessageResponseSchema,
			statusCode: 200,
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdTokenParam),
		Validator('query', WebhookExecuteQueryRequest),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			const {wait} = ctx.req.valid('query');
			const contentType = ctx.req.header('content-type') ?? '';
			const data = contentType.includes('multipart/form-data')
				? await parseWebhookMultipartMessageData(ctx, webhookId, token)
				: await parseWebhookJsonMessageData(ctx);
			const response = await ctx.get('webhookRequestService').executeWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
				data,
				wait,
				requestCache: ctx.get('requestCache'),
			});
			if (!response) {
				return ctx.body(null, 204);
			}
			return ctx.json(response);
		},
	);

	app.post(
		'/webhooks/:webhook_id/:token/github',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_GITHUB),
		OpenAPI({
			operationId: 'execute_github_webhook',
			summary: 'Execute GitHub webhook',
			description:
				'Receives and processes GitHub webhook events, formatting them as messages in the configured channel. Reads event type from X-GitHub-Event header and delivery ID from X-GitHub-Delivery header.',
			responseSchema: null,
			statusCode: 204,
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdTokenParam),
		Validator('json', GitHubWebhook),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			await ctx.get('webhookRequestService').executeGitHubWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
				event: ctx.req.header('X-GitHub-Event') ?? '',
				delivery: ctx.req.header('X-GitHub-Delivery') ?? '',
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/webhooks/:webhook_id/:token/slack',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_EXECUTE),
		OpenAPI({
			operationId: 'execute_slack_webhook',
			summary: 'Execute Slack webhook',
			description:
				'Receives and processes Slack-formatted webhook payloads, converting them to messages in the configured channel. Returns "ok" as plain text with a 200 status code.',
			responseSchema: z.string(),
			statusCode: 200,
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdTokenParam),
		Validator('json', SlackWebhookRequest),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			await ctx.get('webhookRequestService').executeSlackWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			ctx.header('Content-Type', 'text/html; charset=utf-8');
			return ctx.body('ok', 200);
		},
	);

	app.post(
		'/webhooks/:webhook_id/:token/sentry',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_GITHUB),
		OpenAPI({
			operationId: 'execute_sentry_webhook',
			summary: 'Execute Sentry webhook',
			description:
				'Receives and processes Sentry error tracking webhook events, formatting them as messages in the configured channel. Reads event type from X-Sentry-Event header.',
			responseSchema: null,
			statusCode: 204,
			tags: ['Webhooks'],
		}),
		Validator('param', WebhookIdTokenParam),
		Validator('json', SentryWebhook),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			await ctx.get('webhookRequestService').executeSentryWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
				event: ctx.req.header('X-Sentry-Event') ?? '',
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.body(null, 204);
		},
	);

	app.post('/webhooks/livekit', async (ctx) => {
		const response = await ctx.get('webhookRequestService').handleLiveKitWebhook({
			body: await ctx.req.text(),
			authHeader: ctx.req.header('Authorization') ?? undefined,
		});
		return response;
	});

	app.post('/webhooks/sweego', async (ctx) => {
		const response = await ctx.get('webhookRequestService').handleSweegoWebhook({
			body: await ctx.req.text(),
			webhookId: ctx.req.header('webhook-id') ?? undefined,
			timestamp: ctx.req.header('webhook-timestamp') ?? undefined,
			signature: ctx.req.header('webhook-signature') ?? undefined,
		});
		return response;
	});
}
