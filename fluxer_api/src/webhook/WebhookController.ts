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

import type {HonoApp} from '~/App';
import {AttachmentDecayService} from '~/attachment/AttachmentDecayService';
import {createChannelID, createGuildID, createWebhookID, createWebhookToken} from '~/BrandedTypes';
import {Config} from '~/Config';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import {collectMessageAttachments} from '~/channel/services/message/MessageHelpers';
import {Logger} from '~/Logger';
import {LoginRequired} from '~/middleware/AuthMiddleware';
import {BlockAppOriginMiddleware} from '~/middleware/BlockAppOriginMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';
import {GitHubWebhook} from '~/webhook/transformers/GitHubTransformer';
import {
	mapWebhooksToResponse,
	mapWebhookToResponseWithCache,
	WebhookCreateRequest,
	WebhookMessageRequest,
	WebhookUpdateRequest,
} from '~/webhook/WebhookModel';

export const WebhookController = (app: HonoApp) => {
	const decayService = new AttachmentDecayService();

	app.get(
		'/guilds/:guild_id/webhooks',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_LIST_GUILD),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			return ctx.json(
				await mapWebhooksToResponse({
					webhooks: await ctx.get('webhookService').getGuildWebhooks({
						userId: ctx.get('user').id,
						guildId: createGuildID(ctx.req.valid('param').guild_id),
					}),
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.get(
		'/channels/:channel_id/webhooks',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_LIST_CHANNEL),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			return ctx.json(
				await mapWebhooksToResponse({
					webhooks: await ctx.get('webhookService').getChannelWebhooks({
						userId: ctx.get('user').id,
						channelId: createChannelID(ctx.req.valid('param').channel_id),
					}),
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.post(
		'/channels/:channel_id/webhooks',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_CREATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		Validator('json', WebhookCreateRequest),
		async (ctx) => {
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await mapWebhookToResponseWithCache({
					webhook: await ctx.get('webhookService').createWebhook(
						{
							userId: ctx.get('user').id,
							channelId: createChannelID(ctx.req.valid('param').channel_id),
							data: ctx.req.valid('json'),
						},
						auditLogReason,
					),
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.get(
		'/webhooks/:webhook_id',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_GET),
		LoginRequired,
		Validator('param', z.object({webhook_id: Int64Type})),
		async (ctx) => {
			return ctx.json(
				await mapWebhookToResponseWithCache({
					webhook: await ctx.get('webhookService').getWebhook({
						userId: ctx.get('user').id,
						webhookId: createWebhookID(ctx.req.valid('param').webhook_id),
					}),
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.patch(
		'/webhooks/:webhook_id',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_UPDATE),
		LoginRequired,
		Validator('param', z.object({webhook_id: Int64Type})),
		Validator('json', WebhookUpdateRequest),
		async (ctx) => {
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await mapWebhookToResponseWithCache({
					webhook: await ctx.get('webhookService').updateWebhook(
						{
							userId: ctx.get('user').id,
							webhookId: createWebhookID(ctx.req.valid('param').webhook_id),
							data: ctx.req.valid('json'),
						},
						auditLogReason,
					),
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.delete(
		'/webhooks/:webhook_id',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_DELETE),
		LoginRequired,
		Validator('param', z.object({webhook_id: Int64Type})),
		async (ctx) => {
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('webhookService').deleteWebhook(
				{
					userId: ctx.get('user').id,
					webhookId: createWebhookID(ctx.req.valid('param').webhook_id),
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/webhooks/:webhook_id/:token',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_GET),
		Validator('param', z.object({webhook_id: Int64Type, token: createStringType()})),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			return ctx.json(
				await mapWebhookToResponseWithCache({
					webhook: await ctx
						.get('webhookService')
						.getWebhookByToken({webhookId: createWebhookID(webhookId), token: createWebhookToken(token)}),
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.patch(
		'/webhooks/:webhook_id/:token',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_UPDATE),
		Validator('param', z.object({webhook_id: Int64Type, token: createStringType()})),
		Validator('json', WebhookUpdateRequest),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			return ctx.json(
				await mapWebhookToResponseWithCache({
					webhook: await ctx.get('webhookService').updateWebhookByToken({
						webhookId: createWebhookID(webhookId),
						token: createWebhookToken(token),
						data: ctx.req.valid('json'),
					}),
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.delete(
		'/webhooks/:webhook_id/:token',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_DELETE),
		Validator('param', z.object({webhook_id: Int64Type, token: createStringType()})),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			await ctx.get('webhookService').deleteWebhookByToken({
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
		Validator('param', z.object({webhook_id: Int64Type, token: createStringType()})),
		Validator('json', WebhookMessageRequest),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			const wait = ctx.req.query('wait') === 'true';

			const message = await ctx.get('webhookService').executeWebhook({
				webhookId: createWebhookID(webhookId),
				token: createWebhookToken(token),
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});

			if (wait) {
				const messageAttachments = collectMessageAttachments(message);
				const attachmentDecayMap =
					messageAttachments.length > 0
						? await decayService.fetchMetadata(messageAttachments.map((att) => ({attachmentId: att.id})))
						: undefined;
				return ctx.json(
					await mapMessageToResponse({
						message,
						userCacheService: ctx.get('userCacheService'),
						requestCache: ctx.get('requestCache'),
						mediaService: ctx.get('mediaService'),
						attachmentDecayMap,
						getReferencedMessage: (channelId, messageId) =>
							ctx.get('channelRepository').getMessage(channelId, messageId),
					}),
				);
			}

			return ctx.body(null, 204);
		},
	);

	app.post(
		'/webhooks/:webhook_id/:token/github',
		RateLimitMiddleware(RateLimitConfigs.WEBHOOK_GITHUB),
		Validator('param', z.object({webhook_id: Int64Type, token: createStringType()})),
		Validator('json', GitHubWebhook),
		async (ctx) => {
			const {webhook_id: webhookId, token} = ctx.req.valid('param');
			await ctx.get('webhookService').executeGitHubWebhook({
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

	app.post('/webhooks/livekit', async (ctx) => {
		if (!Config.voice.enabled) {
			return ctx.body('Voice not enabled', 404);
		}

		const liveKitWebhookService = ctx.get('liveKitWebhookService');
		if (!liveKitWebhookService) {
			return ctx.body('LiveKit webhook service not available', 503);
		}

		try {
			const body = await ctx.req.text();
			const authHeader = ctx.req.header('Authorization') ?? '';
			const data = await liveKitWebhookService.verifyAndParse(body, authHeader);
			await liveKitWebhookService.processEvent(data);
			return ctx.body(null, 200);
		} catch (error) {
			Logger.debug({error}, 'Error processing LiveKit webhook');
			return ctx.body('Invalid webhook', 400);
		}
	});

	app.post('/webhooks/sendgrid', async (ctx) => {
		if (!Config.email.enabled) {
			return ctx.body('Email not enabled', 404);
		}

		const sendGridWebhookService = ctx.get('sendGridWebhookService');
		if (!sendGridWebhookService) {
			return ctx.body('SendGrid webhook service not available', 503);
		}

		try {
			const body = await ctx.req.text();

			if (Config.email.webhookPublicKey) {
				const signature = ctx.req.header('X-Twilio-Email-Event-Webhook-Signature');
				const timestamp = ctx.req.header('X-Twilio-Email-Event-Webhook-Timestamp');

				if (!signature || !timestamp) {
					Logger.warn('SendGrid webhook missing signature headers');
					return ctx.body('Missing signature headers', 401);
				}

				const isValid = sendGridWebhookService.verifySignature(
					body,
					signature,
					timestamp,
					Config.email.webhookPublicKey,
				);

				if (!isValid) {
					Logger.warn('SendGrid webhook signature verification failed');
					return ctx.body('Invalid signature', 401);
				}
			}

			const events = JSON.parse(body);
			await sendGridWebhookService.processEvents(events);

			return ctx.body(null, 200);
		} catch (error) {
			Logger.error({error}, 'Error processing SendGrid webhook');
			return ctx.body('Invalid webhook', 400);
		}
	});
};
