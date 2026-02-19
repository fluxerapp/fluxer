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

import {AttachmentDecayService} from '@fluxer/api/src/attachment/AttachmentDecayService';
import type {ChannelID, GuildID, UserID, WebhookID, WebhookToken} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import {mapMessageToResponse} from '@fluxer/api/src/channel/MessageMappers';
import {collectMessageAttachments} from '@fluxer/api/src/channel/services/message/MessageHelpers';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import type {LiveKitWebhookService} from '@fluxer/api/src/infrastructure/LiveKitWebhookService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {SweegoWebhookService} from '@fluxer/api/src/webhook/SweegoWebhookService';
import {transformSlackWebhookRequest} from '@fluxer/api/src/webhook/transformers/SlackTransformer';
import {
	mapWebhooksToResponse,
	mapWebhookToResponseWithCache,
	mapWebhookToTokenResponse,
} from '@fluxer/api/src/webhook/WebhookModel';
import type {WebhookExecuteMessageData, WebhookService} from '@fluxer/api/src/webhook/WebhookService';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import type {SentryWebhook} from '@fluxer/schema/src/domains/webhook/SentryWebhookSchemas';
import type {
	SlackWebhookRequest,
	WebhookCreateRequest,
	WebhookTokenUpdateRequest,
	WebhookUpdateRequest,
} from '@fluxer/schema/src/domains/webhook/WebhookRequestSchemas';
import type {WebhookResponse, WebhookTokenResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';

type WebhookExecutionResponse = MessageResponse | null;

interface WebhookListGuildParams {
	userId: UserID;
	guildId: GuildID;
	requestCache: RequestCache;
}

interface WebhookListChannelParams {
	userId: UserID;
	channelId: ChannelID;
	requestCache: RequestCache;
}

interface WebhookCreateParams {
	userId: UserID;
	channelId: ChannelID;
	data: WebhookCreateRequest;
	requestCache: RequestCache;
	auditLogReason?: string | null;
}

interface WebhookGetByUserParams {
	userId: UserID;
	webhookId: WebhookID;
	requestCache: RequestCache;
}

interface WebhookGetByTokenParams {
	webhookId: WebhookID;
	token: WebhookToken;
	requestCache: RequestCache;
}

type WebhookGetParams = WebhookGetByUserParams | WebhookGetByTokenParams;

interface WebhookUpdateByUserParams {
	userId: UserID;
	webhookId: WebhookID;
	data: WebhookUpdateRequest;
	requestCache: RequestCache;
	auditLogReason?: string | null;
}

interface WebhookUpdateByTokenParams {
	webhookId: WebhookID;
	token: WebhookToken;
	data: WebhookTokenUpdateRequest;
	requestCache: RequestCache;
}

type WebhookUpdateParams = WebhookUpdateByUserParams | WebhookUpdateByTokenParams;

interface WebhookDeleteByUserParams {
	userId: UserID;
	webhookId: WebhookID;
	auditLogReason?: string | null;
}

interface WebhookDeleteByTokenParams {
	webhookId: WebhookID;
	token: WebhookToken;
}

type WebhookDeleteParams = WebhookDeleteByUserParams | WebhookDeleteByTokenParams;

interface WebhookExecuteParams {
	webhookId: WebhookID;
	token: WebhookToken;
	data: WebhookExecuteMessageData;
	wait: boolean;
	requestCache: RequestCache;
}

interface WebhookExecuteGitHubParams {
	webhookId: WebhookID;
	token: WebhookToken;
	event: string;
	delivery: string;
	data: GitHubWebhook;
	requestCache: RequestCache;
}

interface WebhookExecuteSentryParams {
	webhookId: WebhookID;
	token: WebhookToken;
	event: string;
	data: SentryWebhook;
	requestCache: RequestCache;
}

interface WebhookExecuteSlackParams {
	webhookId: WebhookID;
	token: WebhookToken;
	data: SlackWebhookRequest;
	requestCache: RequestCache;
}

interface LiveKitWebhookParams {
	body: string;
	authHeader?: string;
}

interface SweegoWebhookParams {
	body: string;
	webhookId?: string;
	timestamp?: string;
	signature?: string;
}

export class WebhookRequestService {
	private readonly decayService = new AttachmentDecayService();

	constructor(
		private readonly webhookService: WebhookService,
		private readonly channelRepository: IChannelRepository,
		private readonly userCacheService: UserCacheService,
		private readonly mediaService: IMediaService,
		private readonly liveKitWebhookService: LiveKitWebhookService | null,
		private readonly sweegoWebhookService: SweegoWebhookService,
	) {}

	async listGuildWebhooks(params: WebhookListGuildParams): Promise<Array<WebhookResponse>> {
		const webhooks = await this.webhookService.getGuildWebhooks({
			userId: params.userId,
			guildId: params.guildId,
		});
		return mapWebhooksToResponse({
			webhooks,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async listChannelWebhooks(params: WebhookListChannelParams): Promise<Array<WebhookResponse>> {
		const webhooks = await this.webhookService.getChannelWebhooks({
			userId: params.userId,
			channelId: params.channelId,
		});
		return mapWebhooksToResponse({
			webhooks,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async createWebhook(params: WebhookCreateParams): Promise<WebhookResponse> {
		const webhook = await this.webhookService.createWebhook(
			{
				userId: params.userId,
				channelId: params.channelId,
				data: params.data,
			},
			params.auditLogReason ?? null,
		);
		return mapWebhookToResponseWithCache({
			webhook,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async getWebhook(params: WebhookGetByUserParams): Promise<WebhookResponse>;
	async getWebhook(params: WebhookGetByTokenParams): Promise<WebhookTokenResponse>;
	async getWebhook(params: WebhookGetParams): Promise<WebhookResponse | WebhookTokenResponse> {
		if ('token' in params) {
			const webhook = await this.webhookService.getWebhookByToken({webhookId: params.webhookId, token: params.token});
			return mapWebhookToTokenResponse(webhook);
		}

		const webhook = await this.webhookService.getWebhook({userId: params.userId, webhookId: params.webhookId});
		return mapWebhookToResponseWithCache({
			webhook,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async updateWebhook(params: WebhookUpdateByUserParams): Promise<WebhookResponse>;
	async updateWebhook(params: WebhookUpdateByTokenParams): Promise<WebhookTokenResponse>;
	async updateWebhook(params: WebhookUpdateParams): Promise<WebhookResponse | WebhookTokenResponse> {
		if ('token' in params) {
			const webhook = await this.webhookService.updateWebhookByToken({
				webhookId: params.webhookId,
				token: params.token,
				data: params.data,
			});
			return mapWebhookToTokenResponse(webhook);
		}

		const webhook = await this.webhookService.updateWebhook(
			{
				userId: params.userId,
				webhookId: params.webhookId,
				data: params.data,
			},
			params.auditLogReason ?? null,
		);
		return mapWebhookToResponseWithCache({
			webhook,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async deleteWebhook(params: WebhookDeleteParams): Promise<void> {
		if ('token' in params) {
			await this.webhookService.deleteWebhookByToken({webhookId: params.webhookId, token: params.token});
			return;
		}

		await this.webhookService.deleteWebhook(
			{
				userId: params.userId,
				webhookId: params.webhookId,
			},
			params.auditLogReason ?? null,
		);
	}

	async executeWebhook(params: WebhookExecuteParams): Promise<WebhookExecutionResponse> {
		const message = await this.webhookService.executeWebhook({
			webhookId: params.webhookId,
			token: params.token,
			data: params.data,
			requestCache: params.requestCache,
		});

		if (!params.wait) {
			return null;
		}

		const messageAttachments = collectMessageAttachments(message);
		const attachmentDecayMap =
			messageAttachments.length > 0
				? await this.decayService.fetchMetadata(messageAttachments.map((att) => ({attachmentId: att.id})))
				: undefined;

		return mapMessageToResponse({
			message,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
			mediaService: this.mediaService,
			attachmentDecayMap,
			getReferencedMessage: (channelId, messageId) => this.channelRepository.getMessage(channelId, messageId),
		});
	}

	async executeGitHubWebhook(params: WebhookExecuteGitHubParams): Promise<void> {
		await this.webhookService.executeGitHubWebhook({
			webhookId: params.webhookId,
			token: params.token,
			event: params.event,
			delivery: params.delivery,
			data: params.data,
			requestCache: params.requestCache,
		});
	}

	async executeSentryWebhook(params: WebhookExecuteSentryParams): Promise<void> {
		await this.webhookService.executeSentryWebhook({
			webhookId: params.webhookId,
			token: params.token,
			event: params.event,
			data: params.data,
			requestCache: params.requestCache,
		});
	}

	async executeSlackWebhook(params: WebhookExecuteSlackParams): Promise<void> {
		await this.webhookService.executeWebhook({
			webhookId: params.webhookId,
			token: params.token,
			data: transformSlackWebhookRequest(params.data),
			requestCache: params.requestCache,
		});
	}

	async handleLiveKitWebhook(params: LiveKitWebhookParams): Promise<Response> {
		if (!Config.voice.enabled) {
			return new Response('Voice not enabled', {status: 404});
		}

		if (!this.liveKitWebhookService) {
			return new Response('LiveKit webhook service not available', {status: 503});
		}

		const response = await this.liveKitWebhookService.handleWebhookRequest({
			body: params.body,
			authHeader: params.authHeader,
		});
		return new Response(response.body, {status: response.status});
	}

	async handleSweegoWebhook(params: SweegoWebhookParams): Promise<Response> {
		if (!Config.email.enabled) {
			return new Response('Email not enabled', {status: 404});
		}

		const response = await this.sweegoWebhookService.handleWebhook({
			body: params.body,
			webhookId: params.webhookId,
			timestamp: params.timestamp,
			signature: params.signature,
			secret: Config.email.webhookSecret,
		});
		return new Response(response.body, {status: response.status});
	}
}
