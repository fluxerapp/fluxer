// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID, WebhookID, WebhookToken} from '@app/api/BrandedTypes';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import {createMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {maskThreadArtifactsFor} from '@app/api/channel/services/message/ThreadMessageResponses';
import {SYSTEM_THREAD_VIEWER, type ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {LiveKitWebhookService} from '@app/api/infrastructure/LiveKitWebhookService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Message} from '@app/api/models/Message';
import type {SweegoWebhookService} from '@app/api/webhook/SweegoWebhookService';
import {transformSlackWebhookRequest} from '@app/api/webhook/transformers/SlackTransformer';
import {
	createWebhookSourceResolver,
	mapWebhooksToResponse,
	mapWebhookToResponseWithCache,
	mapWebhookToTokenResponse,
	type WebhookSourceResolver,
} from '@app/api/webhook/WebhookModel';
import type {WebhookExecuteMessageData, WebhookService} from '@app/api/webhook/WebhookService';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import type {InstatusWebhook} from '@fluxer/schema/src/domains/webhook/InstatusWebhookSchemas';
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
	viewer: ThreadViewer;
	guildId: GuildID;
	requestCache: RequestCache;
}

interface WebhookListChannelParams {
	userId: UserID;
	viewer: ThreadViewer;
	channelId: ChannelID;
	requestCache: RequestCache;
}

interface WebhookCreateParams {
	userId: UserID;
	viewer: ThreadViewer;
	channelId: ChannelID;
	data: WebhookCreateRequest;
	requestCache: RequestCache;
	auditLogReason?: string | null;
}

interface WebhookGetByUserParams {
	userId: UserID;
	viewer: ThreadViewer;
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
	viewer: ThreadViewer;
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
	viewer: ThreadViewer;
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
	threadId?: string;
	data: WebhookExecuteMessageData;
	wait: boolean;
	requestCache: RequestCache;
}

interface WebhookGetMessageParams {
	webhookId: WebhookID;
	token: WebhookToken;
	threadId?: string;
	messageId: MessageID;
	requestCache: RequestCache;
}

interface WebhookEditMessageParams {
	webhookId: WebhookID;
	token: WebhookToken;
	threadId?: string;
	messageId: MessageID;
	data: MessageUpdateRequest;
	requestCache: RequestCache;
}

interface WebhookDeleteMessageParams {
	webhookId: WebhookID;
	token: WebhookToken;
	threadId?: string;
	messageId: MessageID;
	requestCache: RequestCache;
}

interface WebhookExecuteGitHubParams {
	webhookId: WebhookID;
	token: WebhookToken;
	threadId?: string;
	event: string;
	delivery: string;
	data: GitHubWebhook;
	requestCache: RequestCache;
}

interface WebhookExecuteSlackParams {
	webhookId: WebhookID;
	token: WebhookToken;
	threadId?: string;
	data: SlackWebhookRequest;
	requestCache: RequestCache;
}

interface WebhookExecuteInstatusParams {
	webhookId: WebhookID;
	token: WebhookToken;
	data: InstatusWebhook;
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
	constructor(
		private readonly webhookService: WebhookService,
		private readonly channelRepository: IChannelRepository,
		private readonly userCacheService: UserCacheService,
		private readonly liveKitWebhookService: LiveKitWebhookService | null,
		private readonly sweegoWebhookService: SweegoWebhookService,
		private readonly gatewayService: IGatewayService,
	) {}

	private sourceResolver(requestCache: RequestCache): WebhookSourceResolver {
		return createWebhookSourceResolver({
			channelRepository: this.channelRepository,
			gatewayService: this.gatewayService,
			requestCache,
		});
	}

	async listGuildWebhooks(params: WebhookListGuildParams): Promise<Array<WebhookResponse>> {
		const webhooks = await this.webhookService.getGuildWebhooks({
			userId: params.userId,
			viewer: params.viewer,
			guildId: params.guildId,
		});
		return mapWebhooksToResponse({
			webhooks,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
			resolveSource: this.sourceResolver(params.requestCache),
		});
	}

	async listChannelWebhooks(params: WebhookListChannelParams): Promise<Array<WebhookResponse>> {
		const webhooks = await this.webhookService.getChannelWebhooks({
			userId: params.userId,
			viewer: params.viewer,
			channelId: params.channelId,
		});
		return mapWebhooksToResponse({
			webhooks,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
			resolveSource: this.sourceResolver(params.requestCache),
		});
	}

	async createWebhook(params: WebhookCreateParams): Promise<WebhookResponse> {
		const webhook = await this.webhookService.createWebhook(
			{
				userId: params.userId,
				viewer: params.viewer,
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
		const webhook = await this.webhookService.getWebhook({
			userId: params.userId,
			viewer: params.viewer,
			webhookId: params.webhookId,
		});
		return mapWebhookToResponseWithCache({
			webhook,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
			resolveSource: this.sourceResolver(params.requestCache),
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
				viewer: params.viewer,
				webhookId: params.webhookId,
				data: params.data,
			},
			params.auditLogReason ?? null,
		);
		return mapWebhookToResponseWithCache({
			webhook,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
			resolveSource: this.sourceResolver(params.requestCache),
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
				viewer: params.viewer,
				webhookId: params.webhookId,
			},
			params.auditLogReason ?? null,
		);
	}

	async executeWebhook(params: WebhookExecuteParams): Promise<WebhookExecutionResponse> {
		const message = await this.webhookService.executeWebhook({
			webhookId: params.webhookId,
			token: params.token,
			threadId: params.threadId,
			data: params.data,
			requestCache: params.requestCache,
		});
		if (!params.wait) {
			return null;
		}
		return this.mapMessageResponse(message, params.requestCache);
	}

	async getWebhookMessage(params: WebhookGetMessageParams): Promise<MessageResponse> {
		const message = await this.webhookService.getWebhookMessage({
			webhookId: params.webhookId,
			token: params.token,
			threadId: params.threadId,
			messageId: params.messageId,
		});
		return this.mapMessageResponse(message, params.requestCache);
	}

	async editWebhookMessage(params: WebhookEditMessageParams): Promise<MessageResponse> {
		const message = await this.webhookService.editWebhookMessage({
			webhookId: params.webhookId,
			token: params.token,
			threadId: params.threadId,
			messageId: params.messageId,
			data: params.data,
			requestCache: params.requestCache,
		});
		return this.mapMessageResponse(message, params.requestCache);
	}

	async deleteWebhookMessage(params: WebhookDeleteMessageParams): Promise<void> {
		await this.webhookService.deleteWebhookMessage({
			webhookId: params.webhookId,
			token: params.token,
			threadId: params.threadId,
			messageId: params.messageId,
			requestCache: params.requestCache,
		});
	}

	async executeGitHubWebhook(params: WebhookExecuteGitHubParams): Promise<void> {
		await this.webhookService.executeGitHubWebhook({
			webhookId: params.webhookId,
			token: params.token,
			threadId: params.threadId,
			event: params.event,
			delivery: params.delivery,
			data: params.data,
			requestCache: params.requestCache,
		});
	}

	async executeInstatusWebhook(params: WebhookExecuteInstatusParams): Promise<void> {
		await this.webhookService.executeInstatusWebhook({
			webhookId: params.webhookId,
			token: params.token,
			data: params.data,
			requestCache: params.requestCache,
		});
	}

	async executeSlackWebhook(params: WebhookExecuteSlackParams): Promise<void> {
		await this.webhookService.executeWebhook({
			webhookId: params.webhookId,
			token: params.token,
			threadId: params.threadId,
			data: transformSlackWebhookRequest(params.data),
			requestCache: params.requestCache,
			service: true,
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

	private async mapMessageResponse(message: Message, _requestCache: RequestCache): Promise<MessageResponse> {
		const channel = await this.channelRepository.findUnique(message.channelId);
		const response = await createMessageResponseDataService().buildMessage({
			userId: createUserID(0n),
			message,
			access: {
				sourceGuildId: channel?.guildId ?? null,
				messageHistoryCutoff: null,
				canReadMessageHistory: true,
			},
		});
		const [shaped] = maskThreadArtifactsFor(SYSTEM_THREAD_VIEWER, channel?.guildId ?? null, [response]);
		return shaped ?? response;
	}
}
