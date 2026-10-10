// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import {createUserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Webhook} from '@app/api/models/Webhook';
import {getCachedUserPartialResponse} from '@app/api/user/UserCacheHelpers';
import {Permissions, WebhookTypes, type WebhookTypeValue} from '@fluxer/constants/src/ChannelConstants';
import {DELETED_USER_ID} from '@fluxer/constants/src/UserConstants';
import type {WebhookResponse, WebhookTokenResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';

export function mapWebhookToTokenResponse(webhook: Webhook): WebhookTokenResponse {
	return {
		id: webhook.id.toString(),
		guild_id: webhook.guildId?.toString() || '',
		channel_id: webhook.channelId?.toString() || '',
		name: webhook.name || '',
		avatar: webhook.avatarHash,
		type: webhook.type as WebhookTypeValue,
		token: webhook.token,
	};
}

type WebhookSourceFields = Pick<WebhookResponse, 'source_guild' | 'source_channel'>;

export type WebhookSourceResolver = (webhook: Webhook) => Promise<WebhookSourceFields | null>;

const sourceResolutionsByRequest = new WeakMap<RequestCache, Map<string, Promise<WebhookSourceFields | null>>>();

export function createWebhookSourceResolver({
	channelRepository,
	gatewayService,
	requestCache,
}: {
	channelRepository: ChannelRepository;
	gatewayService: IGatewayService;
	requestCache: RequestCache;
}): WebhookSourceResolver {
	let resolutions = sourceResolutionsByRequest.get(requestCache);
	if (!resolutions) {
		resolutions = new Map();
		sourceResolutionsByRequest.set(requestCache, resolutions);
	}
	const memo = resolutions;
	return async (webhook) => {
		const {sourceGuildId, sourceChannelId, creatorId} = webhook;
		if (webhook.type !== WebhookTypes.CHANNEL_FOLLOWER || !sourceGuildId || !sourceChannelId || !creatorId) {
			return null;
		}
		const key = `${sourceGuildId}:${sourceChannelId}:${creatorId}`;
		let pending = memo.get(key);
		if (!pending) {
			pending = resolveWebhookSource({
				channelRepository,
				gatewayService,
				sourceGuildId,
				sourceChannelId,
				creatorId,
			});
			memo.set(key, pending);
		}
		return pending;
	};
}

async function resolveWebhookSource({
	channelRepository,
	gatewayService,
	sourceGuildId,
	sourceChannelId,
	creatorId,
}: {
	channelRepository: ChannelRepository;
	gatewayService: IGatewayService;
	sourceGuildId: GuildID;
	sourceChannelId: ChannelID;
	creatorId: UserID;
}): Promise<WebhookSourceFields | null> {
	try {
		const channel = await channelRepository.channelData.findUnique(sourceChannelId);
		if (!channel || channel.guildId !== sourceGuildId) return null;
		const canView = await gatewayService.checkPermission({
			guildId: sourceGuildId,
			userId: creatorId,
			permission: Permissions.VIEW_CHANNEL,
			channelId: sourceChannelId,
		});
		if (!canView) return null;
		const guild = await gatewayService.getGuildData({
			guildId: sourceGuildId,
			userId: creatorId,
			skipMembershipCheck: true,
		});
		return {
			source_guild: {id: sourceGuildId.toString(), name: guild.name, icon: guild.icon ?? null},
			source_channel: {id: sourceChannelId.toString(), name: channel.name ?? ''},
		};
	} catch (error) {
		Logger.warn(
			{error, sourceGuildId: sourceGuildId.toString(), sourceChannelId: sourceChannelId.toString()},
			'Failed to resolve follower webhook source',
		);
		return null;
	}
}

export async function mapWebhookToResponseWithCache({
	webhook,
	userCacheService,
	requestCache,
	resolveSource,
}: {
	webhook: Webhook;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	resolveSource?: WebhookSourceResolver;
}): Promise<WebhookResponse> {
	const [creatorPartial, source] = await Promise.all([
		getCachedUserPartialResponse({
			userId: webhook.creatorId ?? createUserID(DELETED_USER_ID),
			userCacheService,
			requestCache,
		}),
		resolveSource ? resolveSource(webhook) : Promise.resolve(null),
	]);
	const {token, ...common} = mapWebhookToTokenResponse(webhook);
	return {
		...common,
		...(webhook.type === WebhookTypes.CHANNEL_FOLLOWER ? {} : {token}),
		user: creatorPartial,
		...(source ?? {}),
	};
}

export async function mapWebhooksToResponse({
	webhooks,
	userCacheService,
	requestCache,
	resolveSource,
}: {
	webhooks: Array<Webhook>;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	resolveSource?: WebhookSourceResolver;
}): Promise<Array<WebhookResponse>> {
	return await Promise.all(
		webhooks.map((webhook) => mapWebhookToResponseWithCache({webhook, userCacheService, requestCache, resolveSource})),
	);
}
