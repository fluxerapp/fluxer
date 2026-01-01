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

import {AVATAR_MAX_SIZE} from '~/Constants';
import {MessageRequest} from '~/channel/ChannelModel';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Webhook} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {createBase64StringType, Int64Type, URLType, WebhookNameType, z} from '~/Schema';
import {getCachedUserPartialResponse} from '~/user/UserCacheHelpers';
import {UserPartialResponse} from '~/user/UserModel';

export const WebhookResponse = z.object({
	id: z.string(),
	guild_id: z.string(),
	channel_id: z.string(),
	user: z.lazy(() => UserPartialResponse),
	name: z.string(),
	avatar: z.string().nullish(),
	token: z.string(),
});

export type WebhookResponse = z.infer<typeof WebhookResponse>;

export const WebhookCreateRequest = z.object({
	name: WebhookNameType,
	avatar: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
});

export type WebhookCreateRequest = z.infer<typeof WebhookCreateRequest>;

export const WebhookUpdateRequest = z
	.object({
		name: WebhookNameType,
		avatar: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
		channel_id: Int64Type,
	})
	.partial();

export type WebhookUpdateRequest = z.infer<typeof WebhookUpdateRequest>;

export const WebhookMessageRequest = z.object({
	...MessageRequest.shape,
	username: WebhookNameType.nullish(),
	avatar_url: URLType.nullish(),
});

export type WebhookMessageRequest = z.infer<typeof WebhookMessageRequest>;

export async function mapWebhookToResponseWithCache({
	webhook,
	userCacheService,
	requestCache,
}: {
	webhook: Webhook;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
}): Promise<WebhookResponse> {
	const creatorPartial = await getCachedUserPartialResponse({
		userId: webhook.creatorId!,
		userCacheService,
		requestCache,
	});
	if (!creatorPartial) {
		throw new Error(`Creator user ${webhook.creatorId} not found for webhook`);
	}
	return {
		id: webhook.id.toString(),
		guild_id: webhook.guildId?.toString() || '',
		channel_id: webhook.channelId?.toString() || '',
		user: creatorPartial,
		name: webhook.name || '',
		avatar: webhook.avatarHash,
		token: webhook.token,
	};
}

export async function mapWebhooksToResponse({
	webhooks,
	userCacheService,
	requestCache,
}: {
	webhooks: Array<Webhook>;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
}): Promise<Array<WebhookResponse>> {
	return await Promise.all(
		webhooks.map((webhook) => mapWebhookToResponseWithCache({webhook, userCacheService, requestCache})),
	);
}
