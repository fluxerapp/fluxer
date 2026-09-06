// SPDX-License-Identifier: AGPL-3.0-or-later

import {DELETED_USER_ID} from '@fluxer/constants/src/UserConstants';
import type {WebhookResponse, WebhookTokenResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import type {z} from 'zod';
import {createUserID} from '../BrandedTypes';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Webhook} from '../models/Webhook';
import {getCachedUserPartialResponse} from '../user/UserCacheHelpers';

export function mapWebhookToTokenResponse(webhook: Webhook): z.infer<typeof WebhookTokenResponse> {
	return {
		id: webhook.id.toString(),
		guild_id: webhook.guildId?.toString() || '',
		channel_id: webhook.channelId?.toString() || '',
		name: webhook.name || '',
		avatar: webhook.avatarHash,
		token: webhook.token,
	};
}

export async function mapWebhookToResponseWithCache({
	webhook,
	userCacheService,
	requestCache,
}: {
	webhook: Webhook;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
}): Promise<z.infer<typeof WebhookResponse>> {
	const creatorPartial = await getCachedUserPartialResponse({
		userId: webhook.creatorId ?? createUserID(DELETED_USER_ID),
		userCacheService,
		requestCache,
	});
	return {
		...mapWebhookToTokenResponse(webhook),
		user: creatorPartial,
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
}): Promise<Array<z.infer<typeof WebhookResponse>>> {
	return await Promise.all(
		webhooks.map((webhook) => mapWebhookToResponseWithCache({webhook, userCacheService, requestCache})),
	);
}
