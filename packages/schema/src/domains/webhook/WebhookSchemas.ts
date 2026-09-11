// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const WebhookBaseResponse = {
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for the webhook'),
	guild_id: SnowflakeStringType.describe('The ID of the guild this webhook belongs to'),
	channel_id: SnowflakeStringType.describe('The ID of the channel this webhook posts to'),
	name: z.string().describe('The display name of the webhook'),
	avatar: z.string().nullish().describe('The hash of the webhook avatar image'),
	token: z.string().describe('The secure token used to execute the webhook'),
};
export const WebhookTokenResponse = z.object(WebhookBaseResponse);

export type WebhookTokenResponse = z.infer<typeof WebhookTokenResponse>;

export const WebhookResponse = WebhookTokenResponse.extend({
	user: z.lazy(() => UserPartialResponse).describe('The user who created the webhook'),
});

export type WebhookResponse = z.infer<typeof WebhookResponse>;

export interface Webhook {
	readonly id: string;
	readonly guild_id: string;
	readonly channel_id: string;
	readonly user: UserPartialResponse;
	readonly name: string;
	readonly avatar: string | null;
	readonly token: string;
}

export const WebhookListResponse = z.array(WebhookResponse);
export const SlackWebhookResponse = z.string();
