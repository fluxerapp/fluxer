// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {WebhookTypeSchema} from '@fluxer/schema/src/primitives/WebhookValidators';
import {z} from 'zod';

const WebhookCommonResponse = {
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for the webhook'),
	guild_id: SnowflakeStringType.describe('The ID of the guild this webhook belongs to'),
	channel_id: SnowflakeStringType.describe('The ID of the channel this webhook posts to'),
	name: z.string().describe('The display name of the webhook'),
	avatar: z.string().nullish().describe('The hash of the webhook avatar image'),
	type: WebhookTypeSchema,
};
export const WebhookTokenResponse = z.object({
	...WebhookCommonResponse,
	token: z.string().describe('The secure token used to execute the webhook'),
});

export type WebhookTokenResponse = z.infer<typeof WebhookTokenResponse>;

export const WebhookCreateResponse = WebhookTokenResponse.extend({
	user: z.lazy(() => UserPartialResponse).describe('The user who created the webhook'),
});

export type WebhookCreateResponse = z.infer<typeof WebhookCreateResponse>;

const WebhookSourceGuildResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the guild that owns the followed announcement channel'),
	name: z.string().describe('The name of the guild that owns the followed announcement channel'),
	icon: z.string().nullish().describe('The icon hash of the guild that owns the followed announcement channel'),
});

const WebhookSourceChannelResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the followed announcement channel'),
	name: z.string().describe('The name of the followed announcement channel'),
});

export const WebhookResponse = z.object({
	...WebhookCommonResponse,
	token: z
		.string()
		.optional()
		.describe('The secure token used to execute the webhook, omitted for channel follower webhooks'),
	user: z.lazy(() => UserPartialResponse).describe('The user who created the webhook'),
	source_guild: WebhookSourceGuildResponse.optional().describe(
		'The guild of the followed announcement channel, present on channel follower webhooks while the creator can view it',
	),
	source_channel: WebhookSourceChannelResponse.optional().describe(
		'The followed announcement channel, present on channel follower webhooks while the creator can view it',
	),
});

export type WebhookResponse = z.infer<typeof WebhookResponse>;

export interface WebhookSourceGuild {
	readonly id: string;
	readonly name: string;
	readonly icon?: string | null;
}

export interface WebhookSourceChannel {
	readonly id: string;
	readonly name: string;
}

export interface Webhook {
	readonly id: string;
	readonly guild_id: string;
	readonly channel_id: string;
	readonly user: UserPartialResponse;
	readonly name: string;
	readonly avatar: string | null;
	readonly type: number;
	readonly token?: string;
	readonly source_guild?: WebhookSourceGuild;
	readonly source_channel?: WebhookSourceChannel;
}

export const WebhookListResponse = z.array(WebhookResponse);
export const SlackWebhookResponse = z.string();
