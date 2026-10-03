// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, MessageFlags, MessageTypes, WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	ChannelFollowerStatsResponse,
	ChannelFollowRequest,
	FollowedChannelResponse,
} from '@fluxer/schema/src/domains/channel/ChannelFollowSchemas';
import {ChannelCreateRequest, ChannelUpdateRequest} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {MessageResponseSchema} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {
	WebhookCreateResponse,
	WebhookResponse,
	WebhookTokenResponse,
} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {ChannelTypeSchema} from '@fluxer/schema/src/primitives/ChannelValidators';
import {MessageTypeSchema} from '@fluxer/schema/src/primitives/MessageValidators';
import {describe, expect, it} from 'vitest';

const creator = {
	id: '100',
	username: 'owner',
	discriminator: '0001',
	global_name: null,
	avatar: null,
	avatar_color: null,
	flags: 0,
	mention_flags: 0,
};

const incomingWebhook = {
	id: '1',
	guild_id: '2',
	channel_id: '3',
	name: 'Hook',
	avatar: null,
	type: WebhookTypes.INCOMING,
	token: 'secret',
};

describe('announcement channel type', () => {
	it('accepts type 5 as a channel type', () => {
		expect(ChannelTypeSchema.parse(ChannelTypes.GUILD_ANNOUNCEMENT)).toBe(5);
	});

	it('creates an announcement channel with the text channel fields', () => {
		const parsed = ChannelCreateRequest.parse({
			type: ChannelTypes.GUILD_ANNOUNCEMENT,
			name: 'news',
			topic: 'Updates',
			rate_limit_per_user: 10,
		});
		expect(parsed).toMatchObject({type: 5, name: 'news', topic: 'Updates', rate_limit_per_user: 10, nsfw: false});
	});

	it('updates an announcement channel through its own variant', () => {
		expect(ChannelUpdateRequest.parse({type: ChannelTypes.GUILD_ANNOUNCEMENT, topic: 'New'})).toMatchObject({
			type: 5,
			topic: 'New',
		});
	});
});

describe('channel follow add message type', () => {
	it('accepts type 12 as a message type', () => {
		expect(MessageTypeSchema.parse(MessageTypes.CHANNEL_FOLLOW_ADD)).toBe(12);
	});
});

describe('channel follow schemas', () => {
	it('parses the target channel of a follow request', () => {
		expect(ChannelFollowRequest.parse({webhook_channel_id: '123'})).toEqual({webhook_channel_id: 123n});
		expect(ChannelFollowRequest.safeParse({webhook_channel_id: 'abc'}).success).toBe(false);
		expect(ChannelFollowRequest.safeParse({}).success).toBe(false);
	});

	it('describes the followed channel and its webhook', () => {
		expect(FollowedChannelResponse.parse({channel_id: '1', webhook_id: '2'})).toEqual({
			channel_id: '1',
			webhook_id: '2',
		});
	});

	it('reports follower counts as integers', () => {
		expect(ChannelFollowerStatsResponse.parse({channel_count: 3, guild_count: 2})).toEqual({
			channel_count: 3,
			guild_count: 2,
		});
		expect(ChannelFollowerStatsResponse.safeParse({channel_count: 1.5, guild_count: 1}).success).toBe(false);
	});
});

describe('webhook responses', () => {
	it('keeps the token required on token responses', () => {
		expect(WebhookTokenResponse.parse(incomingWebhook)).toEqual(incomingWebhook);
		const {token: _token, ...withoutToken} = incomingWebhook;
		expect(WebhookTokenResponse.safeParse(withoutToken).success).toBe(false);
		expect(WebhookCreateResponse.safeParse({...withoutToken, user: creator}).success).toBe(false);
	});

	it('omits the token and includes the source on follower webhooks', () => {
		const {token: _token, ...withoutToken} = incomingWebhook;
		const follower = {
			...withoutToken,
			type: WebhookTypes.CHANNEL_FOLLOWER,
			user: creator,
			source_guild: {id: '7', name: 'Source', icon: null},
			source_channel: {id: '8', name: 'news'},
		};
		const parsed = WebhookResponse.parse(follower);
		expect(parsed).not.toHaveProperty('token');
		expect(parsed.source_guild).toEqual({id: '7', name: 'Source', icon: null});
		expect(parsed.source_channel).toEqual({id: '8', name: 'news'});
	});

	it('rejects unknown webhook types', () => {
		expect(WebhookResponse.safeParse({...incomingWebhook, type: 3, user: creator}).success).toBe(false);
	});
});

describe('message responses', () => {
	it('allows a message reference without a message id', () => {
		const shape = MessageResponseSchema.shape.message_reference;
		const reference = {type: 0, guild_id: '1', channel_id: '2'};
		expect(shape.parse(reference)).toEqual(reference);
	});

	it('accepts the crosspost flags', () => {
		const flags = MessageFlags.CROSSPOSTED | MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED;
		expect(MessageResponseSchema.shape.flags.parse(flags)).toBe(flags);
	});
});
