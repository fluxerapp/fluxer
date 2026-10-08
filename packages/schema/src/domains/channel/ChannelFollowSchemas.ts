// SPDX-License-Identifier: AGPL-3.0-or-later

import {Int32Type, SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ChannelFollowRequest = z.object({
	webhook_channel_id: SnowflakeType.describe('The ID of the text channel that receives published messages'),
});

export type ChannelFollowRequest = z.infer<typeof ChannelFollowRequest>;

export const FollowedChannelResponse = z.object({
	channel_id: SnowflakeStringType.describe('The ID of the followed announcement channel'),
	webhook_id: SnowflakeStringType.describe('The ID of the channel follower webhook created in the target channel'),
});

export type FollowedChannelResponse = z.infer<typeof FollowedChannelResponse>;

export const ChannelFollowerStatsResponse = z.object({
	channel_count: Int32Type.describe('The number of channels following this announcement channel'),
	guild_count: Int32Type.describe('The number of distinct guilds following this announcement channel'),
});

export type ChannelFollowerStatsResponse = z.infer<typeof ChannelFollowerStatsResponse>;
