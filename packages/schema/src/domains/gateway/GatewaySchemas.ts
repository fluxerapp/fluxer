// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {Int32Type, SnowflakeStringType, UnsignedInt64StringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const VoiceStateResponse = z.object({
	guild_id: SnowflakeStringType.nullable().describe('The guild ID this voice state is for, null if in a DM call'),
	channel_id: SnowflakeStringType.nullable().describe('The channel ID the user is connected to, null if disconnected'),
	user_id: SnowflakeStringType.describe('The user ID this voice state is for'),
	connection_id: z.string().nullable().optional().describe('The unique connection identifier'),
	session_id: z.string().optional().describe('The session ID for this voice state'),
	member: GuildMemberResponse.optional().describe('The guild member data, if in a guild voice channel'),
	mute: z.boolean().describe('Whether the user is server muted'),
	deaf: z.boolean().describe('Whether the user is server deafened'),
	self_mute: z.boolean().describe('Whether the user has muted themselves'),
	self_deaf: z.boolean().describe('Whether the user has deafened themselves'),
	suppress: z.boolean().optional().describe('Whether the user is prevented from speaking by channel permissions'),
	self_video: z.boolean().optional().describe('Whether the user has their camera enabled'),
	self_stream: z.boolean().optional().describe('Whether the user is streaming'),
	is_mobile: z.boolean().optional().describe('Whether the user is connected from a mobile device'),
	viewer_stream_keys: z
		.array(z.string())
		.nullable()
		.optional()
		.describe('The stream keys the user is currently viewing'),
	version: Int32Type.optional().describe('The voice state version for ordering updates'),
	p2p: z
		.boolean()
		.optional()
		.describe(
			'Whether this connection exchanges media peer-to-peer. A channel or call is peer-to-peer iff it has at least one voice state and every voice state in it has p2p=true.',
		),
	e2ee_capable: z
		.boolean()
		.optional()
		.describe(
			'Whether the client that produced this voice state advertised E2EE support in IDENTIFY. A channel is end-to-end encrypted iff every connected voice state in it has e2ee_capable=true.',
		),
});

export type VoiceStateResponse = z.infer<typeof VoiceStateResponse>;

export const ReadStateResponse = z.object({
	id: SnowflakeStringType.describe('The channel ID for this read state'),
	mention_count: Int32Type.describe('Number of unread mentions in the channel'),
	last_message_id: SnowflakeStringType.nullable().describe('The ID of the last message read'),
	last_pin_timestamp: z.string().nullable().describe('ISO8601 timestamp of the last pinned message acknowledged'),
	version: UnsignedInt64StringType.optional().describe('Read-state version for ordering updates as a decimal uint64'),
	flags: Int32Type.optional().describe('Server-stamped read state flags, present only on thread and forum read states'),
});

export type ReadStateResponse = z.infer<typeof ReadStateResponse>;

export const GatewayBotResponse = z.object({
	url: z.string().describe('WebSocket URL to connect to the gateway'),
	shards: z.number().int().describe('Recommended number of shards to use when connecting'),
	session_start_limit: z
		.object({
			total: z.number().int().describe('Total number of session starts allowed'),
			remaining: z.number().int().describe('Remaining number of session starts'),
			reset_after: z.number().int().describe('Milliseconds until the limit resets'),
			max_concurrency: z.number().int().describe('Maximum number of concurrent IDENTIFY requests'),
		})
		.describe('Session start rate limit information'),
});

export type GatewayBotResponse = z.infer<typeof GatewayBotResponse>;
