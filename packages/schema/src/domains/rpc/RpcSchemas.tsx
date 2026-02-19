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

import {RTC_REGION_ID_MAX_LENGTH, RTC_REGION_ID_MIN_LENGTH} from '@fluxer/constants/src/LimitConstants';
import {ChannelResponse, RtcRegionResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {GuildEmojiResponse, GuildStickerResponse} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {FavoriteMemeResponse} from '@fluxer/schema/src/domains/meme/MemeSchemas';
import {CustomStatusPayload} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {
	CustomStatusResponse,
	RelationshipResponse,
	UserGuildSettingsResponse,
	UserPrivateResponse,
	UserSettingsResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createStringType, SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const RpcGuildCollectionType = z.enum(['guild', 'roles', 'channels', 'emojis', 'stickers', 'members']);

export type RpcGuildCollectionType = z.infer<typeof RpcGuildCollectionType>;

export const ReadStateResponse = z.object({
	id: SnowflakeStringType.describe('The channel ID for this read state'),
	mention_count: z.number().describe('Number of unread mentions in the channel'),
	last_message_id: SnowflakeStringType.nullish().describe('ID of the last read message'),
	last_pin_timestamp: z.string().nullish().describe('Timestamp of the last pinned message'),
});

export type ReadStateResponse = z.infer<typeof ReadStateResponse>;

export const RpcRequest = z.discriminatedUnion('type', [
	z.object({
		type: z.literal('session').describe('Request type for session initialization'),
		token: createStringType().describe('Authentication token for the session'),
		version: z.literal(1).describe('RPC protocol version'),
		ip: createStringType(1, 45).optional().describe('Client IP address'),
		latitude: createStringType(1, 32).optional().describe('Client latitude for region selection'),
		longitude: createStringType(1, 32).optional().describe('Client longitude for region selection'),
	}),
	z.object({
		type: z.literal('guild_collection').describe('Request type for fetching a single guild collection chunk'),
		guild_id: SnowflakeType.describe('ID of the guild to fetch'),
		collection: RpcGuildCollectionType.describe('Guild collection to fetch'),
		limit: z.number().int().min(1).max(1000).optional().describe('Maximum number of items to return'),
		after_user_id: SnowflakeType.optional().describe('Cursor for member collection pagination'),
	}),
	z.object({
		type: z.literal('log_guild_crash').describe('Request type for logging guild crashes'),
		guild_id: SnowflakeType.describe('ID of the guild that crashed'),
		stacktrace: z.string().describe('Error stacktrace from the crash'),
	}),
	z.object({
		type: z.literal('get_user_guild_settings').describe('Request type for fetching user guild settings'),
		user_ids: z.array(SnowflakeType).describe('IDs of users to fetch settings for'),
		guild_id: SnowflakeType.describe('ID of the guild'),
	}),
	z.object({
		type: z.literal('get_push_subscriptions').describe('Request type for fetching push notification subscriptions'),
		user_ids: z.array(SnowflakeType).describe('IDs of users to fetch subscriptions for'),
	}),
	z.object({
		type: z.literal('get_badge_counts').describe('Request type for fetching notification badge counts'),
		user_ids: z.array(SnowflakeType).describe('IDs of users to fetch badge counts for'),
	}),
	z.object({
		type: z.literal('geoip_lookup').describe('Request type for IP geolocation lookup'),
		ip: createStringType(1, 45).describe('IP address to lookup'),
	}),
	z.object({
		type: z.literal('delete_push_subscriptions').describe('Request type for deleting push notification subscriptions'),
		subscriptions: z
			.array(
				z.object({
					user_id: SnowflakeType.describe('ID of the user'),
					subscription_id: createStringType().describe('ID of the subscription to delete'),
				}),
			)
			.describe('List of subscriptions to delete'),
	}),
	z.object({
		type: z.literal('get_user_blocked_ids').describe('Request type for fetching blocked user IDs'),
		user_ids: z.array(SnowflakeType).describe('IDs of users to fetch blocked lists for'),
	}),
	z.object({
		type: z.literal('voice_get_token').describe('Request type for getting voice connection token'),
		guild_id: SnowflakeType.optional().describe('ID of the guild for the voice channel'),
		channel_id: SnowflakeType.describe('ID of the voice channel'),
		user_id: SnowflakeType.describe('ID of the user joining voice'),
		connection_id: createStringType().optional().describe('Existing connection ID for reconnection'),
		rtc_region: createStringType(RTC_REGION_ID_MIN_LENGTH, RTC_REGION_ID_MAX_LENGTH)
			.optional()
			.describe(
				`Preferred voice region for the connection (${RTC_REGION_ID_MIN_LENGTH}-${RTC_REGION_ID_MAX_LENGTH} characters)`,
			),
		latitude: createStringType(1, 32).optional().describe('Client latitude for region selection'),
		longitude: createStringType(1, 32).optional().describe('Client longitude for region selection'),
		can_speak: z.boolean().optional().describe('Whether the user can speak in the channel'),
		can_stream: z.boolean().optional().describe('Whether the user can stream in the channel'),
		can_video: z.boolean().optional().describe('Whether the user can use video in the channel'),
		token_nonce: createStringType(1, 64).optional().describe('Token nonce for replay prevention'),
	}),
	z.object({
		type: z.literal('kick_temporary_member').describe('Request type for kicking temporary guild members'),
		user_id: SnowflakeType.describe('ID of the user to kick'),
		guild_ids: z.array(SnowflakeType).describe('IDs of guilds to kick the user from'),
	}),
	z.object({
		type: z
			.literal('voice_force_disconnect_participant')
			.describe('Request type for force disconnecting a voice participant'),
		guild_id: SnowflakeType.optional().describe('ID of the guild'),
		channel_id: SnowflakeType.describe('ID of the voice channel'),
		user_id: SnowflakeType.describe('ID of the user to disconnect'),
		connection_id: createStringType().describe('Connection ID of the user'),
	}),
	z.object({
		type: z.literal('voice_update_participant').describe('Request type for updating voice participant state'),
		guild_id: SnowflakeType.optional().describe('ID of the guild'),
		channel_id: SnowflakeType.describe('ID of the voice channel'),
		user_id: SnowflakeType.describe('ID of the user to update'),
		mute: z.boolean().describe('Whether the user is muted'),
		deaf: z.boolean().describe('Whether the user is deafened'),
	}),
	z.object({
		type: z
			.literal('voice_force_disconnect_channel')
			.describe('Request type for force disconnecting all participants from a channel'),
		guild_id: SnowflakeType.optional().describe('ID of the guild'),
		channel_id: SnowflakeType.describe('ID of the voice channel to clear'),
	}),
	z.object({
		type: z
			.literal('voice_update_participant_permissions')
			.describe('Request type for updating voice participant permissions'),
		guild_id: SnowflakeType.optional().describe('ID of the guild'),
		channel_id: SnowflakeType.describe('ID of the voice channel'),
		user_id: SnowflakeType.describe('ID of the user to update'),
		connection_id: createStringType().describe('Connection ID of the user'),
		can_speak: z.boolean().describe('Whether the user can speak'),
		can_stream: z.boolean().describe('Whether the user can stream'),
		can_video: z.boolean().describe('Whether the user can use video'),
	}),
	z.object({
		type: z.literal('call_ended').describe('Request type for notifying that a call has ended'),
		channel_id: SnowflakeType.describe('ID of the channel where the call ended'),
		message_id: SnowflakeType.describe('ID of the call start message'),
		participants: z.array(SnowflakeType).describe('IDs of users who participated in the call'),
		ended_timestamp: z.number().describe('Unix timestamp when the call ended'),
	}),
	z.object({
		type: z.literal('get_dm_channel').describe('Request type for fetching a DM channel'),
		channel_id: SnowflakeType.describe('ID of the DM channel'),
		user_id: SnowflakeType.describe('ID of the user requesting the channel'),
	}),
	z.object({
		type: z.literal('validate_custom_status').describe('Request type for validating a custom status'),
		user_id: SnowflakeType.describe('ID of the user'),
		custom_status: CustomStatusPayload.nullish().describe('Custom status data to validate'),
	}),
]);

export type RpcRequest = z.infer<typeof RpcRequest>;

export const RpcResponseSessionData = z.object({
	auth_session_id_hash: z.string().nullish().describe('Hash of the authentication session ID'),
	user: UserPrivateResponse.describe('Private user data for the authenticated user'),
	user_settings: UserSettingsResponse.nullish().describe('User settings configuration'),
	user_guild_settings: z.array(UserGuildSettingsResponse).describe('Per-guild settings for the user'),
	notes: z.record(SnowflakeStringType, z.string()).describe('User notes keyed by user ID'),
	read_states: z.array(ReadStateResponse).describe('Read state for each channel'),
	private_channels: z.array(ChannelResponse).describe('List of DM and group DM channels'),
	relationships: z.array(RelationshipResponse).describe('User relationships (friends, blocked, etc.)'),
	favorite_memes: z.array(FavoriteMemeResponse).describe('List of user favorite memes'),
	guild_ids: z.array(SnowflakeStringType).describe('IDs of guilds the user is a member of'),
	pinned_dms: z.array(SnowflakeStringType).describe('IDs of pinned DM channels'),
	country_code: z.string().describe('Two-letter country code from IP geolocation'),
	rtc_regions: z.array(RtcRegionResponse).describe('Available voice server regions'),
	version: z.number().int().describe('Session data version for cache invalidation'),
});

export type RpcResponseSessionData = z.infer<typeof RpcResponseSessionData>;

export const RpcResponseGuildCollectionData = z.object({
	collection: RpcGuildCollectionType.describe('Guild collection returned in this response'),
	guild: GuildResponse.nullish().describe('Guild information'),
	roles: z.array(GuildRoleResponse).nullish().describe('List of roles in the guild'),
	channels: z.array(ChannelResponse).nullish().describe('List of channels in the guild'),
	emojis: z.array(GuildEmojiResponse).nullish().describe('List of custom emojis in the guild'),
	stickers: z.array(GuildStickerResponse).nullish().describe('List of custom stickers in the guild'),
	members: z.array(GuildMemberResponse).nullish().describe('List of guild members in this chunk'),
	has_more: z.boolean().describe('Whether more data is available for this collection'),
	next_after_user_id: SnowflakeStringType.nullish().describe('Cursor for the next member chunk'),
});

export type RpcResponseGuildCollectionData = z.infer<typeof RpcResponseGuildCollectionData>;

export const RpcResponseValidateCustomStatus = z.object({
	custom_status: CustomStatusResponse.nullish().describe('Validated custom status or null if invalid'),
});

export type RpcResponseValidateCustomStatus = z.infer<typeof RpcResponseValidateCustomStatus>;

export const RpcResponse = z.discriminatedUnion('type', [
	z.object({
		type: z.literal('session').describe('Response type for session initialization'),
		data: RpcResponseSessionData.describe('Session initialization data'),
	}),
	z.object({
		type: z.literal('log_guild_crash').describe('Response type for guild crash logging'),
		data: z
			.object({
				success: z.boolean().describe('Whether the crash was logged successfully'),
			})
			.describe('Crash logging result'),
	}),
	z.object({
		type: z.literal('guild_collection').describe('Response type for guild collection chunks'),
		data: RpcResponseGuildCollectionData.describe('Guild collection chunk data'),
	}),
	z.object({
		type: z.literal('get_user_guild_settings').describe('Response type for user guild settings'),
		data: z
			.object({
				user_guild_settings: z
					.array(UserGuildSettingsResponse.nullable())
					.describe('Guild settings for each requested user'),
			})
			.describe('User guild settings data'),
	}),
	z.object({
		type: z.literal('get_push_subscriptions').describe('Response type for push subscriptions'),
		data: z
			.record(
				SnowflakeStringType,
				z.array(
					z.object({
						subscription_id: z.string().describe('Unique identifier for the subscription'),
						endpoint: z.string().describe('Push notification endpoint URL'),
						p256dh_key: z.string().describe('P-256 Diffie-Hellman public key'),
						auth_key: z.string().describe('Authentication secret key'),
					}),
				),
			)
			.describe('Push subscriptions keyed by user ID'),
	}),
	z.object({
		type: z.literal('delete_push_subscriptions').describe('Response type for push subscription deletion'),
		data: z.object({success: z.boolean().describe('Whether the deletion was successful')}).describe('Deletion result'),
	}),
	z.object({
		type: z.literal('get_user_blocked_ids').describe('Response type for blocked user IDs'),
		data: z.record(SnowflakeStringType, z.array(SnowflakeStringType)).describe('Blocked user IDs keyed by user ID'),
	}),
	z.object({
		type: z.literal('voice_get_token').describe('Response type for voice connection token'),
		data: z
			.object({
				token: z.string().describe('Voice server authentication token'),
				endpoint: z.string().describe('Voice server endpoint URL'),
				connectionId: z.string().describe('Unique connection identifier'),
				tokenNonce: z.string().describe('Token nonce for webhook confirmation'),
			})
			.describe('Voice connection credentials'),
	}),
	z.object({
		type: z.literal('kick_temporary_member').describe('Response type for temporary member kick'),
		data: z
			.object({
				success: z.boolean().describe('Whether the kick was successful'),
			})
			.describe('Kick result'),
	}),
	z.object({
		type: z.literal('get_badge_counts').describe('Response type for badge counts'),
		data: z
			.object({
				badge_counts: z.record(SnowflakeStringType, z.number().int().min(0)).describe('Badge counts keyed by user ID'),
			})
			.describe('Badge count data'),
	}),
	z.object({
		type: z.literal('voice_force_disconnect_participant').describe('Response type for force disconnect participant'),
		data: z
			.object({
				success: z.boolean().describe('Whether the disconnect was successful'),
			})
			.describe('Disconnect result'),
	}),
	z.object({
		type: z.literal('voice_update_participant').describe('Response type for voice participant update'),
		data: z
			.object({
				success: z.boolean().describe('Whether the update was successful'),
			})
			.describe('Update result'),
	}),
	z.object({
		type: z.literal('voice_force_disconnect_channel').describe('Response type for force disconnect channel'),
		data: z
			.object({
				success: z.boolean().describe('Whether the operation was successful'),
				disconnected_count: z.number().optional().describe('Number of participants disconnected'),
				message: z.string().optional().describe('Additional status message'),
			})
			.describe('Channel disconnect result'),
	}),
	z.object({
		type: z.literal('voice_update_participant_permissions').describe('Response type for voice permissions update'),
		data: z
			.object({
				success: z.boolean().describe('Whether the permissions update was successful'),
			})
			.describe('Permissions update result'),
	}),
	z.object({
		type: z.literal('call_ended').describe('Response type for call ended notification'),
		data: z
			.object({
				success: z.boolean().describe('Whether the notification was processed successfully'),
			})
			.describe('Call ended result'),
	}),
	z.object({
		type: z.literal('validate_custom_status').describe('Response type for custom status validation'),
		data: RpcResponseValidateCustomStatus.describe('Custom status validation result'),
	}),
	z.object({
		type: z.literal('geoip_lookup').describe('Response type for IP geolocation lookup'),
		data: z.object({country_code: z.string().describe('Two-letter country code')}).describe('Geolocation result'),
	}),
	z.object({
		type: z.literal('get_dm_channel').describe('Response type for DM channel fetch'),
		data: z
			.object({
				channel: ChannelResponse.nullish().describe('The DM channel or null if not found'),
			})
			.describe('DM channel result'),
	}),
]);

export type RpcResponse = z.infer<typeof RpcResponse>;
