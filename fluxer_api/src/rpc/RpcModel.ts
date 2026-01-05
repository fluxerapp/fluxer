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

import {ChannelResponse} from '~/channel/ChannelModel';
import {FavoriteMemeResponse} from '~/favorite_meme/FavoriteMemeModel';
import {
	GuildEmojiResponse,
	GuildMemberResponse,
	GuildResponse,
	GuildRoleResponse,
	GuildStickerResponse,
} from '~/guild/GuildModel';
import {createStringType, Int64Type, z} from '~/Schema';
import {
	RelationshipResponse,
	UserGuildSettingsResponse,
	UserPrivateResponse,
	UserSettingsResponse,
} from '~/user/UserModel';
import {CustomStatusResponse} from '~/user/UserTypes';

const ReadStateResponse = z.object({
	id: z.string(),
	mention_count: z.number(),
	last_message_id: z.string().nullish(),
	last_pin_timestamp: z.string().nullish(),
});

export const RpcRequest = z.discriminatedUnion('type', [
	z.object({
		type: z.literal('session'),
		token: createStringType(),
		version: z.literal(1),
		ip: createStringType(1, 45).optional(),
		latitude: createStringType(1, 32).optional(),
		longitude: createStringType(1, 32).optional(),
	}),
	z.object({
		type: z.literal('guild'),
		guild_id: Int64Type,
	}),
	z.object({
		type: z.literal('get_user_guild_settings'),
		user_ids: z.array(Int64Type),
		guild_id: Int64Type,
	}),
	z.object({
		type: z.literal('get_push_subscriptions'),
		user_ids: z.array(Int64Type),
	}),
	z.object({
		type: z.literal('get_badge_counts'),
		user_ids: z.array(Int64Type),
	}),
	z.object({
		type: z.literal('geoip_lookup'),
		ip: createStringType(1, 45),
	}),
	z.object({
		type: z.literal('delete_push_subscriptions'),
		subscriptions: z.array(
			z.object({
				user_id: Int64Type,
				subscription_id: createStringType(),
			}),
		),
	}),
	z.object({
		type: z.literal('get_user_blocked_ids'),
		user_ids: z.array(Int64Type),
	}),
	z.object({
		type: z.literal('get_visionary_slots'),
	}),
	z.object({
		type: z.literal('voice_get_token'),
		guild_id: Int64Type.optional(),
		channel_id: Int64Type,
		user_id: Int64Type,
		connection_id: createStringType().optional(),
		latitude: createStringType(1, 32).optional(),
		longitude: createStringType(1, 32).optional(),
		can_speak: z.boolean().optional(),
		can_stream: z.boolean().optional(),
		can_video: z.boolean().optional(),
	}),
	z.object({
		type: z.literal('kick_temporary_member'),
		user_id: Int64Type,
		guild_ids: z.array(Int64Type),
	}),
	z.object({
		type: z.literal('voice_force_disconnect_participant'),
		guild_id: Int64Type.optional(),
		channel_id: Int64Type,
		user_id: Int64Type,
		connection_id: createStringType(),
	}),
	z.object({
		type: z.literal('voice_update_participant'),
		guild_id: Int64Type.optional(),
		channel_id: Int64Type,
		user_id: Int64Type,
		mute: z.boolean(),
		deaf: z.boolean(),
	}),
	z.object({
		type: z.literal('voice_force_disconnect_channel'),
		guild_id: Int64Type.optional(),
		channel_id: Int64Type,
	}),
	z.object({
		type: z.literal('voice_update_participant_permissions'),
		guild_id: Int64Type.optional(),
		channel_id: Int64Type,
		user_id: Int64Type,
		connection_id: createStringType(),
		can_speak: z.boolean(),
		can_stream: z.boolean(),
		can_video: z.boolean(),
	}),
	z.object({
		type: z.literal('call_ended'),
		channel_id: Int64Type,
		message_id: Int64Type,
		participants: z.array(Int64Type),
		ended_timestamp: z.number(),
	}),
	z.object({
		type: z.literal('get_dm_channel'),
		channel_id: Int64Type,
		user_id: Int64Type,
	}),
	z.object({
		type: z.literal('validate_custom_status'),
		user_id: Int64Type,
		custom_status: z.unknown().nullish(),
	}),
]);

export type RpcRequest = z.infer<typeof RpcRequest>;

export const RpcResponseSessionData = z.object({
	auth_session_id_hash: z.string().nullish(),
	user: UserPrivateResponse,
	user_settings: UserSettingsResponse.nullish(),
	user_guild_settings: z.array(UserGuildSettingsResponse),
	notes: z.record(z.string(), z.string()),
	read_states: z.array(ReadStateResponse),
	private_channels: z.array(ChannelResponse),
	relationships: z.array(RelationshipResponse),
	favorite_memes: z.array(FavoriteMemeResponse),
	guild_ids: z.array(z.string()),
	pinned_dms: z.array(z.string()),
	country_code: z.string(),
	rtc_regions: z.array(z.string()),
	version: z.number().int(),
	feature_flags: z.record(z.string(), z.array(z.string())).optional(),
});

export type RpcResponseSessionData = z.infer<typeof RpcResponseSessionData>;

export const RpcResponseGuildData = z.object({
	guild: GuildResponse,
	roles: z.array(GuildRoleResponse),
	channels: z.array(ChannelResponse),
	emojis: z.array(GuildEmojiResponse),
	stickers: z.array(GuildStickerResponse),
	members: z.array(GuildMemberResponse),
});

export type RpcResponseGuildData = z.infer<typeof RpcResponseGuildData>;

export const RpcResponseValidateCustomStatus = z.object({
	custom_status: CustomStatusResponse.nullish(),
});

export type RpcResponseValidateCustomStatus = z.infer<typeof RpcResponseValidateCustomStatus>;

export const RpcResponse = z.discriminatedUnion('type', [
	z.object({
		type: z.literal('session'),
		data: RpcResponseSessionData,
	}),
	z.object({
		type: z.literal('guild'),
		data: RpcResponseGuildData,
	}),
	z.object({
		type: z.literal('get_user_guild_settings'),
		data: z.object({
			user_guild_settings: z.array(UserGuildSettingsResponse.nullable()),
		}),
	}),
	z.object({
		type: z.literal('get_push_subscriptions'),
		data: z.record(
			z.string(),
			z.array(
				z.object({
					subscription_id: z.string(),
					endpoint: z.string(),
					p256dh_key: z.string(),
					auth_key: z.string(),
				}),
			),
		),
	}),
	z.object({
		type: z.literal('delete_push_subscriptions'),
		data: z.object({success: z.boolean()}),
	}),
	z.object({
		type: z.literal('get_user_blocked_ids'),
		data: z.record(z.string(), z.array(z.string())),
	}),
	z.object({
		type: z.literal('get_visionary_slots'),
		data: z.object({
			total: z.number().int(),
			bought: z.number().int(),
			remaining: z.number().int(),
		}),
	}),
	z.object({
		type: z.literal('voice_get_token'),
		data: z.object({
			token: z.string(),
			endpoint: z.string(),
			connectionId: z.string(),
		}),
	}),
	z.object({
		type: z.literal('kick_temporary_member'),
		data: z.object({
			success: z.boolean(),
		}),
	}),
	z.object({
		type: z.literal('get_badge_counts'),
		data: z.object({
			badge_counts: z.record(z.string(), z.number().int().min(0)),
		}),
	}),
	z.object({
		type: z.literal('voice_force_disconnect_participant'),
		data: z.object({
			success: z.boolean(),
		}),
	}),
	z.object({
		type: z.literal('voice_update_participant'),
		data: z.object({
			success: z.boolean(),
		}),
	}),
	z.object({
		type: z.literal('voice_force_disconnect_channel'),
		data: z.object({
			success: z.boolean(),
			disconnected_count: z.number().optional(),
			message: z.string().optional(),
		}),
	}),
	z.object({
		type: z.literal('voice_update_participant_permissions'),
		data: z.object({
			success: z.boolean(),
		}),
	}),
	z.object({
		type: z.literal('call_ended'),
		data: z.object({
			success: z.boolean(),
		}),
	}),
	z.object({
		type: z.literal('validate_custom_status'),
		data: RpcResponseValidateCustomStatus,
	}),
	z.object({
		type: z.literal('geoip_lookup'),
		data: z.object({country_code: z.string()}),
	}),
	z.object({
		type: z.literal('get_dm_channel'),
		data: z.object({
			channel: ChannelResponse.nullish(),
		}),
	}),
]);

export type RpcResponse = z.infer<typeof RpcResponse>;
