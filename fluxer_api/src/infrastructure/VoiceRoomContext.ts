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

import {z} from 'zod';
import type {ChannelID, GuildID, UserID} from '~/BrandedTypes';
import {createChannelID, createGuildID, createUserID} from '~/BrandedTypes';

interface DMRoomContext {
	readonly type: 'dm';
	readonly channelId: ChannelID;
}

interface GuildRoomContext {
	readonly type: 'guild';
	readonly channelId: ChannelID;
	readonly guildId: GuildID;
}

type VoiceRoomContext = DMRoomContext | GuildRoomContext;

interface DMParticipantContext {
	readonly type: 'dm';
	readonly userId: UserID;
	readonly channelId: ChannelID;
	readonly connectionId: string;
}

interface GuildParticipantContext {
	readonly type: 'guild';
	readonly userId: UserID;
	readonly channelId: ChannelID;
	readonly connectionId: string;
	readonly guildId: GuildID;
}

type ParticipantContext = DMParticipantContext | GuildParticipantContext;

const SnowflakeStringSchema = z.string().regex(/^\d+$/, 'Must be a numeric string');

const DMParticipantMetadataSchema = z.object({
	user_id: SnowflakeStringSchema,
	channel_id: SnowflakeStringSchema,
	connection_id: z.string().min(1),
	dm_call: z.union([z.literal('true'), z.literal(true)]),
	region_id: z.string().optional(),
	server_id: z.string().optional(),
});

const GuildParticipantMetadataSchema = z.object({
	user_id: SnowflakeStringSchema,
	channel_id: SnowflakeStringSchema,
	connection_id: z.string().min(1),
	guild_id: SnowflakeStringSchema,
	region_id: z.string().optional(),
	server_id: z.string().optional(),
});

const ParticipantMetadataSchema = z.union([DMParticipantMetadataSchema, GuildParticipantMetadataSchema]);

type RawParticipantMetadata = z.infer<typeof ParticipantMetadataSchema>;

const DM_ROOM_PREFIX = 'dm_channel_';
const GUILD_ROOM_PREFIX = 'guild_';

export function parseRoomName(roomName: string): VoiceRoomContext | null {
	if (roomName.startsWith(DM_ROOM_PREFIX)) {
		const channelIdStr = roomName.slice(DM_ROOM_PREFIX.length);
		try {
			return {
				type: 'dm',
				channelId: createChannelID(BigInt(channelIdStr)),
			};
		} catch {
			return null;
		}
	}

	if (roomName.startsWith(GUILD_ROOM_PREFIX)) {
		const parts = roomName.split('_');
		if (parts.length === 4 && parts[0] === 'guild' && parts[2] === 'channel') {
			try {
				return {
					type: 'guild',
					guildId: createGuildID(BigInt(parts[1])),
					channelId: createChannelID(BigInt(parts[3])),
				};
			} catch {
				return null;
			}
		}
	}

	return null;
}

export function parseParticipantMetadataWithRaw(
	metadata: string,
): {context: ParticipantContext; raw: RawParticipantMetadata} | null {
	try {
		const parsed = JSON.parse(metadata);
		const result = ParticipantMetadataSchema.safeParse(parsed);

		if (!result.success) {
			return null;
		}

		const data = result.data;
		const userId = createUserID(BigInt(data.user_id));
		const channelId = createChannelID(BigInt(data.channel_id));
		const connectionId = data.connection_id;

		if ('dm_call' in data) {
			return {
				context: {
					type: 'dm',
					userId,
					channelId,
					connectionId,
				},
				raw: data,
			};
		}

		return {
			context: {
				type: 'guild',
				userId,
				channelId,
				connectionId,
				guildId: createGuildID(BigInt(data.guild_id)),
			},
			raw: data,
		};
	} catch {
		return null;
	}
}

export function isDMRoom(context: VoiceRoomContext): context is DMRoomContext {
	return context.type === 'dm';
}

const PARTICIPANT_IDENTITY_PREFIX = 'user_';

interface ParticipantIdentity {
	readonly userId: UserID;
	readonly connectionId: string;
}

export function parseParticipantIdentity(identity: string): ParticipantIdentity | null {
	if (!identity.startsWith(PARTICIPANT_IDENTITY_PREFIX)) {
		return null;
	}

	const parts = identity.split('_');
	if (parts.length !== 3 || parts[0] !== 'user') {
		return null;
	}

	try {
		return {
			userId: createUserID(BigInt(parts[1])),
			connectionId: parts[2],
		};
	} catch {
		return null;
	}
}
