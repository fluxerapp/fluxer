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

import type {Guild} from '~/Models';
import {createStringType, Int64Type, z} from '~/Schema';

export const mapGuildToAdminResponse = (guild: Guild): GuildAdminResponse => ({
	id: guild.id.toString(),
	name: guild.name,
	features: Array.from(guild.features),
	owner_id: guild.ownerId.toString(),
	icon: guild.iconHash,
	banner: guild.bannerHash,
	member_count: guild.memberCount,
});

export const GuildAdminResponse = z.object({
	id: z.string(),
	name: z.string(),
	features: z.array(z.string()),
	owner_id: z.string(),
	icon: z.string().nullable(),
	banner: z.string().nullable(),
	member_count: z.number(),
});

export type GuildAdminResponse = z.infer<typeof GuildAdminResponse>;

export const mapGuildsToAdminResponse = (guilds: Array<Guild>): GuildsAdminResponse => {
	return {
		guilds: [
			...guilds.map((guild) => {
				return {
					id: guild.id.toString(),
					name: guild.name,
					features: Array.from(guild.features),
					owner_id: guild.ownerId.toString(),
					icon: guild.iconHash,
					banner: guild.bannerHash,
					member_count: guild.memberCount,
				};
			}),
		],
	};
};

const ListGuildsAdminResponse = z.object({
	guilds: z.array(GuildAdminResponse),
});

type GuildsAdminResponse = z.infer<typeof ListGuildsAdminResponse>;

export const ListUserGuildsRequest = z.object({
	user_id: Int64Type,
});

export type ListUserGuildsRequest = z.infer<typeof ListUserGuildsRequest>;

export const LookupGuildRequest = z.object({
	guild_id: Int64Type,
});

export type LookupGuildRequest = z.infer<typeof LookupGuildRequest>;

export const ListGuildMembersRequest = z.object({
	guild_id: Int64Type,
	limit: z.number().default(50),
	offset: z.number().default(0),
});

export type ListGuildMembersRequest = z.infer<typeof ListGuildMembersRequest>;

export const SearchGuildsRequest = z.object({
	query: createStringType(1, 1024).optional(),
	limit: z.number().default(50),
	offset: z.number().default(0),
});

export type SearchGuildsRequest = z.infer<typeof SearchGuildsRequest>;

export const ReloadGuildRequest = z.object({
	guild_id: Int64Type,
});

export type ReloadGuildRequest = z.infer<typeof ReloadGuildRequest>;

export const ShutdownGuildRequest = z.object({
	guild_id: Int64Type,
});

export type ShutdownGuildRequest = z.infer<typeof ShutdownGuildRequest>;

export const GetProcessMemoryStatsRequest = z.object({
	limit: z.number().int().min(1).max(100).default(25),
});

export type GetProcessMemoryStatsRequest = z.infer<typeof GetProcessMemoryStatsRequest>;
