// SPDX-License-Identifier: AGPL-3.0-or-later

import {PermissionStringType} from '@fluxer/schema/src/primitives/PermissionValidators';
import {Int32Type, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GuildRoleResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this role'),
	name: z.string().describe('The name of the role'),
	color: Int32Type.describe('The colour of the role as an integer'),
	position: Int32Type.describe('The position of the role in the role hierarchy'),
	hoist_position: Int32Type.nullish().describe('The position of the role in the hoisted member list'),
	permissions: PermissionStringType.describe('The permissions bitfield for the role'),
	hoist: z.boolean().describe('Whether this role is displayed separately in the member list'),
	mentionable: z.boolean().describe('Whether this role can be mentioned by anyone'),
	unicode_emoji: z.string().nullish().describe('The unicode emoji for this role'),
});

export type GuildRoleResponse = z.infer<typeof GuildRoleResponse>;

export type GuildRole = Readonly<GuildRoleResponse>;

export const GuildRoleListResponse = z.array(GuildRoleResponse);
