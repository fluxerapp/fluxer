// SPDX-License-Identifier: AGPL-3.0-or-later

import {createStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const TemplateEntityId = z
	.union([
		z.number().int().nonnegative(),
		createStringType(1, 64).refine((value) => /^\d+$/.test(value), 'Template IDs must be numeric'),
	])
	.describe('The template-local numeric identifier');
const TemplatePermissionBitfield = z
	.union([z.string(), z.number().int().nonnegative()])
	.describe('The permissions bitfield as a string or integer');
const TemplateOverwriteType = z
	.union([z.number(), z.string()])
	.transform((value) => {
		if (typeof value === 'string') {
			if (value === 'role') return 0;
			if (value === 'member') return 1;
			return Number(value);
		}
		return value;
	})
	.pipe(z.number())
	.describe('The type of overwrite (0/role = role, 1/member = member)');
const TemplatePermissionOverwrite = z.object({
	id: TemplateEntityId.describe('The ID of the role or user for this overwrite'),
	type: TemplateOverwriteType,
	allow: TemplatePermissionBitfield.describe('The allowed permissions bitfield as a string'),
	deny: TemplatePermissionBitfield.describe('The denied permissions bitfield as a string'),
});

export const TemplateChannel = z.object({
	id: TemplateEntityId.describe('The template-local channel ID'),
	type: z.number().describe('The channel type (0 = text, 2 = voice, 4 = category)'),
	name: z
		.string()
		.nullish()
		.transform((value) => value ?? '')
		.describe('The name of the channel'),
	topic: z.string().nullish().describe('The channel topic'),
	position: z.number().describe('The position of the channel'),
	parent_id: TemplateEntityId.nullish().describe('The template-local ID of the parent category'),
	bitrate: z.number().nullish().describe('The bitrate for voice channels'),
	user_limit: z.number().nullish().describe('The user limit for voice channels'),
	voice_connection_limit: z.number().nullish().describe('The per-user voice connection limit for voice channels'),
	nsfw: z.boolean().optional().describe('Whether the channel is NSFW'),
	rate_limit_per_user: z.number().optional().describe('Slowmode rate limit in seconds'),
	permission_overwrites: z
		.array(TemplatePermissionOverwrite)
		.optional()
		.describe('Permission overwrites for this channel'),
});

export type TemplateChannel = z.infer<typeof TemplateChannel>;

export const TemplateRole = z.object({
	id: TemplateEntityId.describe('The template-local role ID'),
	name: z
		.string()
		.nullish()
		.transform((value) => value ?? '')
		.describe('The name of the role'),
	permissions: TemplatePermissionBitfield.optional().describe('The permissions bitfield as a string (legacy)'),
	permissions_new: TemplatePermissionBitfield.optional().describe('The permissions bitfield as a string (preferred)'),
	color: z.number().optional().describe('The colour of the role as an integer'),
	hoist: z.boolean().optional().describe('Whether the role is hoisted'),
	mentionable: z.boolean().optional().describe('Whether the role is mentionable'),
	unicode_emoji: z.string().nullish().describe('The unicode emoji for the role icon'),
});

export type TemplateRole = z.infer<typeof TemplateRole>;

export const TemplateSerializedGuild = z.object({
	name: z.string().describe('The name of the template guild'),
	description: z.string().nullish().describe('The description of the template guild'),
	verification_level: z.number().optional().describe('The verification level'),
	default_message_notifications: z.number().optional().describe('The default message notification level'),
	explicit_content_filter: z.number().optional().describe('The explicit content filter level'),
	system_channel_id: TemplateEntityId.nullish().describe('The template-local system channel ID'),
	afk_timeout: z.number().optional().describe('The AFK timeout in seconds'),
	system_channel_flags: z.number().optional().describe('The system channel flags'),
	roles: z.array(TemplateRole).describe('The roles in the template'),
	channels: z.array(TemplateChannel).describe('The channels in the template'),
});

export type TemplateSerializedGuild = z.infer<typeof TemplateSerializedGuild>;
