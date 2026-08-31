// SPDX-License-Identifier: AGPL-3.0-or-later

import {InviteTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_INVITE_AGE_SECONDS, MAX_INVITE_USES} from '@fluxer/constants/src/LimitConstants';
import {type Channel, ChannelPartialResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {type Guild, GuildPartialResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {Int32Type} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ChannelInviteCreateRequest = z.object({
	max_uses: z
		.number()
		.int()
		.min(0)
		.max(MAX_INVITE_USES)
		.nullish()
		.default(0)
		.describe('Maximum number of times this invite can be used (0 for unlimited)'),
	max_age: z
		.number()
		.int()
		.min(0)
		.max(MAX_INVITE_AGE_SECONDS)
		.nullish()
		.default(0)
		.describe('Duration in seconds before the invite expires (0 for never)'),
	unique: z
		.boolean()
		.nullish()
		.default(false)
		.describe('Whether to create a new unique invite or reuse an existing one'),
	temporary: z
		.boolean()
		.nullish()
		.default(false)
		.describe('Whether members that joined via this invite should be kicked after disconnecting'),
});

export type ChannelInviteCreateRequest = z.infer<typeof ChannelInviteCreateRequest>;

export const GuildInviteResponse = z.object({
	code: z.string().describe('The unique invite code'),
	type: z.literal(InviteTypes.GUILD).describe('The type of invite (guild)'),
	guild: GuildPartialResponse.describe('The guild this invite is for'),
	channel: z.lazy(() => ChannelPartialResponse).describe('The channel this invite is for'),
	inviter: z
		.lazy(() => UserPartialResponse)
		.nullish()
		.describe('The user who created the invite'),
	member_count: Int32Type.describe('The approximate total member count of the guild'),
	presence_count: Int32Type.describe('The approximate online member count of the guild'),
	expires_at: z.iso.datetime().nullish().describe('ISO8601 timestamp of when the invite expires'),
	temporary: z.boolean().describe('Whether the invite grants temporary membership'),
});

export type GuildInviteResponse = z.infer<typeof GuildInviteResponse>;

export const GroupDmInviteResponse = z.object({
	code: z.string().describe('The unique invite code'),
	type: z.literal(InviteTypes.GROUP_DM).describe('The type of invite (group DM)'),
	channel: z.lazy(() => ChannelPartialResponse).describe('The group DM channel this invite is for'),
	inviter: z
		.lazy(() => UserPartialResponse)
		.nullish()
		.describe('The user who created the invite'),
	member_count: Int32Type.describe('The current member count of the group DM'),
	expires_at: z.iso.datetime().nullish().describe('ISO8601 timestamp of when the invite expires'),
	temporary: z.boolean().describe('Whether the invite grants temporary membership'),
});

export type GroupDmInviteResponse = z.infer<typeof GroupDmInviteResponse>;

export const GuildInviteMetadataResponse = z.object({
	...GuildInviteResponse.shape,
	created_at: z.iso.datetime().describe('ISO8601 timestamp of when the invite was created'),
	uses: Int32Type.describe('The number of times this invite has been used'),
	max_uses: Int32Type.describe('The maximum number of times this invite can be used'),
	max_age: Int32Type.describe('The duration in seconds before the invite expires'),
});

export type GuildInviteMetadataResponse = z.infer<typeof GuildInviteMetadataResponse>;

export const GroupDmInviteMetadataResponse = z.object({
	...GroupDmInviteResponse.shape,
	created_at: z.iso.datetime().describe('ISO8601 timestamp of when the invite was created'),
	uses: Int32Type.describe('The number of times this invite has been used'),
	max_uses: Int32Type.describe('The maximum number of times this invite can be used'),
});

export type GroupDmInviteMetadataResponse = z.infer<typeof GroupDmInviteMetadataResponse>;

export const InviteResponseSchema = z.union([GuildInviteResponse, GroupDmInviteResponse]);

export type InviteResponseSchema = z.infer<typeof InviteResponseSchema>;

export const InviteMetadataResponseSchema = z.union([GuildInviteMetadataResponse, GroupDmInviteMetadataResponse]);

export type InviteMetadataResponseSchema = z.infer<typeof InviteMetadataResponseSchema>;

interface InviteBase {
	readonly code: string;
	readonly type: number;
	readonly inviter?: UserPartialResponse | null;
	readonly expires_at: string | null;
	readonly temporary: boolean;
}

export interface GuildInvite extends InviteBase {
	readonly type: typeof InviteTypes.GUILD;
	readonly guild: Guild;
	readonly channel: Channel;
	readonly member_count: number;
	readonly presence_count: number;
	readonly uses?: number;
	readonly max_uses?: number;
	readonly created_at?: string;
}

export interface GroupDmInvite extends InviteBase {
	readonly type: typeof InviteTypes.GROUP_DM;
	readonly channel: Channel;
	readonly member_count: number;
}

export type Invite = GuildInvite | GroupDmInvite;
