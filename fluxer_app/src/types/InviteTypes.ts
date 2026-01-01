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

import {InviteTypes} from '~/Constants';
import type {Channel} from '~/records/ChannelRecord';
import type {Guild} from '~/records/GuildRecord';
import type {UserPartial} from '~/records/UserRecord';
import type {PackSummary} from '~/types/PackTypes';

export type InviteTypeValue = (typeof InviteTypes)[keyof typeof InviteTypes];

export interface InviteBase {
	code: string;
	type: InviteTypeValue;
	inviter?: UserPartial | null;
	expires_at: string | null;
	temporary: boolean;
}

export interface GuildInvite extends InviteBase {
	type: typeof InviteTypes.GUILD;
	guild: Guild;
	channel: Channel;
	member_count: number;
	presence_count: number;
	uses?: number;
	max_uses?: number;
	created_at?: string;
}

export interface GroupDmInvite extends InviteBase {
	type: typeof InviteTypes.GROUP_DM;
	channel: Channel;
	member_count: number;
}

export type PackInviteType = typeof InviteTypes.EMOJI_PACK | typeof InviteTypes.STICKER_PACK;

export interface PackInvite extends InviteBase {
	type: PackInviteType;
	pack: PackSummary & {creator: UserPartial};
}

export type Invite = GuildInvite | GroupDmInvite | PackInvite;

export const isGuildInvite = (invite: Invite): invite is GuildInvite => invite.type === InviteTypes.GUILD;
export const isGroupDmInvite = (invite: Invite): invite is GroupDmInvite => invite.type === InviteTypes.GROUP_DM;
export const isPackInvite = (invite: Invite): invite is PackInvite =>
	invite.type === InviteTypes.EMOJI_PACK || invite.type === InviteTypes.STICKER_PACK;
