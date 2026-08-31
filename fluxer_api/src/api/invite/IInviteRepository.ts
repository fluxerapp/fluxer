// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, InviteCode, UserID} from '../BrandedTypes';
import type {GuildInviteBundle} from '../models/GuildInviteBundle';
import type {Invite} from '../models/Invite';

export abstract class IInviteRepository {
	abstract findUnique(code: InviteCode): Promise<Invite | null>;

	abstract listChannelInvites(channelId: ChannelID): Promise<Array<Invite>>;

	abstract listGuildInvites(guildId: GuildID): Promise<Array<Invite>>;

	abstract create(data: {
		code: InviteCode;
		type: number;
		guild_id: GuildID | null;
		channel_id?: ChannelID | null;
		inviter_id?: UserID | null;
		uses: number;
		max_uses: number;
		max_age: number;
		temporary?: boolean;
	}): Promise<Invite>;

	abstract updateInviteUses(code: InviteCode, uses: number, invite: Invite): Promise<void>;

	abstract delete(code: InviteCode): Promise<void>;

	abstract createGuildBundle(data: {
		code: InviteCode;
		max_uses: number;
		max_age: number;
		uses: number;
		invites: Array<{
			guild_id: GuildID;
			channel_id: ChannelID;
			code: InviteCode;
		}>;
		inviter_id: UserID;
	}): Promise<GuildInviteBundle>;

	abstract findUniqueGuildBundle(code: InviteCode): Promise<GuildInviteBundle | null>;
}
