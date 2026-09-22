// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, InviteCode, UserID} from '@app/api/BrandedTypes';
import type {Invite} from '@app/api/models/Invite';

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
}
