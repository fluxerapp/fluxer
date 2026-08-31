import type {ChannelID, GuildID, InviteCode, UserID} from '../BrandedTypes';
import type {GuildInviteBundleRow} from '../database/types/InviteBundleTypes';

export class GuildInviteBundle {
	readonly code: InviteCode;
	readonly uses: number;
	readonly maxUses: number;
	readonly maxAge: number;
	readonly createdAt: Date;
	readonly invites: Array<{
		guild_id: GuildID;
		channel_id: ChannelID;
		code: InviteCode;
	}>;
	readonly inviterId: UserID;

	constructor(row: GuildInviteBundleRow) {
		this.code = row.code;
		this.uses = row.uses;
		this.maxUses = row.max_uses;
		this.maxAge = row.max_age;
		this.createdAt = row.created_at;
		this.invites = row.invites;
		this.inviterId = row.inviter_id;
	}

	toRow(): GuildInviteBundleRow {
		return {
			code: this.code,
			uses: this.uses,
			max_uses: this.maxUses,
			max_age: this.maxAge,
			created_at: this.createdAt,
			invites: this.invites,
			inviter_id: this.inviterId,
		};
	}
}
