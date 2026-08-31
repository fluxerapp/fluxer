import type {ChannelID, GuildID, InviteCode, UserID} from '../../BrandedTypes';

export const GUILD_INVITE_BUNDLE_COLUMNS = [
	'code',
	'invites',
	'inviter_id',
	'created_at',
	'uses',
	'max_uses',
	'max_age',
] as const satisfies ReadonlyArray<keyof GuildInviteBundleRow>;

export interface GuildInviteBundleRow {
	code: InviteCode;
	invites: Array<{
		guild_id: GuildID;
		channel_id: ChannelID;
		code: InviteCode;
	}>;
	inviter_id: UserID;
	created_at: Date;
	uses: number;
	max_uses: number;
	max_age: number;
}
