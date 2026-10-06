// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, InviteCode, RoleID, UserID} from '@app/api/BrandedTypes';
import type {GuildMemberRow} from '@app/api/database/types/GuildTypes';
import {GuildMemberProfileFlags, type JoinSourceType} from '@fluxer/constants/src/GuildConstants';
import type {MentionReplyPreference} from '@fluxer/constants/src/UserConstants';

export class GuildMember {
	readonly guildId: GuildID;
	readonly userId: UserID;
	readonly joinedAt: Date;
	readonly nickname: string | null;
	readonly avatarHash: string | null;
	readonly bannerHash: string | null;
	readonly bio: string | null;
	readonly pronouns: string | null;
	readonly accentColor: number | null;
	readonly joinSourceType: JoinSourceType | null;
	readonly sourceInviteCode: InviteCode | null;
	readonly inviterId: UserID | null;
	readonly isDeaf: boolean;
	readonly isMute: boolean;
	readonly communicationDisabledUntil: Date | null;
	readonly roleIds: Set<RoleID>;
	readonly isPremiumSanitized: boolean;
	readonly isTemporary: boolean;
	readonly profileFlags: number;
	readonly mentionFlags: MentionReplyPreference;
	readonly version: number;

	constructor(row: GuildMemberRow) {
		this.guildId = row.guild_id;
		this.userId = row.user_id;
		this.joinedAt = row.joined_at;
		this.nickname = row.nick ?? null;
		this.avatarHash = row.avatar_hash ?? null;
		this.bannerHash = row.banner_hash ?? null;
		this.bio = row.bio ?? null;
		this.pronouns = row.pronouns ?? null;
		this.accentColor = row.accent_color ?? null;
		this.joinSourceType = (row.join_source_type ?? null) as JoinSourceType | null;
		this.sourceInviteCode = row.source_invite_code ?? null;
		this.inviterId = row.inviter_id ?? null;
		this.isDeaf = row.deaf ?? false;
		this.isMute = row.mute ?? false;
		this.communicationDisabledUntil = row.communication_disabled_until ?? null;
		this.roleIds = row.role_ids ?? new Set();
		this.isPremiumSanitized = row.is_premium_sanitized ?? false;
		this.isTemporary = row.temporary ?? false;
		this.profileFlags = row.profile_flags ?? 0;
		this.mentionFlags = row.mention_flags ?? 0;
		this.version = row.version;
	}

	hasProfileFlag(flag: number): boolean {
		return (this.profileFlags & flag) === flag;
	}

	isAvatarUnset(): boolean {
		return this.hasProfileFlag(GuildMemberProfileFlags.AVATAR_UNSET);
	}

	isBannerUnset(): boolean {
		return this.hasProfileFlag(GuildMemberProfileFlags.BANNER_UNSET);
	}

	toRow(): GuildMemberRow {
		return {
			guild_id: this.guildId,
			user_id: this.userId,
			joined_at: this.joinedAt,
			nick: this.nickname,
			avatar_hash: this.avatarHash,
			banner_hash: this.bannerHash,
			bio: this.bio,
			pronouns: this.pronouns,
			accent_color: this.accentColor,
			join_source_type: this.joinSourceType,
			source_invite_code: this.sourceInviteCode,
			inviter_id: this.inviterId,
			deaf: this.isDeaf,
			mute: this.isMute,
			communication_disabled_until: this.communicationDisabledUntil,
			role_ids: this.roleIds.size > 0 ? this.roleIds : null,
			is_premium_sanitized: this.isPremiumSanitized,
			temporary: this.isTemporary,
			profile_flags: this.profileFlags || null,
			mention_flags: this.mentionFlags || null,
			version: this.version,
		};
	}
}
