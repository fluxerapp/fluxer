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

import type {GuildMember, GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserPartial, UserProfile} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';

export type Profile = Readonly<{
	user: UserPartial;
	user_profile: UserProfile;
	guild_member_profile?: UserProfile | null;
	timezone_offset: number | null;
	guild_member?: GuildMember;
	premium_type?: number;
	premium_since?: string;
	premium_lifetime_sequence?: number;
	mutual_friends?: Array<UserPartial>;
}>;

export class ProfileRecord {
	readonly userId: string;
	readonly guildId: string | null;
	readonly userProfile: Readonly<UserProfile>;
	readonly guildMemberProfile: Readonly<UserProfile> | null;
	readonly timezoneOffset: number | null;
	readonly premiumType: number | null;
	readonly premiumSince: Date | null;
	readonly premiumLifetimeSequence: number | null;
	readonly mutualFriends: ReadonlyArray<UserPartial> | null;

	constructor(profile: Profile, guildId?: string) {
		this.userId = profile.user.id;
		this.guildId = guildId ?? null;
		this.userProfile = Object.freeze({...profile.user_profile});
		this.guildMemberProfile = profile.guild_member_profile ? Object.freeze({...profile.guild_member_profile}) : null;
		this.timezoneOffset = profile.timezone_offset;
		this.premiumType = profile.premium_type ?? null;
		this.premiumSince = profile.premium_since ? new Date(profile.premium_since) : null;
		this.premiumLifetimeSequence = profile.premium_lifetime_sequence ?? null;
		this.mutualFriends = profile.mutual_friends ? Object.freeze([...profile.mutual_friends]) : null;
	}

	withUpdates(updates: Partial<Profile>): ProfileRecord {
		return new ProfileRecord(
			{
				user: {...this.toJSON().user, ...(updates.user ?? {})},
				user_profile: updates.user_profile ?? this.userProfile,
				guild_member_profile:
					updates.guild_member_profile === undefined ? this.guildMemberProfile : (updates.guild_member_profile ?? null),
				timezone_offset: updates.timezone_offset ?? this.timezoneOffset,
				guild_member: updates.guild_member,
				premium_type: updates.premium_type !== undefined ? updates.premium_type : (this.premiumType ?? undefined),
				premium_since:
					updates.premium_since !== undefined ? updates.premium_since : (this.premiumSince?.toISOString() ?? undefined),
				premium_lifetime_sequence:
					updates.premium_lifetime_sequence !== undefined
						? updates.premium_lifetime_sequence
						: (this.premiumLifetimeSequence ?? undefined),
				mutual_friends: updates.mutual_friends ?? (this.mutualFriends ? [...this.mutualFriends] : undefined),
			},
			this.guildId ?? undefined,
		);
	}

	withGuildId(guildId: string | null): ProfileRecord {
		return new ProfileRecord(this.toJSON(), guildId ?? undefined);
	}

	get guild(): GuildRecord | null {
		if (!this.guildId) return null;
		return GuildStore.getGuild(this.guildId) ?? null;
	}

	get guildMember(): GuildMemberRecord | null {
		if (!this.guildId) return null;
		return GuildMemberStore.getMember(this.guildId, this.userId) ?? null;
	}

	getGuildMemberProfile(): Readonly<UserProfile> | null {
		return this.guildMemberProfile;
	}

	getEffectiveProfile(): Readonly<UserProfile> {
		if (!this.guildMemberProfile) {
			return this.userProfile;
		}

		const guildMember = this.guildMember;
		const isBannerUnset = guildMember?.isBannerUnset() ?? false;
		const bannerColor = isBannerUnset
			? null
			: (this.guildMemberProfile?.banner_color ?? this.userProfile.banner_color ?? null);

		return {
			bio: this.guildMemberProfile.bio ?? this.userProfile.bio,
			banner: isBannerUnset ? null : (this.guildMemberProfile.banner ?? this.userProfile.banner),
			banner_color: bannerColor,
			pronouns: this.guildMemberProfile.pronouns ?? this.userProfile.pronouns,
			accent_color: this.guildMemberProfile.accent_color ?? this.userProfile.accent_color,
		};
	}

	equals(other: ProfileRecord): boolean {
		return (
			this.userId === other.userId &&
			this.guildId === other.guildId &&
			JSON.stringify(this.userProfile) === JSON.stringify(other.userProfile) &&
			JSON.stringify(this.guildMemberProfile) === JSON.stringify(other.guildMemberProfile) &&
			this.timezoneOffset === other.timezoneOffset &&
			this.premiumType === other.premiumType &&
			this.premiumSince === other.premiumSince &&
			this.premiumLifetimeSequence === other.premiumLifetimeSequence &&
			JSON.stringify(this.mutualFriends) === JSON.stringify(other.mutualFriends)
		);
	}

	toJSON(): Profile {
		return {
			user: UserStore.getUser(this.userId)!,
			user_profile: {...this.userProfile},
			guild_member_profile: this.guildMemberProfile ? {...this.guildMemberProfile} : undefined,
			timezone_offset: this.timezoneOffset,
			premium_type: this.premiumType ?? undefined,
			premium_since: this.premiumSince?.toISOString() ?? undefined,
			premium_lifetime_sequence: this.premiumLifetimeSequence ?? undefined,
			mutual_friends: this.mutualFriends ? [...this.mutualFriends] : undefined,
		};
	}
}
