// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Users from '@app/features/user/state/Users';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {UserPartial, UserProfile} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface ProfileMutualGuild {
	id: string;
	nick: string | null;
}

export type ProfileWire = Readonly<{
	user: UserPartial;
	user_profile: UserProfile;
	guild_member_profile?: UserProfile | null;
	timezone_offset: number | null;
	guild_member?: GuildMemberData;
	premium_type?: number;
	premium_since?: string;
	premium_lifetime_sequence?: number;
	mutual_friends?: Array<UserPartial>;
	mutual_guilds?: Array<ProfileMutualGuild>;
	connected_accounts?: Array<ConnectionResponse>;
	profile_limited?: boolean;
}>;

export class Profile {
	readonly userId: string;
	readonly guildId: string | null;
	readonly userProfile: Readonly<UserProfile>;
	readonly guildMemberProfile: Readonly<UserProfile> | null;
	readonly timezoneOffset: number | null;
	readonly premiumType: number | null;
	readonly premiumSince: Date | null;
	readonly premiumLifetimeSequence: number | null;
	readonly mutualFriends: ReadonlyArray<UserPartial> | null;
	readonly mutualGuilds: ReadonlyArray<ProfileMutualGuild> | null;
	readonly connectedAccounts: ReadonlyArray<ConnectionResponse> | null;
	readonly profileLimited: boolean;
	private readonly embeddedGuildMember: GuildMember | null;

	constructor(profile: ProfileWire, guildId?: string) {
		this.userId = profile.user.id;
		this.guildId = guildId ?? null;
		this.userProfile = Object.freeze({...profile.user_profile});
		this.guildMemberProfile = profile.guild_member_profile ? Object.freeze({...profile.guild_member_profile}) : null;
		this.timezoneOffset = profile.timezone_offset;
		this.premiumType = profile.premium_type ?? null;
		this.premiumSince = profile.premium_since ? new Date(profile.premium_since) : null;
		this.premiumLifetimeSequence = profile.premium_lifetime_sequence ?? null;
		this.mutualFriends = profile.mutual_friends ? Object.freeze([...profile.mutual_friends]) : null;
		this.mutualGuilds = profile.mutual_guilds ? Object.freeze([...profile.mutual_guilds]) : null;
		this.connectedAccounts = profile.connected_accounts ? Object.freeze([...profile.connected_accounts]) : null;
		this.profileLimited = profile.profile_limited === true;
		this.embeddedGuildMember =
			this.guildId && profile.guild_member
				? new GuildMember(this.guildId, profile.guild_member, {cacheUser: false})
				: null;
	}

	withUpdates(updates: Partial<ProfileWire>): Profile {
		return new Profile(
			{
				user: {...this.toJSON().user, ...(updates.user ?? {})},
				user_profile: updates.user_profile ?? this.userProfile,
				guild_member_profile:
					updates.guild_member_profile === undefined ? this.guildMemberProfile : (updates.guild_member_profile ?? null),
				timezone_offset: updates.timezone_offset !== undefined ? updates.timezone_offset : this.timezoneOffset,
				guild_member:
					updates.guild_member !== undefined ? updates.guild_member : (this.embeddedGuildMember?.toJSON() ?? undefined),
				premium_type: updates.premium_type !== undefined ? updates.premium_type : (this.premiumType ?? undefined),
				premium_since:
					updates.premium_since !== undefined ? updates.premium_since : (this.premiumSince?.toISOString() ?? undefined),
				premium_lifetime_sequence:
					updates.premium_lifetime_sequence !== undefined
						? updates.premium_lifetime_sequence
						: (this.premiumLifetimeSequence ?? undefined),
				mutual_friends: updates.mutual_friends ?? (this.mutualFriends ? [...this.mutualFriends] : undefined),
				mutual_guilds: updates.mutual_guilds ?? (this.mutualGuilds ? [...this.mutualGuilds] : undefined),
				connected_accounts:
					updates.connected_accounts ?? (this.connectedAccounts ? [...this.connectedAccounts] : undefined),
				profile_limited: updates.profile_limited !== undefined ? updates.profile_limited : this.profileLimited,
			},
			this.guildId ?? undefined,
		);
	}

	withGuildId(guildId: string | null): Profile {
		return new Profile(this.toJSON(), guildId ?? undefined);
	}

	get guild(): Guild | null {
		if (!this.guildId) return null;
		return Guilds.getGuild(this.guildId) ?? null;
	}

	get guildMember(): GuildMember | null {
		if (!this.guildId) return null;
		return GuildMembers.getMember(this.guildId, this.userId) ?? this.embeddedGuildMember ?? null;
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

	toJSON(): ProfileWire {
		return {
			user: Users.getUser(this.userId)!.toJSON(),
			user_profile: {...this.userProfile},
			guild_member_profile: this.guildMemberProfile ? {...this.guildMemberProfile} : undefined,
			guild_member: this.embeddedGuildMember?.toJSON(),
			timezone_offset: this.timezoneOffset,
			premium_type: this.premiumType ?? undefined,
			premium_since: this.premiumSince?.toISOString() ?? undefined,
			premium_lifetime_sequence: this.premiumLifetimeSequence ?? undefined,
			mutual_friends: this.mutualFriends ? [...this.mutualFriends] : undefined,
			mutual_guilds: this.mutualGuilds ? [...this.mutualGuilds] : undefined,
			connected_accounts: this.connectedAccounts ? [...this.connectedAccounts] : undefined,
			profile_limited: this.profileLimited || undefined,
		};
	}
}
