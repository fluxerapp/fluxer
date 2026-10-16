// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/features/guild/models/Guild';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import type {UserProfile} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const EMPTY_ROLES: ReadonlyArray<GuildRole> = Object.freeze([]);

export type ProfileGuildMembership =
	| {
			kind: 'global';
			guildId: null;
			guild: null;
			member: null;
			roles: ReadonlyArray<GuildRole>;
	  }
	| {
			kind: 'guildUnavailable';
			guildId: string;
			guild: Guild | null;
			member: null;
			roles: ReadonlyArray<GuildRole>;
	  }
	| {
			kind: 'guildMember';
			guildId: string;
			guild: Guild | null;
			member: GuildMember;
			roles: ReadonlyArray<GuildRole>;
	  };

const GLOBAL_PROFILE_MEMBERSHIP: ProfileGuildMembership = Object.freeze({
	kind: 'global',
	guildId: null,
	guild: null,
	member: null,
	roles: EMPTY_ROLES,
});

interface ResolveProfileGuildMembershipOptions {
	fallbackGuildId?: string;
	userId?: string;
	allowStoreFallback?: boolean;
}

export function resolveProfileGuildMembership(
	profile: Profile | null | undefined,
	options: ResolveProfileGuildMembershipOptions = {},
): ProfileGuildMembership {
	const profileGuildId = profile?.guildId ?? null;
	const guildId = profileGuildId ?? (options.allowStoreFallback ? (options.fallbackGuildId ?? null) : null);
	if (!guildId) {
		return GLOBAL_PROFILE_MEMBERSHIP;
	}
	const storeMember =
		options.allowStoreFallback && options.userId ? GuildMembers.getMember(guildId, options.userId) : null;
	const profileMember = profileGuildId === guildId ? (profile?.guildMember ?? null) : null;
	const member = storeMember ?? profileMember;
	const guild = profile?.guildId === guildId ? (profile.guild ?? null) : (Guilds.getGuild(guildId) ?? null);
	if (!member) {
		return {
			kind: 'guildUnavailable',
			guildId,
			guild,
			member: null,
			roles: EMPTY_ROLES,
		};
	}
	return {
		kind: 'guildMember',
		guildId,
		guild,
		member,
		roles: member.getSortedRoles(),
	};
}

export function getProfileMembershipDisplayName(
	user: User,
	membership: ProfileGuildMembership,
	fallbackGuildId?: string,
): string {
	if (membership.kind === 'guildMember') {
		return NicknameUtils.formatNameForStreamerMode(membership.member.nick ?? user.displayName);
	}
	return NicknameUtils.getNickname(user, fallbackGuildId);
}

export function toProfileDisplayContext({
	user,
	profile,
	membership,
	guildMemberProfile,
}: {
	user: User;
	profile: Profile | null | undefined;
	membership: ProfileGuildMembership;
	guildMemberProfile?: UserProfile | null;
}): ProfileDisplayUtils.ProfileDisplayContext {
	return {
		user,
		profile,
		guildId: membership.kind === 'guildMember' ? membership.guildId : undefined,
		guildMember: membership.kind === 'guildMember' ? membership.member : undefined,
		guildMemberProfile: membership.kind === 'guildMember' ? (guildMemberProfile ?? null) : undefined,
	};
}
