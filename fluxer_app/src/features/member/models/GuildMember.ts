// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import Guilds from '@app/features/guild/state/Guilds';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {GuildMemberProfileFlags} from '@fluxer/constants/src/GuildConstants';
import type {MentionReplyPreference} from '@fluxer/constants/src/UserConstants';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

interface GuildMemberRecordOptions {
	instanceId?: string;
	cacheUser?: boolean;
}

export class GuildMember {
	readonly instanceId: string;
	readonly guildId: string;
	readonly user: User;
	private readonly cacheUser: boolean;
	readonly nick: string | null;
	readonly avatar: string | null;
	readonly banner: string | null;
	readonly accentColor: number | null;
	readonly roles: ReadonlySet<string>;
	readonly joinedAt: Date;
	readonly mute: boolean;
	readonly deaf: boolean;
	readonly communicationDisabledUntil: Date | null;
	readonly profileFlags: number;
	readonly mentionFlags: MentionReplyPreference;

	constructor(guildId: string, guildMember: GuildMemberData, options?: GuildMemberRecordOptions) {
		this.instanceId = options?.instanceId ?? RuntimeConfig.localInstanceDomain;
		this.guildId = guildId;
		this.cacheUser = options?.cacheUser !== false;
		const cachedUser = Users.getUser(guildMember.user.id);
		if (cachedUser) {
			this.user = cachedUser;
		} else {
			this.user = new User(guildMember.user, {instanceId: this.instanceId});
			if (this.cacheUser) {
				Users.cacheUsers([this.user.toJSON()]);
			}
		}
		this.nick = guildMember.nick ?? null;
		this.avatar = guildMember.avatar ?? null;
		this.banner = guildMember.banner ?? null;
		this.accentColor = guildMember.accent_color ?? null;
		this.roles = new Set(guildMember.roles);
		this.joinedAt = new Date(guildMember.joined_at);
		this.mute = guildMember.mute ?? false;
		this.deaf = guildMember.deaf ?? false;
		this.communicationDisabledUntil = guildMember.communication_disabled_until
			? new Date(guildMember.communication_disabled_until)
			: null;
		this.profileFlags = guildMember.profile_flags ?? 0;
		this.mentionFlags = (guildMember.mention_flags ?? 0) as MentionReplyPreference;
	}

	matches(guildMember: GuildMemberData): boolean {
		const until = guildMember.communication_disabled_until
			? new Date(guildMember.communication_disabled_until).getTime()
			: null;
		return (
			this.nick === (guildMember.nick ?? null) &&
			this.avatar === (guildMember.avatar ?? null) &&
			this.banner === (guildMember.banner ?? null) &&
			this.accentColor === (guildMember.accent_color ?? null) &&
			this.mute === (guildMember.mute ?? false) &&
			this.deaf === (guildMember.deaf ?? false) &&
			this.profileFlags === (guildMember.profile_flags ?? 0) &&
			this.mentionFlags === (guildMember.mention_flags ?? 0) &&
			(this.communicationDisabledUntil?.getTime() ?? null) === until &&
			this.roles.size === guildMember.roles.length &&
			guildMember.roles.every((roleId) => this.roles.has(roleId))
		);
	}

	isAvatarUnset(): boolean {
		return (this.profileFlags & GuildMemberProfileFlags.AVATAR_UNSET) !== 0;
	}

	isBannerUnset(): boolean {
		return (this.profileFlags & GuildMemberProfileFlags.BANNER_UNSET) !== 0;
	}

	getSortedRoles(): ReadonlyArray<GuildRole> {
		const guild = Guilds.getGuild(this.guildId);
		if (!guild) {
			return [];
		}
		return Array.from(this.roles)
			.map((roleId) => guild.roles[roleId])
			.filter((role): role is GuildRole => role !== undefined)
			.sort((a, b) => {
				if (b.position !== a.position) {
					return b.position - a.position;
				}
				return BigInt(a.id) < BigInt(b.id) ? -1 : 1;
			});
	}

	getColorString(): string | undefined {
		const sortedRoles = this.getSortedRoles();
		for (const role of sortedRoles) {
			if (role.color) {
				return ColorUtils.int2rgb(role.color);
			}
		}
		const guild = Guilds.getGuild(this.guildId);
		if (guild) {
			const everyoneRole = guild.roles[this.guildId];
			if (everyoneRole?.color) {
				return ColorUtils.int2rgb(everyoneRole.color);
			}
		}
		return;
	}

	isCurrentUser(): boolean {
		return this.user.id === Authentication.currentUserId;
	}

	isTimedOut(): boolean {
		if (!this.communicationDisabledUntil) {
			return false;
		}
		return this.communicationDisabledUntil.getTime() > Date.now();
	}

	toJSON(): GuildMemberData {
		return {
			user: this.user.toJSON(),
			nick: this.nick,
			avatar: this.avatar,
			banner: this.banner,
			accent_color: this.accentColor,
			roles: Array.from(this.roles),
			joined_at: this.joinedAt.toISOString(),
			mute: this.mute,
			deaf: this.deaf,
			communication_disabled_until: this.communicationDisabledUntil?.toISOString() ?? null,
			profile_flags: this.profileFlags,
			mention_flags: this.mentionFlags,
		};
	}
}
