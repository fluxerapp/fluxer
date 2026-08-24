// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel as ChannelModel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild as GuildModel} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import type {User as UserModel} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import type {ChannelId, GuildId, UserId} from '@fluxer/schema/src/branded/WireIds';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {makeAutoObservable, observable, reaction} from 'mobx';

const isChannelLike = (value: unknown): value is ChannelModel | WireChannel => {
	return Boolean(value && typeof value === 'object' && 'type' in value && 'id' in value);
};
const isGuildLike = (value: unknown): value is GuildModel | WireGuild => {
	return Boolean(value && typeof value === 'object' && ('owner_id' in value || 'ownerId' in value));
};

class Permission {
	private readonly guildPermissions = observable.map<GuildId, bigint>();
	private readonly channelPermissions = observable.map<ChannelId, bigint>();
	private readonly guildVersions = observable.map<GuildId, number>();
	private globalVersion = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getChannelPermissions(channelId: string): bigint | undefined {
		return this.channelPermissions.get(channelId as ChannelId);
	}

	getGuildPermissions(guildId: string): bigint | undefined {
		return this.guildPermissions.get(guildId as GuildId);
	}

	getGuildVersion(guildId: string): number | undefined {
		return this.guildVersions.get(guildId as GuildId);
	}

	get version(): number {
		return this.globalVersion;
	}

	can(
		permission: bigint,
		context:
			| ChannelModel
			| WireChannel
			| GuildModel
			| WireGuild
			| {
					channelId?: string;
					guildId?: string;
			  },
	): boolean {
		let permissions = PermissionUtils.NONE;
		if (isChannelLike(context)) {
			permissions = this.channelPermissions.get(context.id as ChannelId) ?? PermissionUtils.NONE;
		} else if (isGuildLike(context)) {
			permissions = this.guildPermissions.get(context.id as GuildId) ?? PermissionUtils.NONE;
		} else if (context.channelId) {
			permissions = this.channelPermissions.get(context.channelId as ChannelId) ?? PermissionUtils.NONE;
		} else if (context.guildId) {
			permissions = this.guildPermissions.get(context.guildId as GuildId) ?? PermissionUtils.NONE;
		}
		return (permissions & permission) === permission;
	}

	canManageUser(permission: bigint, otherUser: UserModel | UserId, guild: GuildModel): boolean {
		const otherUserId = typeof otherUser === 'string' ? otherUser : otherUser.id;
		if (guild.ownerId === otherUserId) {
			return false;
		}
		const me = Users.currentUser;
		if (!me) return false;
		if (!this.can(permission, guild)) {
			return false;
		}
		const wireGuild = guild.toJSON();
		const myHighestRole = PermissionUtils.getHighestRole(wireGuild, me.id);
		const otherHighestRole = PermissionUtils.getHighestRole(wireGuild, otherUserId);
		return PermissionUtils.isRoleHigher(wireGuild, me.id, myHighestRole, otherHighestRole);
	}

	handleGatewayReady(): void {
		this.rebuildPermissions();
	}

	handleConnectionClose(): void {
		this.guildPermissions.clear();
		this.channelPermissions.clear();
		this.guildVersions.clear();
		this.bumpGlobalVersion();
	}

	handleGuild(): void {
		this.rebuildPermissions();
	}

	handleGuildMemberUpdate(userId: string): void {
		const currentUser = Users.currentUser;
		if (!currentUser) return;
		if (userId !== currentUser.id) return;
		this.rebuildPermissions();
	}

	handleUserUpdate(userId: string): void {
		this.handleGuildMemberUpdate(userId);
	}

	handleChannelUpdate(channelId: string): void {
		const channel = Channels.getChannel(channelId);
		if (!channel) {
			return;
		}
		const currentUser = Users.currentUser;
		if (!currentUser) return;
		this.channelPermissions.set(
			channel.id as ChannelId,
			PermissionUtils.computePermissions(currentUser, channel.toJSON()),
		);
		this.bumpGuildVersion(channel.guildId);
	}

	handleChannelDelete(channelId: string, guildId?: string): void {
		this.channelPermissions.delete(channelId as ChannelId);
		this.bumpGuildVersion(guildId);
	}

	handleGuildRole(guildId: string): void {
		const currentUser = Users.currentUser;
		if (!currentUser) return;
		const guild = Guilds.getGuild(guildId);
		if (!guild) return;
		this.guildPermissions.set(guildId as GuildId, PermissionUtils.computePermissions(currentUser, guild.toJSON()));
		for (const channel of Channels.channels) {
			if (channel.guildId === guildId) {
				this.channelPermissions.set(
					channel.id as ChannelId,
					PermissionUtils.computePermissions(currentUser, channel.toJSON()),
				);
			}
		}
		this.bumpGuildVersion(guildId);
	}

	private rebuildPermissions(): void {
		const user = Users.currentUser;
		if (!user) {
			this.guildPermissions.clear();
			this.channelPermissions.clear();
			this.guildVersions.clear();
			this.bumpGlobalVersion();
			return;
		}
		this.guildPermissions.clear();
		this.channelPermissions.clear();
		for (const guild of Guilds.getGuilds()) {
			this.guildPermissions.set(guild.id as GuildId, PermissionUtils.computePermissions(user, guild.toJSON()));
			this.bumpGuildVersion(guild.id);
		}
		for (const channel of Channels.channels) {
			if (Object.keys(channel.permissionOverwrites).length === 0) {
				if (channel.guildId != null) {
					const guildPerms = this.guildPermissions.get(channel.guildId as GuildId) ?? PermissionUtils.NONE;
					this.channelPermissions.set(channel.id as ChannelId, guildPerms);
				} else {
					this.channelPermissions.set(channel.id as ChannelId, PermissionUtils.NONE);
				}
			} else {
				this.channelPermissions.set(
					channel.id as ChannelId,
					PermissionUtils.computePermissions(user, channel.toJSON()),
				);
			}
			this.bumpGuildVersion(channel.guildId);
		}
		this.bumpGlobalVersion();
	}

	private bumpGlobalVersion(): void {
		this.globalVersion += 1;
	}

	private bumpGuildVersion(guildId?: string | null): void {
		if (!guildId) return;
		const current = this.guildVersions.get(guildId as GuildId) ?? 0;
		this.guildVersions.set(guildId as GuildId, current + 1);
		this.bumpGlobalVersion();
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.version,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new Permission();
