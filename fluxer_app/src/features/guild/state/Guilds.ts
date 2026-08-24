// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {Guild} from '@app/features/guild/models/Guild';
import {GuildRole} from '@app/features/guild/models/GuildRole';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRole as WireGuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {makeAutoObservable} from 'mobx';

class Guilds {
	guilds: Record<string, Guild> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getGuild(guildId: string): Guild | undefined {
		return this.guilds[guildId];
	}

	getGuildIds(): Array<string> {
		return Object.keys(this.guilds);
	}

	getGuildRoles(guildId: string, includeEveryone = false): Array<GuildRole> {
		const guild = this.guilds[guildId];
		if (!guild) {
			return [];
		}
		return Object.values(guild.roles).filter((role) => includeEveryone || role.id !== guildId);
	}

	getGuildRole(guildId: string, roleId: string): GuildRole | undefined {
		return this.guilds[guildId]?.roles[roleId];
	}

	getGuilds(): Array<Guild> {
		return Object.values(this.guilds);
	}

	getOwnedGuilds(userId: string): Array<Guild> {
		return Object.values(this.guilds).filter((guild) => guild.ownerId === userId);
	}

	handleGatewayReady({guilds}: {guilds: Array<GuildReadyData>}): void {
		const availableGuilds = guilds.filter((guild) => !guild.unavailable);
		if (availableGuilds.length === 0) {
			this.guilds = {};
			return;
		}
		this.guilds = availableGuilds.reduce<Record<string, Guild>>((acc, guildData) => {
			acc[guildData.id] = Guild.fromGuildReadyData(guildData);
			return acc;
		}, {});
	}

	handleGuildCreate(guild: GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}
		this.guilds[guild.id] = Guild.fromGuildReadyData(guild);
	}

	handleGuildUpdate(guild: WireGuild): void {
		const existingGuild = this.guilds[guild.id];
		if (!existingGuild) {
			return;
		}
		this.guilds[guild.id] = new Guild({
			...guild,
			roles: existingGuild.roles,
		});
	}

	handleGuildDelete({guildId, unavailable}: {guildId: string; unavailable?: boolean}): void {
		delete this.guilds[guildId];
		if (!unavailable) {
			const history = RouterUtils.getHistory();
			const currentPath = history?.location.pathname ?? '';
			const currentGuildId = currentPath.split('/')[2];
			if (Routes.isGuildChannelRoute(currentPath) && currentGuildId === guildId) {
				RouterUtils.transitionTo(Routes.ME);
			}
		}
	}

	private updateGuildWithRoles(
		guildId: string,
		roleUpdater: (roles: Record<string, GuildRole>) => Record<string, GuildRole>,
	): void {
		const guild = this.guilds[guildId];
		if (!guild) {
			return;
		}
		const updatedRoles = roleUpdater({...guild.roles});
		this.guilds[guildId] = new Guild({
			...guild.toJSON(),
			roles: updatedRoles,
		});
	}

	handleGuildRoleCreate({guildId, role}: {guildId: string; role: WireGuildRole}): void {
		this.updateGuildWithRoles(guildId, (roles) => ({
			...roles,
			[role.id]: new GuildRole(guildId, role),
		}));
	}

	handleGuildRoleDelete({guildId, roleId}: {guildId: string; roleId: string}): void {
		this.updateGuildWithRoles(guildId, (roles) =>
			Object.fromEntries(Object.entries(roles).filter(([id]) => id !== roleId)),
		);
	}

	handleGuildRoleUpdate({guildId, role}: {guildId: string; role: WireGuildRole}): void {
		this.updateGuildWithRoles(guildId, (roles) => ({
			...roles,
			[role.id]: new GuildRole(guildId, role),
		}));
	}

	handleGuildRoleUpdateBulk({guildId, roles}: {guildId: string; roles: Array<WireGuildRole>}): void {
		this.updateGuildWithRoles(guildId, (existingRoles) => {
			const updatedRoles = {...existingRoles};
			for (const role of roles) {
				updatedRoles[role.id] = new GuildRole(guildId, role);
			}
			return updatedRoles;
		});
	}
}

export default new Guilds();
