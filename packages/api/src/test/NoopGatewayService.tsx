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

import {type ChannelID, type GuildID, guildIdToRoleId, type RoleID, type UserID} from '@fluxer/api/src/BrandedTypes';
import type {GatewayDispatchEvent} from '@fluxer/api/src/constants/Gateway';
import {mapGuildToGuildResponse} from '@fluxer/api/src/guild/GuildModel';
import {GuildMemberRepository} from '@fluxer/api/src/guild/repositories/GuildMemberRepository';
import {GuildRepository} from '@fluxer/api/src/guild/repositories/GuildRepository';
import {GuildRoleRepository} from '@fluxer/api/src/guild/repositories/GuildRoleRepository';
import {type CallData, IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import {ALL_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

const guildOwners = new Map<string, UserID>();
const guildMembers = new Map<string, Set<UserID>>();
const guildRepository = new GuildRepository();
const guildMemberRepository = new GuildMemberRepository();
const roleRepository = new GuildRoleRepository();

function createDummyGuildResponse(params: {guildId: GuildID; userId: UserID}): GuildResponse {
	const ownerId = guildOwners.get(params.guildId.toString()) ?? params.userId;

	return {
		id: params.guildId.toString(),
		name: 'Test Guild',
		icon: null,
		banner: null,
		banner_width: null,
		banner_height: null,
		splash: null,
		splash_width: null,
		splash_height: null,
		splash_card_alignment: 0,
		embed_splash: null,
		embed_splash_width: null,
		embed_splash_height: null,
		vanity_url_code: null,
		owner_id: ownerId.toString(),
		system_channel_id: null,
		system_channel_flags: 0,
		rules_channel_id: null,
		afk_channel_id: null,
		afk_timeout: 60,
		features: [],
		verification_level: 0,
		mfa_level: 0,
		nsfw_level: 0,
		explicit_content_filter: 0,
		default_message_notifications: 0,
		disabled_operations: 0,
		message_history_cutoff: null,
		permissions: null,
	};
}

export class NoopGatewayService extends IGatewayService {
	constructor() {
		super();
		guildOwners.clear();
		guildMembers.clear();
	}

	setGuildOwner(guildId: GuildID, ownerId: UserID): void {
		guildOwners.set(guildId.toString(), ownerId);
		let members = guildMembers.get(guildId.toString());
		if (!members) {
			members = new Set();
			guildMembers.set(guildId.toString(), members);
		}
		members.add(ownerId);
	}

	addGuildMember(guildId: GuildID, userId: UserID): void {
		let members = guildMembers.get(guildId.toString());
		if (!members) {
			members = new Set();
			guildMembers.set(guildId.toString(), members);
		}
		members.add(userId);
	}

	async dispatchGuild(_params: {guildId: GuildID; event: GatewayDispatchEvent; data: unknown}): Promise<void> {}

	async getGuildCounts(guildId: GuildID): Promise<{memberCount: number; presenceCount: number}> {
		const members = guildMembers.get(guildId.toString());
		return {memberCount: members?.size ?? 0, presenceCount: 0};
	}

	async getChannelCount(_params: {guildId: GuildID}): Promise<number> {
		return 0;
	}

	async startGuild(guildId: GuildID): Promise<void> {
		const guild = await guildRepository.findUnique(guildId);
		if (guild) {
			this.setGuildOwner(guildId, guild.ownerId);
		}
	}

	async stopGuild(_guildId: GuildID): Promise<void> {}

	async reloadGuild(_guildId: GuildID): Promise<void> {}

	async reloadAllGuilds(_guildIds: Array<GuildID>): Promise<{count: number}> {
		return {count: 0};
	}

	async shutdownGuild(_guildId: GuildID): Promise<void> {}

	async getGuildMemoryStats(_limit: number): Promise<{
		guilds: Array<{
			guild_id: string | null;
			guild_name: string;
			guild_icon: string | null;
			memory: string;
			member_count: number;
			session_count: number;
			presence_count: number;
		}>;
	}> {
		return {guilds: []};
	}

	async getUsersToMentionByRoles(_params: {
		guildId: GuildID;
		channelId: ChannelID;
		roleIds: Array<RoleID>;
		authorId: UserID;
	}): Promise<Array<UserID>> {
		return [];
	}

	async getUsersToMentionByUserIds(_params: {
		guildId: GuildID;
		channelId: ChannelID;
		userIds: Array<UserID>;
		authorId: UserID;
	}): Promise<Array<UserID>> {
		return [];
	}

	async getAllUsersToMention(_params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
	}): Promise<Array<UserID>> {
		return [];
	}

	async resolveAllMentions(_params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
		mentionEveryone: boolean;
		mentionHere: boolean;
		roleIds: Array<RoleID>;
		userIds: Array<UserID>;
	}): Promise<Array<UserID>> {
		return [];
	}

	async getUserPermissions(params: {guildId: GuildID; userId: UserID; channelId?: ChannelID}): Promise<bigint> {
		const {guildId, userId, channelId} = params;

		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			return 0n;
		}

		if (guild.ownerId === userId) {
			return ALL_PERMISSIONS;
		}

		const member = await guildMemberRepository.getMember(guildId, userId);
		if (!member) {
			return 0n;
		}

		const roles = await roleRepository.listRoles(guildId);
		const guildPermissions = this.calculateGuildPermissions(member.roleIds, roles, guildId);

		if (!channelId) {
			return guildPermissions;
		}

		const {ChannelDataRepository} = await import('@fluxer/api/src/channel/repositories/ChannelDataRepository');
		const channelRepo = new ChannelDataRepository();
		const channel = await channelRepo.findUnique(channelId);
		if (!channel) {
			return guildPermissions;
		}

		return this.applyChannelOverwrites(guildPermissions, member.roleIds, channel, userId, guildId);
	}

	async getUserPermissionsBatch(_params: {
		guildIds: Array<GuildID>;
		userId: UserID;
		channelId?: ChannelID;
	}): Promise<Map<GuildID, bigint>> {
		return new Map();
	}

	async canManageRoles(params: {
		guildId: GuildID;
		userId: UserID;
		targetUserId: UserID;
		roleId: RoleID;
	}): Promise<boolean> {
		const {guildId, userId, roleId} = params;

		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			return false;
		}

		if (guild.ownerId === userId) {
			return true;
		}

		const member = await guildMemberRepository.getMember(guildId, userId);
		if (!member) {
			return false;
		}

		const roles = await roleRepository.listRoles(guildId);
		const userPermissions = this.calculateGuildPermissions(member.roleIds, roles, guildId);

		if ((userPermissions & Permissions.MANAGE_ROLES) === 0n) {
			return false;
		}

		const targetRole = roles.find((r) => r.id === roleId);
		if (!targetRole) {
			return false;
		}

		const userMaxPosition = this.getMaxRolePosition(member.roleIds, roles);
		return userMaxPosition > targetRole.position;
	}

	async canManageRole(params: {guildId: GuildID; userId: UserID; roleId: RoleID}): Promise<boolean> {
		const {guildId, userId, roleId} = params;

		const member = await guildMemberRepository.getMember(guildId, userId);
		if (!member) {
			return false;
		}

		const roles = await roleRepository.listRoles(guildId);
		const targetRole = roles.find((r) => r.id === roleId);
		if (!targetRole) {
			return false;
		}

		const userMaxPosition = this.getMaxRolePosition(member.roleIds, roles);
		if (userMaxPosition > targetRole.position) {
			return true;
		}

		if (userMaxPosition === targetRole.position) {
			const highestRole = this.getHighestRole(member.roleIds, roles);
			if (highestRole) {
				return String(highestRole.id) < String(targetRole.id);
			}
		}

		return false;
	}

	private getHighestRole(
		memberRoleIds: Set<RoleID>,
		allRoles: Array<{id: RoleID; position: number}>,
	): {id: RoleID; position: number} | null {
		let highest: {id: RoleID; position: number} | null = null;
		for (const roleId of memberRoleIds) {
			const role = allRoles.find((r) => r.id === roleId);
			if (!role) continue;
			if (!highest) {
				highest = role;
			} else if (role.position > highest.position) {
				highest = role;
			} else if (role.position === highest.position && String(role.id) < String(highest.id)) {
				highest = role;
			}
		}
		return highest;
	}

	async getAssignableRoles(_params: {guildId: GuildID; userId: UserID}): Promise<Array<RoleID>> {
		return [];
	}

	async getUserMaxRolePosition(params: {guildId: GuildID; userId: UserID}): Promise<number> {
		const {guildId, userId} = params;
		const member = await guildMemberRepository.getMember(guildId, userId);
		if (!member) {
			return 0;
		}
		const roles = await roleRepository.listRoles(guildId);
		return this.getMaxRolePosition(member.roleIds, roles);
	}

	async checkTargetMember(params: {guildId: GuildID; userId: UserID; targetUserId: UserID}): Promise<boolean> {
		const {guildId, userId, targetUserId} = params;

		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			return false;
		}

		if (guild.ownerId === userId) {
			return true;
		}

		if (guild.ownerId === targetUserId) {
			return false;
		}

		const member = await guildMemberRepository.getMember(guildId, userId);
		const targetMember = await guildMemberRepository.getMember(guildId, targetUserId);
		if (!member || !targetMember) {
			return false;
		}

		const roles = await roleRepository.listRoles(guildId);
		const userMaxPosition = this.getMaxRolePosition(member.roleIds, roles);
		const targetMaxPosition = this.getMaxRolePosition(targetMember.roleIds, roles);
		return userMaxPosition > targetMaxPosition;
	}

	async getViewableChannels(params: {guildId: GuildID; userId: UserID}): Promise<Array<ChannelID>> {
		const {guildId, userId} = params;
		const guild = await guildRepository.findUnique(guildId);

		const {ChannelDataRepository} = await import('@fluxer/api/src/channel/repositories/ChannelDataRepository');
		const channelRepo = new ChannelDataRepository();
		const channels = await channelRepo.listGuildChannels(guildId);

		if (guild?.ownerId === userId) {
			return channels.map((ch) => ch.id);
		}

		const member = await guildMemberRepository.getMember(guildId, userId);
		if (!member) {
			return [];
		}

		const roles = await roleRepository.listRoles(guildId);
		const guildPermissions = this.calculateGuildPermissions(member.roleIds, roles, guildId);

		if ((guildPermissions & Permissions.ADMINISTRATOR) !== 0n) {
			return channels.map((ch) => ch.id);
		}

		const viewable: Array<ChannelID> = [];
		for (const channel of channels) {
			const channelPermissions = this.applyChannelOverwrites(
				guildPermissions,
				member.roleIds,
				channel,
				userId,
				guildId,
			);
			if ((channelPermissions & Permissions.VIEW_CHANNEL) !== 0n) {
				viewable.push(channel.id);
			}
		}

		return viewable;
	}

	async getCategoryChannelCount(_params: {guildId: GuildID; categoryId: ChannelID}): Promise<number> {
		return 0;
	}

	async getMembersWithRole(_params: {guildId: GuildID; roleId: RoleID}): Promise<Array<UserID>> {
		return [];
	}

	async getGuildData(params: {
		guildId: GuildID;
		userId: UserID;
		skipMembershipCheck?: boolean;
	}): Promise<GuildResponse> {
		if (!params.skipMembershipCheck) {
			const isMember = await this.hasGuildMember({guildId: params.guildId, userId: params.userId});
			if (!isMember) {
				throw new UnknownGuildError();
			}
		}

		const guild = await guildRepository.findUnique(params.guildId);

		if (guild) {
			const ownerId = guild.ownerId;
			guildOwners.set(params.guildId.toString(), ownerId);
			this.setGuildOwner(params.guildId, ownerId);

			return mapGuildToGuildResponse(guild);
		}

		return createDummyGuildResponse({guildId: params.guildId, userId: params.userId});
	}

	async getGuildMember(params: {
		guildId: GuildID;
		userId: UserID;
	}): Promise<{success: boolean; memberData?: GuildMemberResponse}> {
		const members = guildMembers.get(params.guildId.toString());
		const isMember = members?.has(params.userId) ?? false;

		if (!isMember) {
			return {success: false};
		}

		return {
			success: true,
			memberData: {
				user: {
					id: params.userId.toString(),
					username: 'testuser',
					discriminator: '0000',
					global_name: null,
					avatar: null,
					avatar_color: null,
					bot: false,
					system: false,
					flags: 0,
				},
				nick: null,
				avatar: null,
				banner: null,
				accent_color: null,
				roles: [],
				joined_at: '2024-01-01T00:00:00.000Z',
				deaf: false,
				mute: false,
				communication_disabled_until: null,
				profile_flags: null,
			},
		};
	}

	async hasGuildMember(params: {guildId: GuildID; userId: UserID}): Promise<boolean> {
		const members = guildMembers.get(params.guildId.toString());
		return members?.has(params.userId) ?? false;
	}

	async listGuildMembers(_params: {guildId: GuildID; limit: number; offset: number}): Promise<{
		members: Array<GuildMemberResponse>;
		total: number;
	}> {
		return {members: [], total: 0};
	}

	async listGuildMembersCursor(_params: {guildId: GuildID; limit: number; after?: UserID}): Promise<{
		members: Array<GuildMemberResponse>;
		total: number;
	}> {
		return {members: [], total: 0};
	}

	async checkPermission(params: {
		guildId: GuildID;
		userId: UserID;
		permission: bigint;
		channelId?: ChannelID;
	}): Promise<boolean> {
		const {guildId, userId, permission, channelId} = params;

		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			return false;
		}

		if (guild.ownerId === userId) {
			return true;
		}

		const member = await guildMemberRepository.getMember(guildId, userId);
		if (!member) {
			return false;
		}

		const roles = await roleRepository.listRoles(guildId);
		const guildPermissions = this.calculateGuildPermissions(member.roleIds, roles, guildId);

		if ((guildPermissions & Permissions.ADMINISTRATOR) !== 0n) {
			return true;
		}

		let userPermissions = guildPermissions;

		if (channelId) {
			const {ChannelDataRepository} = await import('@fluxer/api/src/channel/repositories/ChannelDataRepository');
			const channelRepo = new ChannelDataRepository();
			const channel = await channelRepo.findUnique(channelId);

			if (channel) {
				userPermissions = this.applyChannelOverwrites(guildPermissions, member.roleIds, channel, userId, guildId);
			}
		}

		return (userPermissions & permission) === permission;
	}

	private calculateGuildPermissions(
		memberRoleIds: Set<RoleID>,
		allRoles: Array<{id: RoleID; permissions: bigint}>,
		guildId: GuildID,
	): bigint {
		let permissions = 0n;

		const everyoneRoleId = guildIdToRoleId(guildId);
		const everyoneRole = allRoles.find((r) => r.id === everyoneRoleId);
		if (everyoneRole) {
			permissions |= everyoneRole.permissions;
		}

		for (const roleId of memberRoleIds) {
			const role = allRoles.find((r) => r.id === roleId);
			if (role) {
				permissions |= role.permissions;

				if ((permissions & Permissions.ADMINISTRATOR) !== 0n) {
					return ALL_PERMISSIONS;
				}
			}
		}

		return permissions;
	}

	private applyChannelOverwrites(
		basePermissions: bigint,
		memberRoleIds: Set<RoleID>,
		channel: {permissionOverwrites?: Map<RoleID | UserID, {allow: bigint; deny: bigint}>},
		userId: UserID,
		guildId: GuildID,
	): bigint {
		if ((basePermissions & Permissions.ADMINISTRATOR) !== 0n) {
			return ALL_PERMISSIONS;
		}

		if (!channel.permissionOverwrites) {
			return basePermissions;
		}

		let permissions = basePermissions;

		const everyoneRoleId = guildIdToRoleId(guildId);
		const everyoneOverwrite = channel.permissionOverwrites.get(everyoneRoleId);
		if (everyoneOverwrite) {
			permissions = (permissions & ~everyoneOverwrite.deny) | everyoneOverwrite.allow;
		}

		let roleAllow = 0n;
		let roleDeny = 0n;
		for (const roleId of memberRoleIds) {
			const overwrite = channel.permissionOverwrites.get(roleId);
			if (overwrite) {
				roleAllow |= overwrite.allow;
				roleDeny |= overwrite.deny;
			}
		}
		permissions = (permissions & ~roleDeny) | roleAllow;

		const userOverwrite = channel.permissionOverwrites.get(userId);
		if (userOverwrite) {
			permissions = (permissions & ~userOverwrite.deny) | userOverwrite.allow;
		}

		return permissions;
	}

	private getMaxRolePosition(memberRoleIds: Set<RoleID>, allRoles: Array<{id: RoleID; position: number}>): number {
		let maxPosition = -1;
		for (const roleId of memberRoleIds) {
			const role = allRoles.find((r) => r.id === roleId);
			if (role) {
				maxPosition = Math.max(maxPosition, role.position);
			}
		}
		return maxPosition;
	}

	async getVanityUrlChannel(_guildId: GuildID): Promise<ChannelID | null> {
		return null;
	}

	async getFirstViewableTextChannel(_guildId: GuildID): Promise<ChannelID | null> {
		return null;
	}

	async dispatchPresence(_params: {userId: UserID; event: GatewayDispatchEvent; data: unknown}): Promise<void> {}

	async invalidatePushBadgeCount(_params: {userId: UserID}): Promise<void> {}

	async joinGuild(params: {userId: UserID; guildId: GuildID}): Promise<void> {
		this.addGuildMember(params.guildId, params.userId);
	}

	async leaveGuild(params: {userId: UserID; guildId: GuildID}): Promise<void> {
		const members = guildMembers.get(params.guildId.toString());
		if (members) {
			members.delete(params.userId);
		}
	}

	async terminateSession(_params: {userId: UserID; sessionIdHashes: Array<string>}): Promise<void> {}

	async terminateAllSessionsForUser(_params: {userId: UserID}): Promise<void> {}

	async updateMemberVoice(_params: {
		guildId: GuildID;
		userId: UserID;
		mute: boolean;
		deaf: boolean;
	}): Promise<{success: boolean}> {
		return {success: false};
	}

	async disconnectVoiceUser(_params: {guildId: GuildID; userId: UserID; connectionId: string}): Promise<void> {}

	async disconnectVoiceUserIfInChannel(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId?: string;
	}): Promise<{success: boolean; ignored?: boolean}> {
		return {success: false, ignored: true};
	}

	async disconnectAllVoiceUsersInChannel(_params: {
		guildId: GuildID;
		channelId: ChannelID;
	}): Promise<{success: boolean; disconnectedCount: number}> {
		return {success: false, disconnectedCount: 0};
	}

	async confirmVoiceConnection(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
		connectionId: string;
		tokenNonce: string;
	}): Promise<{success: boolean; error?: string}> {
		return {success: false};
	}

	async getVoiceStatesForChannel(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{voiceStates: Array<{connectionId: string; userId: string; channelId: string}>}> {
		return {voiceStates: []};
	}

	async getPendingJoinsForChannel(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{pendingJoins: Array<{connectionId: string; userId: string; tokenNonce: string; expiresAt: number}>}> {
		return {pendingJoins: []};
	}

	async getVoiceState(_params: {guildId: GuildID; userId: UserID}): Promise<{channel_id: string | null} | null> {
		return null;
	}

	async moveMember(_params: {
		guildId: GuildID;
		moderatorId: UserID;
		userId: UserID;
		channelId: ChannelID | null;
		connectionId: string | null;
	}): Promise<{success?: boolean; error?: string}> {
		return {success: false};
	}

	async hasActivePresence(_userId: UserID): Promise<boolean> {
		return false;
	}

	async addTemporaryGuild(_params: {userId: UserID; guildId: GuildID}): Promise<void> {}

	async removeTemporaryGuild(_params: {userId: UserID; guildId: GuildID}): Promise<void> {}

	async syncGroupDmRecipients(_params: {
		userId: UserID;
		recipientsByChannel: Record<string, Array<string>>;
	}): Promise<void> {}

	async switchVoiceRegion(_params: {guildId: GuildID; channelId: ChannelID}): Promise<void> {}

	async getCall(_channelId: ChannelID): Promise<CallData | null> {
		return null;
	}

	async createCall(
		_channelId: ChannelID,
		_messageId: string,
		_region: string,
		_ringing: Array<string>,
		_recipients: Array<string>,
	): Promise<CallData> {
		return {
			channel_id: _channelId.toString(),
			message_id: _messageId,
			region: _region,
			ringing: _ringing,
			recipients: _recipients,
			voice_states: [],
		};
	}

	async updateCallRegion(_channelId: ChannelID, _region: string | null): Promise<boolean> {
		return true;
	}

	async ringCallRecipients(_channelId: ChannelID, _recipients: Array<string>): Promise<boolean> {
		return true;
	}

	async stopRingingCallRecipients(_channelId: ChannelID, _recipients: Array<string>): Promise<boolean> {
		return true;
	}

	async deleteCall(_channelId: ChannelID): Promise<boolean> {
		return true;
	}

	async getDiscoveryOnlineCounts(_guildIds: Array<GuildID>): Promise<Map<GuildID, number>> {
		return new Map();
	}

	async getDiscoveryGuildCounts(
		_guildIds: Array<GuildID>,
	): Promise<Map<GuildID, {memberCount: number; onlineCount: number}>> {
		return new Map();
	}

	async getNodeStats(): Promise<{
		status: string;
		sessions: number;
		guilds: number;
		presences: number;
		calls: number;
		memory: {
			total: string;
			processes: string;
			system: string;
		};
		process_count: number;
		process_limit: number;
		uptime_seconds: number;
	}> {
		return {
			status: 'ok',
			sessions: 0,
			guilds: 0,
			presences: 0,
			calls: 0,
			memory: {total: '0', processes: '0', system: '0'},
			process_count: 0,
			process_limit: 0,
			uptime_seconds: 0,
		};
	}
}
