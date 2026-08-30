// SPDX-License-Identifier: AGPL-3.0-or-later

import {ALL_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {type ChannelID, type GuildID, guildIdToRoleId, type MessageID, type RoleID, type UserID} from '../BrandedTypes';
import type {GatewayDispatchEvent} from '../constants/Gateway';
import {
	mapGuildEmojiToResponse,
	mapGuildRoleToResponse,
	mapGuildStickerToResponse,
	mapGuildToGuildResponse,
} from '../guild/GuildModel';
import {GuildMemberRepository} from '../guild/repositories/GuildMemberRepository';
import {GuildRepository} from '../guild/repositories/GuildRepository';
import {GuildRoleRepository} from '../guild/repositories/GuildRoleRepository';
import {
	type CallData,
	type GatewayActiveVoiceRooms,
	type GatewayChannelMention,
	type GatewayGuildMemoryStats,
	type GatewayMentionSources,
	type GatewayMentionSourcesPage,
	type GatewayNodeStats,
	type GatewayVoiceStateCounts,
	type GatewayVoiceStateEntry,
	type GuildChannelAuthContext,
	IGatewayService,
} from '../infrastructure/IGatewayService';
import {UserRepository} from '../user/repositories/UserRepository';
import {mapUserToPartialResponse} from '../user/UserMappers';

const guildOwners = new Map<string, UserID>();
const guildMembers = new Map<string, Set<UserID>>();
const guildRepository = new GuildRepository();
const guildMemberRepository = new GuildMemberRepository();
const roleRepository = new GuildRoleRepository();

interface TestVoiceState extends GatewayVoiceStateEntry {}

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
		nsfw: false,
		content_warning_level: 0,
		content_warning_text: null,
		explicit_content_filter: 0,
		default_message_notifications: 0,
		disabled_operations: 0,
		message_history_cutoff: null,
	};
}

export class NoopGatewayService extends IGatewayService {
	private readonly voiceStatesByChannel = new Map<string, Array<TestVoiceState>>();

	constructor() {
		super();
		guildOwners.clear();
		guildMembers.clear();
	}

	private getVoiceStateKey(params: {guildId?: GuildID; channelId: ChannelID}): string {
		return `${params.guildId?.toString() ?? 'dm'}:${params.channelId.toString()}`;
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

	setVoiceStatesForChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		voiceStates: Array<TestVoiceState>;
	}): void {
		this.voiceStatesByChannel.set(this.getVoiceStateKey(params), [...params.voiceStates]);
	}

	async dispatchGuild(_params: {guildId: GuildID; event: GatewayDispatchEvent; data: unknown}): Promise<void> {}

	async getGuildCounts(guildId: GuildID): Promise<{
		memberCount: number;
		presenceCount: number;
	}> {
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

	async reloadAllGuilds(_guildIds: Array<GuildID>): Promise<{
		count: number;
	}> {
		return {count: 0};
	}

	async shutdownGuild(_guildId: GuildID): Promise<void> {}

	async getGuildMemoryStats(_limit: number): Promise<GatewayGuildMemoryStats> {
		return {guilds: []};
	}

	async getVoiceStateCounts(): Promise<GatewayVoiceStateCounts> {
		return {
			total_voice_states: 0,
			regions: [],
			servers: [],
		};
	}

	async getActiveVoiceRooms(): Promise<GatewayActiveVoiceRooms> {
		return {
			nodeCount: 1,
			rooms: Array.from(this.voiceStatesByChannel.entries()).flatMap(([key, voiceStates]) => {
				if (voiceStates.length === 0) {
					return [];
				}
				const [guildIdText, channelIdText] = key.split(':');
				return [
					{
						guildId: guildIdText === 'dm' ? undefined : (BigInt(guildIdText) as GuildID),
						channelId: BigInt(channelIdText) as ChannelID,
						voiceStateCount: voiceStates.length,
					},
				];
			}),
		};
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

	async resolveMentionSources(_params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
		mentionEveryone: boolean;
		mentionHere: boolean;
		roleIds: Array<RoleID>;
		userIds: Array<UserID>;
	}): Promise<GatewayMentionSources> {
		return {
			directUserIds: [],
			roleUserIds: [],
			everyoneUserIds: [],
		};
	}

	async resolveMentionSourcesPage(_params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
		mentionEveryone: boolean;
		mentionHere: boolean;
		roleIds: Array<RoleID>;
		userIds: Array<UserID>;
		limit: number;
		cursor?: string;
	}): Promise<GatewayMentionSourcesPage> {
		return {
			mentions: [],
			nextCursor: null,
		};
	}

	async resolveChannelMentions(params: {
		guildId: GuildID;
		channelIds: Array<ChannelID>;
	}): Promise<Array<GatewayChannelMention>> {
		if (params.channelIds.length === 0) {
			return [];
		}
		const roles = await roleRepository.listRoles(params.guildId);
		const basePermissions = this.calculateGuildPermissions(new Set(), roles, params.guildId);
		const {ChannelDataRepository} = await import('../channel/repositories/ChannelDataRepository');
		const channelRepo = new ChannelDataRepository();
		const channels = await channelRepo.listGuildChannels(params.guildId);
		const channelsById = new Map(channels.map((channel) => [channel.id, channel]));
		const result: Array<GatewayChannelMention> = [];
		const seen = new Set<ChannelID>();
		for (const channelId of params.channelIds) {
			if (seen.has(channelId)) {
				continue;
			}
			seen.add(channelId);
			const channel = channelsById.get(channelId);
			if (channel?.name && this.canEveryoneViewChannel(basePermissions, channel, params.guildId)) {
				result.push({
					id: channel.id.toString(),
					name: channel.name,
					type: channel.type,
				});
			}
		}
		return result;
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
		const {ChannelDataRepository} = await import('../channel/repositories/ChannelDataRepository');
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
		const {guildId, userId, targetUserId, roleId} = params;
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
		const userHighest = this.getHighestRole(member.roleIds, roles);
		if (!userHighest || !this.roleOutranks(userHighest, targetRole)) {
			return false;
		}
		return this.checkTargetMember({guildId, userId, targetUserId});
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
		const userHighest = this.getHighestRole(member.roleIds, roles);
		return this.roleOutranks(userHighest, targetRole);
	}

	private getHighestRole(
		memberRoleIds: Set<RoleID>,
		allRoles: Array<{
			id: RoleID;
			position: number;
		}>,
	): {
		id: RoleID;
		position: number;
	} | null {
		let highest: {
			id: RoleID;
			position: number;
		} | null = null;
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

	private roleOutranks(
		roleA: {
			id: RoleID;
			position: number;
		} | null,
		roleB: {
			id: RoleID;
			position: number;
		} | null,
	): boolean {
		if (!roleA) return false;
		if (!roleB) return true;
		if (roleA.position !== roleB.position) {
			return roleA.position > roleB.position;
		}
		return String(roleA.id) < String(roleB.id);
	}

	async getAssignableRoles(params: {guildId: GuildID; userId: UserID}): Promise<Array<RoleID>> {
		const {guildId, userId} = params;
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			return [];
		}
		const roles = await roleRepository.listRoles(guildId);
		if (guild.ownerId === userId) {
			return roles.map((role) => role.id);
		}
		const member = await guildMemberRepository.getMember(guildId, userId);
		if (!member) {
			return [];
		}
		const userHighest = this.getHighestRole(member.roleIds, roles);
		if (!userHighest) {
			return [];
		}
		return roles.filter((role) => this.roleOutranks(userHighest, role)).map((role) => role.id);
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
		const userHighest = this.getHighestRole(member.roleIds, roles);
		const targetHighest = this.getHighestRole(targetMember.roleIds, roles);
		return this.roleOutranks(userHighest, targetHighest);
	}

	async getViewableChannels(params: {guildId: GuildID; userId: UserID}): Promise<Array<ChannelID>> {
		const {guildId, userId} = params;
		const guild = await guildRepository.findUnique(guildId);
		const {ChannelDataRepository} = await import('../channel/repositories/ChannelDataRepository');
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
			const [roles, emojis, stickers, viewableChannelIds] = await Promise.all([
				roleRepository.listRoles(params.guildId),
				guildRepository.listEmojis(params.guildId),
				guildRepository.listStickers(params.guildId),
				params.skipMembershipCheck
					? Promise.resolve<Array<ChannelID>>([])
					: this.getViewableChannels({guildId: params.guildId, userId: params.userId}),
			]);
			const {ChannelDataRepository} = await import('../channel/repositories/ChannelDataRepository');
			const channelRepo = new ChannelDataRepository();
			const allChannels = await channelRepo.listGuildChannels(params.guildId);
			const viewableChannelIdSet = new Set(
				params.skipMembershipCheck ? allChannels.map((channel) => channel.id) : viewableChannelIds,
			);
			const channels = allChannels
				.filter((channel) => viewableChannelIdSet.has(channel.id))
				.map((channel) => ({
					id: channel.id.toString(),
					guild_id: channel.guildId?.toString(),
					name: channel.name ?? undefined,
					type: channel.type,
					position: channel.position,
					parent_id: channel.parentId?.toString() ?? null,
					nsfw: channel.isNsfw,
				}));
			return {
				...mapGuildToGuildResponse(guild),
				roles: roles.map(mapGuildRoleToResponse),
				emojis: emojis.map(mapGuildEmojiToResponse),
				stickers: stickers.map(mapGuildStickerToResponse),
				channels,
				member_count: guildMembers.get(params.guildId.toString())?.size ?? 0,
				online_count: 0,
			};
		}
		return createDummyGuildResponse({guildId: params.guildId, userId: params.userId});
	}

	async getGuildAuthContext(params: {
		guildId: GuildID;
		userId: UserID;
		channelId?: ChannelID;
	}): Promise<GuildChannelAuthContext> {
		const guild = await this.getGuildData({guildId: params.guildId, userId: params.userId});
		const parentId = params.channelId?.toString();
		const parentChannel = parentId ? (guild.channels?.find((channel) => channel.id === parentId) ?? null) : null;
		return {guild, parentChannel};
	}

	async getGuildMember(params: {guildId: GuildID; userId: UserID}): Promise<{
		success: boolean;
		memberData?: GuildMemberResponse;
	}> {
		const member = await guildMemberRepository.getMember(params.guildId, params.userId);
		if (!member) {
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
		const user = await new UserRepository().findUnique(params.userId);
		if (!user) {
			return {success: false};
		}
		const isTimedOut =
			member.communicationDisabledUntil !== null && member.communicationDisabledUntil.getTime() > Date.now();
		return {
			success: true,
			memberData: {
				user: mapUserToPartialResponse(user),
				nick: member.nickname,
				avatar: member.avatarHash,
				banner: member.bannerHash,
				accent_color: member.accentColor,
				roles: Array.from(member.roleIds).map((id) => id.toString()),
				joined_at: member.joinedAt.toISOString(),
				deaf: member.isDeaf,
				mute: isTimedOut ? true : member.isMute,
				communication_disabled_until: member.communicationDisabledUntil?.toISOString() ?? null,
				profile_flags: member.profileFlags || undefined,
				mention_flags: member.mentionFlags || undefined,
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
			const {ChannelDataRepository} = await import('../channel/repositories/ChannelDataRepository');
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
		allRoles: Array<{
			id: RoleID;
			permissions: bigint;
		}>,
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
		channel: {
			permissionOverwrites?: Map<
				RoleID | UserID,
				{
					allow: bigint;
					deny: bigint;
				}
			>;
		},
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

	private canEveryoneViewChannel(
		basePermissions: bigint,
		channel: {
			guildId: GuildID | null;
			permissionOverwrites?: Map<RoleID | UserID, {allow: bigint; deny: bigint}>;
		},
		guildId: GuildID,
	): boolean {
		if ((basePermissions & Permissions.ADMINISTRATOR) !== 0n) {
			return true;
		}
		let permissions = basePermissions;
		const overwrite = channel.permissionOverwrites?.get(guildIdToRoleId(guildId));
		if (overwrite) {
			permissions = (permissions & ~overwrite.deny) | overwrite.allow;
		}
		return (permissions & Permissions.VIEW_CHANNEL) !== 0n;
	}

	private getMaxRolePosition(
		memberRoleIds: Set<RoleID>,
		allRoles: Array<{
			id: RoleID;
			position: number;
		}>,
	): number {
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

	async invalidatePushBadgeCounts(_params: {userIds: Array<UserID>}): Promise<void> {}

	async invalidatePushSubscriptions(_params: {userId: UserID}): Promise<void> {}

	async clearPushChannelNotifications(_params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {}

	async syncPushUserGuildSettings(_params: {userId: UserID; guildId: GuildID; settings: unknown}): Promise<void> {}

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

	async updateMemberVoice(_params: {guildId: GuildID; userId: UserID; mute: boolean; deaf: boolean}): Promise<{
		success: boolean;
	}> {
		return {success: false};
	}

	async disconnectVoiceUser(_params: {guildId: GuildID; userId: UserID; connectionId: string}): Promise<void> {}

	async disconnectVoiceUserIfInChannel(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId?: string;
	}): Promise<{
		success: boolean;
		ignored?: boolean;
	}> {
		return {success: false, ignored: true};
	}

	async disconnectAllVoiceUsersInChannel(_params: {guildId: GuildID; channelId: ChannelID}): Promise<{
		success: boolean;
		disconnectedCount: number;
	}> {
		return {success: false, disconnectedCount: 0};
	}

	async confirmVoiceConnection(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
		connectionId: string;
		tokenNonce?: string;
	}): Promise<{
		success: boolean;
		error?: string;
	}> {
		return {success: false};
	}

	async repairVoiceStateFromCache(_params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId: string;
	}): Promise<{
		success: boolean;
		repaired?: boolean;
		error?: string;
	}> {
		return {success: false};
	}

	async getVoiceStatesForChannel(params: {guildId?: GuildID; channelId: ChannelID}): Promise<{
		voiceStates: Array<GatewayVoiceStateEntry>;
	}> {
		return {
			voiceStates: [...(this.voiceStatesByChannel.get(this.getVoiceStateKey(params)) ?? [])],
		};
	}

	async getPendingJoinsForChannel(_params: {guildId?: GuildID; channelId: ChannelID}): Promise<{
		pendingJoins: Array<{
			connectionId: string;
			userId: string;
			tokenNonce: string;
			expiresAt: number;
		}>;
	}> {
		return {pendingJoins: []};
	}

	async getVoiceState(_params: {guildId: GuildID; userId: UserID}): Promise<{
		channel_id: string | null;
	} | null> {
		return null;
	}

	async moveMember(_params: {
		guildId: GuildID;
		moderatorId: UserID;
		userId: UserID;
		channelId: ChannelID | null;
		connectionId: string | null;
	}): Promise<{
		success?: boolean;
		error?: string;
	}> {
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

	async getDiscoveryGuildCounts(_guildIds: Array<GuildID>): Promise<
		Map<
			GuildID,
			{
				memberCount: number;
				onlineCount: number;
			}
		>
	> {
		return new Map();
	}

	async getNodeStats(): Promise<GatewayNodeStats> {
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
			node_count: 0,
			nodes: [],
		};
	}
}
