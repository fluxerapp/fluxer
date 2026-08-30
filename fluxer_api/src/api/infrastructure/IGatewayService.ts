// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {ChannelID, GuildID, MessageID, RoleID, UserID} from '../BrandedTypes';
import type {GatewayDispatchEvent} from '../constants/Gateway';

interface VoiceState {
	user_id: string;
	session_id: string;
	self_mute: boolean;
	self_deaf: boolean;
	self_video: boolean;
	viewer_stream_keys?: Array<string>;
}

export interface CallData {
	channel_id: string;
	message_id: string;
	region: string;
	ringing: Array<string>;
	recipients: Array<string>;
	voice_states: Array<VoiceState>;
}

export interface GatewayGuildMemoryStatsEntry {
	node_id: string;
	guild_id: string | null;
	guild_name: string;
	guild_icon: string | null;
	memory: string;
	member_count: number;
	session_count: number;
	presence_count: number;
}

export interface GatewayGuildMemoryStats {
	guilds: Array<GatewayGuildMemoryStatsEntry>;
}

export interface GatewayNodeMemoryStats {
	total: string;
	processes: string;
	system: string;
}

export interface GatewayNodeStatsEntry {
	node_id: string;
	status: string;
	sessions: number;
	guilds: number;
	presences: number;
	calls: number;
	memory: GatewayNodeMemoryStats;
	process_count: number;
	process_limit: number;
	uptime_seconds: number;
}

export interface GatewayNodeStats {
	status: string;
	sessions: number;
	guilds: number;
	presences: number;
	calls: number;
	memory: GatewayNodeMemoryStats;
	process_count: number;
	process_limit: number;
	uptime_seconds: number;
	node_count: number;
	nodes: Array<GatewayNodeStatsEntry>;
}

export interface GatewayVoiceStateRegionCount {
	region_id: string;
	voice_state_count: number;
}

export interface GatewayVoiceStateServerCount {
	server_id: string;
	voice_state_count: number;
}

export interface GatewayVoiceStateCounts {
	total_voice_states: number;
	regions: Array<GatewayVoiceStateRegionCount>;
	servers: Array<GatewayVoiceStateServerCount>;
}

export interface GatewayActiveVoiceRoom {
	guildId?: GuildID;
	channelId: ChannelID;
	voiceStateCount: number;
}

export interface GatewayActiveVoiceRooms {
	rooms: Array<GatewayActiveVoiceRoom>;
	nodeCount: number;
}

export interface GatewayVoiceStateEntry {
	connectionId: string;
	userId: string;
	channelId: string;
	regionId?: string;
	serverId?: string;
}

export interface GuildChannelAuthContext {
	guild: GuildResponse;
	parentChannel: ChannelResponse | null;
}

export interface GatewayChannelMention {
	id: string;
	name: string;
	type: number;
}

export interface GatewayMentionSources {
	directUserIds: Array<UserID>;
	roleUserIds: Array<UserID>;
	everyoneUserIds: Array<UserID>;
}

export interface GatewayMentionSourceEntry {
	userId: UserID;
	direct: boolean;
	role: boolean;
	everyone: boolean;
}

export interface GatewayMentionSourcesPage {
	mentions: Array<GatewayMentionSourceEntry>;
	nextCursor: string | null;
}

export abstract class IGatewayService {
	abstract dispatchGuild(params: {guildId: GuildID; event: GatewayDispatchEvent; data: unknown}): Promise<void>;

	abstract getGuildCounts(guildId: GuildID): Promise<{
		memberCount: number;
		presenceCount: number;
	}>;

	abstract getChannelCount(params: {guildId: GuildID}): Promise<number>;

	abstract startGuild(guildId: GuildID): Promise<void>;

	abstract stopGuild(guildId: GuildID): Promise<void>;

	abstract reloadGuild(guildId: GuildID): Promise<void>;

	abstract reloadAllGuilds(guildIds: Array<GuildID>): Promise<{
		count: number;
	}>;

	abstract shutdownGuild(guildId: GuildID): Promise<void>;

	abstract getGuildMemoryStats(limit: number): Promise<GatewayGuildMemoryStats>;

	abstract getVoiceStateCounts(): Promise<GatewayVoiceStateCounts>;

	abstract getActiveVoiceRooms(): Promise<GatewayActiveVoiceRooms>;

	abstract getUsersToMentionByRoles(params: {
		guildId: GuildID;
		channelId: ChannelID;
		roleIds: Array<RoleID>;
		authorId: UserID;
	}): Promise<Array<UserID>>;

	abstract getUsersToMentionByUserIds(params: {
		guildId: GuildID;
		channelId: ChannelID;
		userIds: Array<UserID>;
		authorId: UserID;
	}): Promise<Array<UserID>>;

	abstract getAllUsersToMention(params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
	}): Promise<Array<UserID>>;

	abstract resolveAllMentions(params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
		mentionEveryone: boolean;
		mentionHere: boolean;
		roleIds: Array<RoleID>;
		userIds: Array<UserID>;
	}): Promise<Array<UserID>>;

	abstract resolveMentionSources(params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
		mentionEveryone: boolean;
		mentionHere: boolean;
		roleIds: Array<RoleID>;
		userIds: Array<UserID>;
	}): Promise<GatewayMentionSources>;

	abstract resolveMentionSourcesPage(params: {
		guildId: GuildID;
		channelId: ChannelID;
		authorId: UserID;
		mentionEveryone: boolean;
		mentionHere: boolean;
		roleIds: Array<RoleID>;
		userIds: Array<UserID>;
		limit: number;
		cursor?: string;
	}): Promise<GatewayMentionSourcesPage>;

	abstract resolveChannelMentions(params: {
		guildId: GuildID;
		channelIds: Array<ChannelID>;
	}): Promise<Array<GatewayChannelMention>>;

	abstract getUserPermissions(params: {guildId: GuildID; userId: UserID; channelId?: ChannelID}): Promise<bigint>;

	abstract getUserPermissionsBatch(params: {
		guildIds: Array<GuildID>;
		userId: UserID;
		channelId?: ChannelID;
	}): Promise<Map<GuildID, bigint>>;

	abstract canManageRoles(params: {
		guildId: GuildID;
		userId: UserID;
		targetUserId: UserID;
		roleId: RoleID;
	}): Promise<boolean>;

	abstract canManageRole(params: {guildId: GuildID; userId: UserID; roleId: RoleID}): Promise<boolean>;

	abstract getAssignableRoles(params: {guildId: GuildID; userId: UserID}): Promise<Array<RoleID>>;

	abstract getUserMaxRolePosition(params: {guildId: GuildID; userId: UserID}): Promise<number>;

	abstract checkTargetMember(params: {guildId: GuildID; userId: UserID; targetUserId: UserID}): Promise<boolean>;

	abstract getViewableChannels(params: {guildId: GuildID; userId: UserID}): Promise<Array<ChannelID>>;

	abstract getCategoryChannelCount(params: {guildId: GuildID; categoryId: ChannelID}): Promise<number>;

	abstract getMembersWithRole(params: {guildId: GuildID; roleId: RoleID}): Promise<Array<UserID>>;

	abstract getGuildData(params: {
		guildId: GuildID;
		userId: UserID;
		skipMembershipCheck?: boolean;
	}): Promise<GuildResponse>;

	abstract getGuildAuthContext(params: {
		guildId: GuildID;
		userId: UserID;
		channelId?: ChannelID;
	}): Promise<GuildChannelAuthContext>;

	abstract getGuildMember(params: {guildId: GuildID; userId: UserID}): Promise<{
		success: boolean;
		memberData?: GuildMemberResponse;
	}>;

	abstract hasGuildMember(params: {guildId: GuildID; userId: UserID}): Promise<boolean>;

	abstract listGuildMembers(params: {guildId: GuildID; limit: number; offset: number}): Promise<{
		members: Array<GuildMemberResponse>;
		total: number;
	}>;

	abstract listGuildMembersCursor(params: {guildId: GuildID; limit: number; after?: UserID}): Promise<{
		members: Array<GuildMemberResponse>;
		total: number;
	}>;

	abstract checkPermission(params: {
		guildId: GuildID;
		userId: UserID;
		permission: bigint;
		channelId?: ChannelID;
	}): Promise<boolean>;

	abstract getVanityUrlChannel(guildId: GuildID): Promise<ChannelID | null>;

	abstract getFirstViewableTextChannel(guildId: GuildID): Promise<ChannelID | null>;

	abstract dispatchPresence(params: {userId: UserID; event: GatewayDispatchEvent; data: unknown}): Promise<void>;

	abstract invalidatePushBadgeCount(params: {userId: UserID}): Promise<void>;

	abstract invalidatePushBadgeCounts(params: {userIds: Array<UserID>}): Promise<void>;

	abstract invalidatePushSubscriptions(params: {userId: UserID}): Promise<void>;

	abstract clearPushChannelNotifications(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void>;

	abstract syncPushUserGuildSettings(params: {userId: UserID; guildId: GuildID; settings: unknown}): Promise<void>;

	abstract joinGuild(params: {userId: UserID; guildId: GuildID}): Promise<void>;

	abstract leaveGuild(params: {userId: UserID; guildId: GuildID}): Promise<void>;

	abstract terminateSession(params: {userId: UserID; sessionIdHashes: Array<string>}): Promise<void>;

	abstract terminateAllSessionsForUser(params: {userId: UserID}): Promise<void>;

	abstract updateMemberVoice(params: {guildId: GuildID; userId: UserID; mute: boolean; deaf: boolean}): Promise<{
		success: boolean;
	}>;

	abstract disconnectVoiceUser(params: {guildId: GuildID; userId: UserID; connectionId: string}): Promise<void>;

	abstract disconnectVoiceUserIfInChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId?: string;
	}): Promise<{
		success: boolean;
		ignored?: boolean;
	}>;

	abstract disconnectAllVoiceUsersInChannel(params: {guildId: GuildID; channelId: ChannelID}): Promise<{
		success: boolean;
		disconnectedCount: number;
	}>;

	abstract confirmVoiceConnection(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		connectionId: string;
		tokenNonce?: string;
	}): Promise<{
		success: boolean;
		error?: string;
	}>;

	abstract repairVoiceStateFromCache(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId: string;
	}): Promise<{
		success: boolean;
		repaired?: boolean;
		error?: string;
	}>;

	abstract getVoiceStatesForChannel(params: {guildId?: GuildID; channelId: ChannelID}): Promise<{
		voiceStates: Array<GatewayVoiceStateEntry>;
	}>;

	abstract getPendingJoinsForChannel(params: {guildId?: GuildID; channelId: ChannelID}): Promise<{
		pendingJoins: Array<{
			connectionId: string;
			userId: string;
			tokenNonce: string;
			expiresAt: number;
		}>;
	}>;

	abstract getVoiceState(params: {guildId: GuildID; userId: UserID}): Promise<{
		channel_id: string | null;
	} | null>;

	abstract moveMember(params: {
		guildId: GuildID;
		moderatorId: UserID;
		userId: UserID;
		channelId: ChannelID | null;
		connectionId: string | null;
	}): Promise<{
		success?: boolean;
		error?: string;
	}>;

	abstract hasActivePresence(userId: UserID): Promise<boolean>;

	abstract addTemporaryGuild(params: {userId: UserID; guildId: GuildID}): Promise<void>;

	abstract removeTemporaryGuild(params: {userId: UserID; guildId: GuildID}): Promise<void>;

	abstract syncGroupDmRecipients(params: {
		userId: UserID;
		recipientsByChannel: Record<string, Array<string>>;
	}): Promise<void>;

	abstract switchVoiceRegion(params: {guildId: GuildID; channelId: ChannelID}): Promise<void>;

	abstract getCall(channelId: ChannelID): Promise<CallData | null>;

	abstract createCall(
		channelId: ChannelID,
		messageId: string,
		region: string,
		ringing: Array<string>,
		recipients: Array<string>,
	): Promise<CallData>;

	abstract updateCallRegion(channelId: ChannelID, region: string | null): Promise<boolean>;

	abstract ringCallRecipients(channelId: ChannelID, recipients: Array<string>): Promise<boolean>;

	abstract stopRingingCallRecipients(channelId: ChannelID, recipients: Array<string>): Promise<boolean>;

	abstract deleteCall(channelId: ChannelID): Promise<boolean>;

	abstract getDiscoveryOnlineCounts(guildIds: Array<GuildID>): Promise<Map<GuildID, number>>;

	abstract getDiscoveryGuildCounts(guildIds: Array<GuildID>): Promise<
		Map<
			GuildID,
			{
				memberCount: number;
				onlineCount: number;
			}
		>
	>;

	abstract getNodeStats(): Promise<GatewayNodeStats>;
}
