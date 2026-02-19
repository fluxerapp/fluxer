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

import type {ChannelID, GuildID, RoleID, UserID} from '@fluxer/api/src/BrandedTypes';
import type {GatewayDispatchEvent} from '@fluxer/api/src/constants/Gateway';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

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

export abstract class IGatewayService {
	abstract dispatchGuild(params: {guildId: GuildID; event: GatewayDispatchEvent; data: unknown}): Promise<void>;

	abstract getGuildCounts(guildId: GuildID): Promise<{memberCount: number; presenceCount: number}>;

	abstract getChannelCount(params: {guildId: GuildID}): Promise<number>;

	abstract startGuild(guildId: GuildID): Promise<void>;

	abstract stopGuild(guildId: GuildID): Promise<void>;

	abstract reloadGuild(guildId: GuildID): Promise<void>;

	abstract reloadAllGuilds(guildIds: Array<GuildID>): Promise<{count: number}>;

	abstract shutdownGuild(guildId: GuildID): Promise<void>;

	abstract getGuildMemoryStats(limit: number): Promise<{
		guilds: Array<{
			guild_id: string | null;
			guild_name: string;
			guild_icon: string | null;
			memory: string;
			member_count: number;
			session_count: number;
			presence_count: number;
		}>;
	}>;

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

	abstract getGuildMember(params: {
		guildId: GuildID;
		userId: UserID;
	}): Promise<{success: boolean; memberData?: GuildMemberResponse}>;

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

	abstract joinGuild(params: {userId: UserID; guildId: GuildID}): Promise<void>;

	abstract leaveGuild(params: {userId: UserID; guildId: GuildID}): Promise<void>;

	abstract terminateSession(params: {userId: UserID; sessionIdHashes: Array<string>}): Promise<void>;

	abstract terminateAllSessionsForUser(params: {userId: UserID}): Promise<void>;

	abstract updateMemberVoice(params: {
		guildId: GuildID;
		userId: UserID;
		mute: boolean;
		deaf: boolean;
	}): Promise<{success: boolean}>;

	abstract disconnectVoiceUser(params: {guildId: GuildID; userId: UserID; connectionId: string}): Promise<void>;

	abstract disconnectVoiceUserIfInChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId?: string;
	}): Promise<{success: boolean; ignored?: boolean}>;

	abstract disconnectAllVoiceUsersInChannel(params: {
		guildId: GuildID;
		channelId: ChannelID;
	}): Promise<{success: boolean; disconnectedCount: number}>;

	abstract confirmVoiceConnection(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		connectionId: string;
		tokenNonce: string;
	}): Promise<{success: boolean; error?: string}>;

	abstract getVoiceStatesForChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{voiceStates: Array<{connectionId: string; userId: string; channelId: string}>}>;

	abstract getPendingJoinsForChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{pendingJoins: Array<{connectionId: string; userId: string; tokenNonce: string; expiresAt: number}>}>;

	abstract getVoiceState(params: {guildId: GuildID; userId: UserID}): Promise<{channel_id: string | null} | null>;

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

	abstract getDiscoveryGuildCounts(
		guildIds: Array<GuildID>,
	): Promise<Map<GuildID, {memberCount: number; onlineCount: number}>>;

	abstract getNodeStats(): Promise<{
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
	}>;
}
