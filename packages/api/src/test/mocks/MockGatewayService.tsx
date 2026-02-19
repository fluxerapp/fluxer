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
import type {CallData, IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {vi} from 'vitest';

export interface MockGatewayServiceConfig {
	confirmVoiceConnectionResult?: {success: boolean; error?: string};
	confirmVoiceConnectionThrows?: Error;
	disconnectVoiceUserIfInChannelResult?: {success: boolean; ignored?: boolean};
	disconnectVoiceUserIfInChannelThrows?: Error;
	getVoiceStatesForChannelResult?: Array<{connectionId: string; userId: string; channelId: string}>;
	getPendingJoinsForChannelResult?: Array<{
		connectionId: string;
		userId: string;
		tokenNonce: string;
		expiresAt: number;
	}>;
}

interface GatewayGuildMemoryStat {
	guild_id: string | null;
	guild_name: string;
	guild_icon: string | null;
	memory: string;
	member_count: number;
	session_count: number;
	presence_count: number;
}

export class MockGatewayService implements IGatewayService {
	readonly confirmVoiceConnectionSpy = vi.fn();
	readonly disconnectVoiceUserIfInChannelSpy = vi.fn();
	readonly getVoiceStatesForChannelSpy = vi.fn();
	readonly getPendingJoinsForChannelSpy = vi.fn();
	readonly disconnectVoiceUserSpy = vi.fn();
	readonly moveMemberSpy = vi.fn();
	readonly updateMemberVoiceSpy = vi.fn();

	private config: MockGatewayServiceConfig;

	constructor(config: MockGatewayServiceConfig = {}) {
		this.config = config;
	}

	configure(config: MockGatewayServiceConfig): void {
		this.config = {...this.config, ...config};
	}

	async confirmVoiceConnection(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		connectionId: string;
		tokenNonce: string;
	}): Promise<{success: boolean; error?: string}> {
		this.confirmVoiceConnectionSpy(params);
		if (this.config.confirmVoiceConnectionThrows) {
			throw this.config.confirmVoiceConnectionThrows;
		}
		return this.config.confirmVoiceConnectionResult ?? {success: true};
	}

	async disconnectVoiceUserIfInChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId?: string;
	}): Promise<{success: boolean; ignored?: boolean}> {
		this.disconnectVoiceUserIfInChannelSpy(params);
		if (this.config.disconnectVoiceUserIfInChannelThrows) {
			throw this.config.disconnectVoiceUserIfInChannelThrows;
		}
		return this.config.disconnectVoiceUserIfInChannelResult ?? {success: true};
	}

	async getVoiceStatesForChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{voiceStates: Array<{connectionId: string; userId: string; channelId: string}>}> {
		this.getVoiceStatesForChannelSpy(params);
		return {voiceStates: this.config.getVoiceStatesForChannelResult ?? []};
	}

	async getPendingJoinsForChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{pendingJoins: Array<{connectionId: string; userId: string; tokenNonce: string; expiresAt: number}>}> {
		this.getPendingJoinsForChannelSpy(params);
		return {pendingJoins: this.config.getPendingJoinsForChannelResult ?? []};
	}

	async disconnectVoiceUser(params: {guildId: GuildID; userId: UserID; connectionId: string}): Promise<void> {
		this.disconnectVoiceUserSpy(params);
	}

	async moveMember(params: {
		guildId: GuildID;
		moderatorId: UserID;
		userId: UserID;
		channelId: ChannelID | null;
		connectionId: string | null;
	}): Promise<{success?: boolean; error?: string}> {
		this.moveMemberSpy(params);
		return {success: true};
	}

	async updateMemberVoice(params: {
		guildId: GuildID;
		userId: UserID;
		mute: boolean;
		deaf: boolean;
	}): Promise<{success: boolean}> {
		this.updateMemberVoiceSpy(params);
		return {success: true};
	}

	async dispatchGuild(_params: {guildId: GuildID; event: GatewayDispatchEvent; data: unknown}): Promise<void> {}
	async getGuildCounts(_guildId: GuildID): Promise<{memberCount: number; presenceCount: number}> {
		return {memberCount: 0, presenceCount: 0};
	}
	async getChannelCount(_params: {guildId: GuildID}): Promise<number> {
		return 0;
	}
	async startGuild(_guildId: GuildID): Promise<void> {}
	async stopGuild(_guildId: GuildID): Promise<void> {}
	async reloadGuild(_guildId: GuildID): Promise<void> {}
	async reloadAllGuilds(_guildIds: Array<GuildID>): Promise<{count: number}> {
		return {count: 0};
	}
	async shutdownGuild(_guildId: GuildID): Promise<void> {}
	async getGuildMemoryStats(_limit: number): Promise<{guilds: Array<GatewayGuildMemoryStat>}> {
		return {guilds: []};
	}
	async getUsersToMentionByRoles(_params: unknown): Promise<Array<UserID>> {
		return [];
	}
	async getUsersToMentionByUserIds(_params: unknown): Promise<Array<UserID>> {
		return [];
	}
	async getAllUsersToMention(_params: unknown): Promise<Array<UserID>> {
		return [];
	}
	async resolveAllMentions(_params: unknown): Promise<Array<UserID>> {
		return [];
	}
	async getUserPermissions(_params: unknown): Promise<bigint> {
		return 0n;
	}
	async getUserPermissionsBatch(_params: unknown): Promise<Map<GuildID, bigint>> {
		return new Map();
	}
	async canManageRoles(_params: unknown): Promise<boolean> {
		return false;
	}
	async canManageRole(_params: unknown): Promise<boolean> {
		return false;
	}
	async getAssignableRoles(_params: unknown): Promise<Array<RoleID>> {
		return [];
	}
	async getUserMaxRolePosition(_params: unknown): Promise<number> {
		return 0;
	}
	async checkTargetMember(_params: unknown): Promise<boolean> {
		return false;
	}
	async getViewableChannels(_params: unknown): Promise<Array<ChannelID>> {
		return [];
	}
	async getCategoryChannelCount(_params: unknown): Promise<number> {
		return 0;
	}
	async getMembersWithRole(_params: unknown): Promise<Array<UserID>> {
		return [];
	}
	async getGuildData(_params: unknown): Promise<GuildResponse> {
		throw new Error('Not implemented');
	}
	async getGuildMember(_params: unknown): Promise<{success: boolean; memberData?: GuildMemberResponse}> {
		return {success: false};
	}
	async hasGuildMember(_params: unknown): Promise<boolean> {
		return false;
	}
	async listGuildMembers(_params: unknown): Promise<{members: Array<GuildMemberResponse>; total: number}> {
		return {members: [], total: 0};
	}
	async listGuildMembersCursor(_params: unknown): Promise<{members: Array<GuildMemberResponse>; total: number}> {
		return {members: [], total: 0};
	}
	async checkPermission(_params: unknown): Promise<boolean> {
		return false;
	}
	async getVanityUrlChannel(_guildId: GuildID): Promise<ChannelID | null> {
		return null;
	}
	async getFirstViewableTextChannel(_guildId: GuildID): Promise<ChannelID | null> {
		return null;
	}
	async dispatchPresence(_params: unknown): Promise<void> {}
	async invalidatePushBadgeCount(_params: unknown): Promise<void> {}
	async joinGuild(_params: unknown): Promise<void> {}
	async leaveGuild(_params: unknown): Promise<void> {}
	async terminateSession(_params: unknown): Promise<void> {}
	async terminateAllSessionsForUser(_params: unknown): Promise<void> {}
	async disconnectAllVoiceUsersInChannel(_params: unknown): Promise<{success: boolean; disconnectedCount: number}> {
		return {success: true, disconnectedCount: 0};
	}
	async getVoiceState(_params: unknown): Promise<{channel_id: string | null} | null> {
		return null;
	}
	async hasActivePresence(_userId: UserID): Promise<boolean> {
		return false;
	}
	async addTemporaryGuild(_params: unknown): Promise<void> {}
	async removeTemporaryGuild(_params: unknown): Promise<void> {}
	async syncGroupDmRecipients(_params: unknown): Promise<void> {}
	async switchVoiceRegion(_params: unknown): Promise<void> {}
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
		throw new Error('Not implemented');
	}
	async updateCallRegion(_channelId: ChannelID, _region: string | null): Promise<boolean> {
		return false;
	}
	async ringCallRecipients(_channelId: ChannelID, _recipients: Array<string>): Promise<boolean> {
		return false;
	}
	async stopRingingCallRecipients(_channelId: ChannelID, _recipients: Array<string>): Promise<boolean> {
		return false;
	}
	async deleteCall(_channelId: ChannelID): Promise<boolean> {
		return false;
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
		memory: {total: string; processes: string; system: string};
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

	reset(): void {
		this.confirmVoiceConnectionSpy.mockClear();
		this.disconnectVoiceUserIfInChannelSpy.mockClear();
		this.getVoiceStatesForChannelSpy.mockClear();
		this.getPendingJoinsForChannelSpy.mockClear();
		this.disconnectVoiceUserSpy.mockClear();
		this.moveMemberSpy.mockClear();
		this.updateMemberVoiceSpy.mockClear();
		this.config = {};
	}
}
