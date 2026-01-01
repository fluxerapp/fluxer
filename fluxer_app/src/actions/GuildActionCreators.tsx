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

import type {ChannelMoveOperation} from '~/components/layout/utils/channelMoveOperation';
import type {AuditLogActionType} from '~/constants/AuditLogActionType';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {Guild} from '~/records/GuildRecord';
import type {GuildRole} from '~/records/GuildRoleRecord';
import type {Invite} from '~/records/MessageRecord';
import InviteStore from '~/stores/InviteStore';

const logger = new Logger('Guilds');

import type {UserPartial} from '~/records/UserRecord';

export interface AuditLogChangeEntry {
	key?: string;
	old_value?: unknown;
	new_value?: unknown;
	oldValue?: unknown;
	newValue?: unknown;
}

export type GuildAuditLogChangePayload = Array<AuditLogChangeEntry> | null;

export interface GuildAuditLogEntry {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: Record<string, unknown>;
	changes?: GuildAuditLogChangePayload;
}

export interface GuildAuditLogFetchParams {
	userId?: string;
	actionType?: AuditLogActionType;
	limit?: number;
	beforeLogId?: string;
	afterLogId?: string;
}

interface GuildAuditLogFetchResponse {
	audit_log_entries: Array<GuildAuditLogEntry>;
	users: Array<UserPartial>;
	webhooks: Array<unknown>;
}

export interface GuildBan {
	user: {
		id: string;
		username: string;
		tag: string;
		discriminator: string;
		avatar: string | null;
	};
	reason: string | null;
	moderator_id: string;
	banned_at: string;
	expires_at: string | null;
}

export const create = async (params: Pick<Guild, 'name'> & {icon?: string | null}): Promise<Guild> => {
	try {
		const response = await http.post<Guild>(Endpoints.GUILDS, params);
		const guild = response.body;
		logger.debug(`Created new guild: ${params.name}`);
		return guild;
	} catch (error) {
		logger.error('Failed to create guild:', error);
		throw error;
	}
};

export const update = async (
	guildId: string,
	params: Partial<
		Pick<
			Guild,
			| 'name'
			| 'icon'
			| 'banner'
			| 'splash'
			| 'embed_splash'
			| 'splash_card_alignment'
			| 'afk_channel_id'
			| 'afk_timeout'
			| 'system_channel_id'
			| 'system_channel_flags'
			| 'features'
			| 'default_message_notifications'
			| 'verification_level'
			| 'mfa_level'
			| 'explicit_content_filter'
		>
	>,
): Promise<Guild> => {
	try {
		const response = await http.patch<Guild>(Endpoints.GUILD(guildId), params);
		const guild = response.body;
		logger.debug(`Updated guild ${guildId}`);
		return guild;
	} catch (error) {
		logger.error(`Failed to update guild ${guildId}:`, error);
		throw error;
	}
};

export const moveChannel = async (guildId: string, operation: ChannelMoveOperation): Promise<void> => {
	try {
		await http.patch({
			url: Endpoints.GUILD_CHANNELS(guildId),
			body: [
				{
					id: operation.channelId,
					parent_id: operation.newParentId,
					lock_permissions: false,
					position: operation.position,
				},
			],
			retries: 5,
		});
		logger.debug(`Moved channel ${operation.channelId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to move channel ${operation.channelId} in guild ${guildId}:`, error);
		throw error;
	}
};

export const getVanityURL = async (guildId: string): Promise<{code: string | null; uses: number}> => {
	try {
		const response = await http.get<{code: string | null; uses: number}>(Endpoints.GUILD_VANITY_URL(guildId));
		const result = response.body;
		logger.debug(`Fetched vanity URL for guild ${guildId}`);
		return result;
	} catch (error) {
		logger.error(`Failed to fetch vanity URL for guild ${guildId}:`, error);
		throw error;
	}
};

export const updateVanityURL = async (guildId: string, code: string | null): Promise<string> => {
	try {
		const response = await http.patch<{code: string}>(Endpoints.GUILD_VANITY_URL(guildId), {code});
		logger.debug(`Updated vanity URL for guild ${guildId} to ${code || 'none'}`);
		return response.body.code;
	} catch (error) {
		logger.error(`Failed to update vanity URL for guild ${guildId}:`, error);
		throw error;
	}
};

export const createRole = async (guildId: string, name: string): Promise<void> => {
	try {
		await http.post({url: Endpoints.GUILD_ROLES(guildId), body: {name}});
		logger.debug(`Created role "${name}" in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to create role in guild ${guildId}:`, error);
		throw error;
	}
};

export const updateRole = async (guildId: string, roleId: string, patch: Partial<GuildRole>): Promise<void> => {
	try {
		await http.patch({url: Endpoints.GUILD_ROLE(guildId, roleId), body: patch});
		logger.debug(`Updated role ${roleId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update role ${roleId} in guild ${guildId}:`, error);
		throw error;
	}
};

export const deleteRole = async (guildId: string, roleId: string): Promise<void> => {
	try {
		await http.delete({url: Endpoints.GUILD_ROLE(guildId, roleId)});
		logger.debug(`Deleted role ${roleId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to delete role ${roleId} from guild ${guildId}:`, error);
		throw error;
	}
};

export const setRoleOrder = async (guildId: string, orderedRoleIds: Array<string>): Promise<void> => {
	try {
		const filteredIds = orderedRoleIds.filter((id) => id !== guildId);
		const payload = filteredIds.map((id, index) => ({id, position: filteredIds.length - index}));
		await http.patch({url: Endpoints.GUILD_ROLES(guildId), body: payload, retries: 5});
		logger.debug(`Updated role ordering in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update role ordering in guild ${guildId}:`, error);
		throw error;
	}
};

export const setRoleHoistOrder = async (guildId: string, orderedRoleIds: Array<string>): Promise<void> => {
	try {
		const filteredIds = orderedRoleIds.filter((id) => id !== guildId);
		const payload = filteredIds.map((id, index) => ({id, hoist_position: filteredIds.length - index}));
		await http.patch({url: Endpoints.GUILD_ROLE_HOIST_POSITIONS(guildId), body: payload, retries: 5});
		logger.debug(`Updated role hoist ordering in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update role hoist ordering in guild ${guildId}:`, error);
		throw error;
	}
};

export const resetRoleHoistOrder = async (guildId: string): Promise<void> => {
	try {
		await http.delete({url: Endpoints.GUILD_ROLE_HOIST_POSITIONS(guildId)});
		logger.debug(`Reset role hoist ordering in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to reset role hoist ordering in guild ${guildId}:`, error);
		throw error;
	}
};

export const remove = async (guildId: string): Promise<void> => {
	try {
		await http.post({url: Endpoints.GUILD_DELETE(guildId), body: {}});
		logger.debug(`Deleted guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to delete guild ${guildId}:`, error);
		throw error;
	}
};

export const leave = async (guildId: string): Promise<void> => {
	try {
		await http.delete({url: Endpoints.USER_GUILDS(guildId)});
		logger.debug(`Left guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to leave guild ${guildId}:`, error);
		throw error;
	}
};

export const fetchGuildInvites = async (guildId: string): Promise<Array<Invite>> => {
	try {
		InviteStore.handleGuildInvitesFetchPending(guildId);
		const response = await http.get<Array<Invite>>(Endpoints.GUILD_INVITES(guildId));
		const invites = response.body;
		InviteStore.handleGuildInvitesFetchSuccess(guildId, invites);
		return invites;
	} catch (error) {
		logger.error(`Failed to fetch invites for guild ${guildId}:`, error);
		InviteStore.handleGuildInvitesFetchError(guildId);
		throw error;
	}
};

export const toggleInvitesDisabled = async (guildId: string, disabled: boolean): Promise<Guild> => {
	try {
		const response = await http.patch<Guild>(Endpoints.GUILD(guildId), {
			features: disabled ? ['INVITES_DISABLED'] : [],
		});
		const guild = response.body;
		logger.debug(`${disabled ? 'Disabled' : 'Enabled'} invites for guild ${guildId}`);
		return guild;
	} catch (error) {
		logger.error(`Failed to ${disabled ? 'disable' : 'enable'} invites for guild ${guildId}:`, error);
		throw error;
	}
};

export const toggleTextChannelFlexibleNames = async (guildId: string, enabled: boolean): Promise<Guild> => {
	try {
		const response = await http.patch<Guild>(Endpoints.GUILD_TEXT_CHANNEL_FLEXIBLE_NAMES(guildId), {enabled});
		const guild = response.body;
		logger.debug(`${enabled ? 'Enabled' : 'Disabled'} flexible text channel names for guild ${guildId}`);
		return guild;
	} catch (error) {
		logger.error(
			`Failed to ${enabled ? 'enable' : 'disable'} flexible text channel names for guild ${guildId}:`,
			error,
		);
		throw error;
	}
};

export const toggleDetachedBanner = async (guildId: string, enabled: boolean): Promise<Guild> => {
	try {
		const response = await http.patch<Guild>(Endpoints.GUILD_DETACHED_BANNER(guildId), {enabled});
		const guild = response.body;
		logger.debug(`${enabled ? 'Enabled' : 'Disabled'} detached banner for guild ${guildId}`);
		return guild;
	} catch (error) {
		logger.error(`Failed to ${enabled ? 'enable' : 'disable'} detached banner for guild ${guildId}:`, error);
		throw error;
	}
};

export const toggleDisallowUnclaimedAccounts = async (guildId: string, enabled: boolean): Promise<Guild> => {
	try {
		const response = await http.patch<Guild>(Endpoints.GUILD_DISALLOW_UNCLAIMED_ACCOUNTS(guildId), {enabled});
		const guild = response.body;
		logger.debug(`${enabled ? 'Enabled' : 'Disabled'} disallow unclaimed accounts for guild ${guildId}`);
		return guild;
	} catch (error) {
		logger.error(
			`Failed to ${enabled ? 'enable' : 'disable'} disallow unclaimed accounts for guild ${guildId}:`,
			error,
		);
		throw error;
	}
};

export const transferOwnership = async (guildId: string, newOwnerId: string): Promise<Guild> => {
	try {
		const response = await http.post<Guild>(Endpoints.GUILD_TRANSFER_OWNERSHIP(guildId), {
			new_owner_id: newOwnerId,
		});
		const guild = response.body;
		logger.debug(`Transferred ownership of guild ${guildId} to ${newOwnerId}`);
		return guild;
	} catch (error) {
		logger.error(`Failed to transfer ownership of guild ${guildId}:`, error);
		throw error;
	}
};

export const banMember = async (
	guildId: string,
	userId: string,
	deleteMessageDays?: number,
	reason?: string,
	banDurationSeconds?: number,
): Promise<void> => {
	try {
		await http.put({
			url: Endpoints.GUILD_BAN(guildId, userId),
			body: {
				delete_message_days: deleteMessageDays ?? 0,
				reason: reason ?? null,
				ban_duration_seconds: banDurationSeconds,
			},
		});
		logger.debug(`Banned user ${userId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to ban user ${userId} from guild ${guildId}:`, error);
		throw error;
	}
};

export const unbanMember = async (guildId: string, userId: string): Promise<void> => {
	try {
		await http.delete({url: Endpoints.GUILD_BAN(guildId, userId)});
		logger.debug(`Unbanned user ${userId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to unban user ${userId} from guild ${guildId}:`, error);
		throw error;
	}
};

export const fetchBans = async (guildId: string): Promise<Array<GuildBan>> => {
	try {
		const response = await http.get<Array<GuildBan>>(Endpoints.GUILD_BANS(guildId));
		const bans = response.body;
		logger.debug(`Fetched ${bans.length} bans for guild ${guildId}`);
		return bans;
	} catch (error) {
		logger.error(`Failed to fetch bans for guild ${guildId}:`, error);
		throw error;
	}
};

export const fetchGuildAuditLogs = async (
	guildId: string,
	params: GuildAuditLogFetchParams,
): Promise<GuildAuditLogFetchResponse> => {
	try {
		const query: Record<string, string | number> = {};
		if (params.limit !== undefined) query.limit = params.limit;
		if (params.beforeLogId !== undefined) query.before = params.beforeLogId;
		if (params.afterLogId !== undefined) query.after = params.afterLogId;
		if (params.userId) query.user_id = params.userId;
		if (params.actionType !== undefined) query.action_type = params.actionType;

		const response = await http.get<GuildAuditLogFetchResponse>({
			url: Endpoints.GUILD_AUDIT_LOGS(guildId),
			query,
		});

		const data = response.body;
		logger.debug(`Fetched ${data.audit_log_entries.length} audit log entries for guild ${guildId}`);
		return data;
	} catch (error) {
		logger.error(`Failed to fetch audit logs for guild ${guildId}:`, error);
		throw error;
	}
};
