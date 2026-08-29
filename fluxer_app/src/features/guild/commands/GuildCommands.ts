// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelMoveOperation} from '@app/features/app/components/layout/utils/ChannelMoveOperation';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import Guilds from '@app/features/guild/state/Guilds';
import {createRoleHoistOrderPayload, createRoleOrderPayload} from '@app/features/guild/utils/GuildRoleOrderUtils';
import Invites from '@app/features/invite/state/Invites';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {
	AuditLogWebhookResponse,
	GuildAuditLogEntryResponse,
} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {
	DiscoveryApplicationRequest,
	DiscoveryApplicationResponse,
	DiscoveryStatusResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import type {GuildVanityURLUpdateResponse} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {Guild, GuildVanityURLResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {TemplateSerializedGuild} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const logger = new Logger('GuildCommands');

export interface GuildAuditLogFetchParams {
	userId?: string;
	actionType?: AuditLogActionType;
	limit?: number;
	beforeLogId?: string;
	afterLogId?: string;
}

interface GuildAuditLogFetchResponse {
	audit_log_entries: Array<GuildAuditLogEntryResponse>;
	users: Array<UserPartial>;
	webhooks: Array<AuditLogWebhookResponse>;
}

type GuildCreateParams = Pick<Guild, 'name'> & {
	icon?: string | null;
};

interface GuildTemplateCreateParams {
	name: string;
	icon?: string | null;
	template: TemplateSerializedGuild;
}

interface BanMemberRequest {
	delete_message_seconds: number;
	reason: string | null;
	ban_duration_seconds?: number;
}

export interface GuildBan {
	user: {
		id: string;
		username: string;
		global_name?: string | null;
		tag: string;
		discriminator: string;
		avatar: string | null;
	};
	reason: string | null;
	moderator_id: string;
	banned_at: string;
	expires_at: string | null;
}

async function requestGuildCreate(params: GuildCreateParams | GuildTemplateCreateParams): Promise<Guild> {
	const response = await http.post<Guild>(Endpoints.GUILDS, {body: params});
	return response.body;
}

async function requestGuildUpdate(guildId: string, params: GuildUpdatePayload): Promise<Guild> {
	const response = await http.patch<Guild>(Endpoints.GUILD(guildId), {body: params});
	return response.body;
}

function channelMovePayload(operation: ChannelMoveOperation): Array<Record<string, string | number | boolean | null>> {
	return [
		{
			id: operation.channelId,
			parent_id: operation.newParentId,
			preceding_sibling_id: operation.precedingSiblingId,
			lock_permissions: false,
			position: operation.position,
		},
	];
}

async function requestChannelMove(guildId: string, operation: ChannelMoveOperation): Promise<void> {
	await http.patch(Endpoints.GUILD_CHANNELS(guildId), {
		body: channelMovePayload(operation),
		retries: 5,
	});
}

async function requestVanityURL(guildId: string): Promise<GuildVanityURLResponse> {
	const response = await http.get<GuildVanityURLResponse>(Endpoints.GUILD_VANITY_URL(guildId));
	return response.body;
}

async function requestVanityURLUpdate(guildId: string, code: string | null): Promise<string | null> {
	const response = await http.patch<GuildVanityURLUpdateResponse>(Endpoints.GUILD_VANITY_URL(guildId), {body: {code}});
	return response.body.code ?? null;
}

async function requestRoleOrder(guildId: string, orderedRoleIds: Array<string>): Promise<void> {
	const payload = createRoleOrderPayload({guildId, orderedRoleIds});
	await http.patch(Endpoints.GUILD_ROLES(guildId), {body: payload, retries: 5});
}

async function requestRoleHoistOrder(guildId: string, orderedRoleIds: Array<string>): Promise<void> {
	const payload = createRoleHoistOrderPayload({guildId, orderedRoleIds});
	await http.patch(Endpoints.GUILD_ROLE_HOIST_POSITIONS(guildId), {body: payload, retries: 5});
}

function transferOwnershipRequest(newOwnerId: string): {new_owner_id: string} {
	return {new_owner_id: newOwnerId};
}

function banMemberRequest(
	deleteMessageSeconds?: number,
	reason?: string,
	banDurationSeconds?: number,
): BanMemberRequest {
	return {
		delete_message_seconds: deleteMessageSeconds ?? 0,
		reason: reason ?? null,
		ban_duration_seconds: banDurationSeconds,
	};
}

function auditLogQuery(params: GuildAuditLogFetchParams): Record<string, string | number> {
	const query: Record<string, string | number> = {};
	if (params.limit !== undefined) query.limit = params.limit;
	if (params.beforeLogId !== undefined) query.before = params.beforeLogId;
	if (params.afterLogId !== undefined) query.after = params.afterLogId;
	if (params.userId) query.user_id = params.userId;
	if (params.actionType !== undefined) query.action_type = params.actionType;
	return query;
}

export async function create(params: GuildCreateParams): Promise<Guild> {
	try {
		const guild = await requestGuildCreate(params);
		logger.debug(`Created new guild: ${params['name']}`);
		return guild;
	} catch (error) {
		logger.error('Failed to create guild:', error);
		throw error;
	}
}

export async function createFromTemplate(params: GuildTemplateCreateParams): Promise<Guild> {
	try {
		const guild = await requestGuildCreate(params);
		logger.debug(`Created new guild from template: ${params.name}`);
		return guild;
	} catch (error) {
		logger.error('Failed to create guild from template:', error);
		throw error;
	}
}

export interface GuildUpdatePayload {
	name?: Guild['name'];
	icon?: Guild['icon'];
	banner?: Guild['banner'];
	splash?: Guild['splash'];
	embed_splash?: Guild['embed_splash'];
	splash_card_alignment?: Guild['splash_card_alignment'];
	afk_channel_id?: Guild['afk_channel_id'];
	afk_timeout?: Guild['afk_timeout'];
	system_channel_id?: Guild['system_channel_id'];
	system_channel_flags?: Guild['system_channel_flags'];
	features?: Array<string>;
	default_message_notifications?: Guild['default_message_notifications'];
	message_history_cutoff?: Guild['message_history_cutoff'];
	verification_level?: Guild['verification_level'];
	mfa_level?: Guild['mfa_level'];
	nsfw_level?: Guild['nsfw_level'];
	nsfw?: Guild['nsfw'];
	content_warning_level?: Guild['content_warning_level'];
	content_warning_text?: Guild['content_warning_text'];
	explicit_content_filter?: Guild['explicit_content_filter'];
}

export async function update(guildId: string, params: GuildUpdatePayload): Promise<Guild> {
	try {
		const guild = await requestGuildUpdate(guildId, params);
		logger.debug(`Updated guild ${guildId}`);
		return guild;
	} catch (error) {
		logger.error(`Failed to update guild ${guildId}:`, error);
		throw error;
	}
}

export async function toggleFeature(guildId: string, feature: string, enabled: boolean): Promise<Guild> {
	const guild = Guilds.getGuild(guildId);
	if (!guild) {
		throw new Error(`Cannot toggle feature on unknown guild ${guildId}`);
	}
	const features = new Set(guild.features);
	if (enabled) {
		features.add(feature);
	} else {
		features.delete(feature);
	}
	return await update(guildId, {features: Array.from(features)});
}

export async function moveChannel(guildId: string, operation: ChannelMoveOperation): Promise<void> {
	try {
		await requestChannelMove(guildId, operation);
		logger.debug(`Moved channel ${operation.channelId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to move channel ${operation.channelId} in guild ${guildId}:`, error);
		throw error;
	}
}

export async function getVanityURL(guildId: string): Promise<{
	code: string | null;
	uses: number;
}> {
	try {
		const result = await requestVanityURL(guildId);
		logger.debug(`Fetched vanity URL for guild ${guildId}`);
		return {
			code: result.code ?? null,
			uses: result.uses,
		};
	} catch (error) {
		logger.error(`Failed to fetch vanity URL for guild ${guildId}:`, error);
		throw error;
	}
}

export async function updateVanityURL(guildId: string, code: string | null): Promise<string | null> {
	try {
		const updatedCode = await requestVanityURLUpdate(guildId, code);
		logger.debug(`Updated vanity URL for guild ${guildId} to ${code || 'none'}`);
		return updatedCode;
	} catch (error) {
		logger.error(`Failed to update vanity URL for guild ${guildId}:`, error);
		throw error;
	}
}

export async function createRole(guildId: string, name: string): Promise<void> {
	try {
		await http.post(Endpoints.GUILD_ROLES(guildId), {body: {name}});
		logger.debug(`Created role "${name}" in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to create role in guild ${guildId}:`, error);
		throw error;
	}
}

export async function updateRole(guildId: string, roleId: string, patch: Partial<GuildRole>): Promise<void> {
	try {
		await http.patch(Endpoints.GUILD_ROLE(guildId, roleId), {body: patch});
		logger.debug(`Updated role ${roleId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update role ${roleId} in guild ${guildId}:`, error);
		throw error;
	}
}

export async function deleteRole(guildId: string, roleId: string): Promise<void> {
	try {
		await http.delete(Endpoints.GUILD_ROLE(guildId, roleId));
		logger.debug(`Deleted role ${roleId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to delete role ${roleId} from guild ${guildId}:`, error);
		throw error;
	}
}

export async function setRoleOrder(guildId: string, orderedRoleIds: Array<string>): Promise<void> {
	try {
		await requestRoleOrder(guildId, orderedRoleIds);
		logger.debug(`Updated role ordering in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update role ordering in guild ${guildId}:`, error);
		throw error;
	}
}

export async function setRoleHoistOrder(guildId: string, orderedRoleIds: Array<string>): Promise<void> {
	try {
		await requestRoleHoistOrder(guildId, orderedRoleIds);
		logger.debug(`Updated role hoist ordering in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update role hoist ordering in guild ${guildId}:`, error);
		throw error;
	}
}

export async function resetRoleHoistOrder(guildId: string): Promise<void> {
	try {
		await http.delete(Endpoints.GUILD_ROLE_HOIST_POSITIONS(guildId));
		logger.debug(`Reset role hoist ordering in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to reset role hoist ordering in guild ${guildId}:`, error);
		throw error;
	}
}

export async function remove(guildId: string): Promise<void> {
	try {
		await http.post(Endpoints.GUILD_DELETE(guildId), {body: {}});
		logger.debug(`Deleted guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to delete guild ${guildId}:`, error);
		throw error;
	}
}

export async function leave(guildId: string, options: {deleteMessages?: boolean} = {}): Promise<void> {
	try {
		await http.delete(Endpoints.USER_GUILDS(guildId), {
			query: options.deleteMessages ? {delete_messages: true} : undefined,
		});
		logger.debug(`Left guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to leave guild ${guildId}:`, error);
		throw error;
	}
}

export async function bulkDeleteMyMessages(guildId: string): Promise<void> {
	try {
		await http.post(Endpoints.USER_GUILD_BULK_DELETE_MY_MESSAGES(guildId), {body: {}});
		logger.debug(`Deleted caller's messages in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to delete caller's messages in guild ${guildId}:`, error);
		throw error;
	}
}

export async function fetchGuildInvites(guildId: string): Promise<Array<Invite>> {
	try {
		Invites.handleGuildInvitesFetchPending(guildId);
		const response = await http.get<Array<Invite>>(Endpoints.GUILD_INVITES(guildId));
		const invites = response.body;
		Invites.handleGuildInvitesFetchSuccess(guildId, invites);
		return invites;
	} catch (error) {
		logger.error(`Failed to fetch invites for guild ${guildId}:`, error);
		Invites.handleGuildInvitesFetchError(guildId);
		throw error;
	}
}

export async function transferOwnership(guildId: string, newOwnerId: string): Promise<Guild> {
	try {
		const response = await http.post<Guild>(Endpoints.GUILD_TRANSFER_OWNERSHIP(guildId), {
			body: transferOwnershipRequest(newOwnerId),
		});
		const guild = response.body;
		logger.debug(`Transferred ownership of guild ${guildId} to ${newOwnerId}`);
		return guild;
	} catch (error) {
		logger.error(`Failed to transfer ownership of guild ${guildId}:`, error);
		throw error;
	}
}

export async function banMember(
	guildId: string,
	userId: string,
	deleteMessageSeconds?: number,
	reason?: string,
	banDurationSeconds?: number,
): Promise<void> {
	try {
		await http.put(Endpoints.GUILD_BAN(guildId, userId), {
			body: banMemberRequest(deleteMessageSeconds, reason, banDurationSeconds),
		});
		logger.debug(`Banned user ${userId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to ban user ${userId} from guild ${guildId}:`, error);
		throw error;
	}
}

export async function unbanMember(guildId: string, userId: string): Promise<void> {
	try {
		await http.delete(Endpoints.GUILD_BAN(guildId, userId));
		logger.debug(`Unbanned user ${userId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to unban user ${userId} from guild ${guildId}:`, error);
		throw error;
	}
}

export async function fetchBans(guildId: string): Promise<Array<GuildBan>> {
	try {
		const response = await http.get<Array<GuildBan>>(Endpoints.GUILD_BANS(guildId));
		const bans = response.body;
		logger.debug(`Fetched ${bans.length} bans for guild ${guildId}`);
		return bans;
	} catch (error) {
		logger.error(`Failed to fetch bans for guild ${guildId}:`, error);
		throw error;
	}
}

export async function fetchGuildAuditLogs(
	guildId: string,
	params: GuildAuditLogFetchParams,
): Promise<GuildAuditLogFetchResponse> {
	try {
		const response = await http.get<GuildAuditLogFetchResponse>(Endpoints.GUILD_AUDIT_LOGS(guildId), {
			query: auditLogQuery(params),
		});
		const data = response.body;
		logger.debug(`Fetched ${data.audit_log_entries.length} audit log entries for guild ${guildId}`);
		return data;
	} catch (error) {
		logger.error(`Failed to fetch audit logs for guild ${guildId}:`, error);
		throw error;
	}
}

export async function getDiscoveryStatus(guildId: string): Promise<DiscoveryStatusResponse> {
	try {
		const response = await http.get<DiscoveryStatusResponse>(Endpoints.GUILD_DISCOVERY(guildId));
		logger.debug(`Fetched discovery status for guild ${guildId}`);
		return response.body;
	} catch (error) {
		logger.error(`Failed to fetch discovery status for guild ${guildId}:`, error);
		throw error;
	}
}

export async function applyForDiscovery(
	guildId: string,
	params: DiscoveryApplicationRequest,
): Promise<DiscoveryApplicationResponse> {
	try {
		const response = await http.post<DiscoveryApplicationResponse>(Endpoints.GUILD_DISCOVERY(guildId), {
			body: params,
		});
		logger.debug(`Applied for discovery for guild ${guildId}`);
		return response.body;
	} catch (error) {
		logger.error(`Failed to apply for discovery for guild ${guildId}:`, error);
		throw error;
	}
}

export async function updateDiscoveryApplication(
	guildId: string,
	params: Partial<DiscoveryApplicationRequest>,
): Promise<DiscoveryApplicationResponse> {
	try {
		const response = await http.patch<DiscoveryApplicationResponse>(Endpoints.GUILD_DISCOVERY(guildId), {
			body: params,
		});
		logger.debug(`Updated discovery application for guild ${guildId}`);
		return response.body;
	} catch (error) {
		logger.error(`Failed to update discovery application for guild ${guildId}:`, error);
		throw error;
	}
}

export async function withdrawDiscoveryApplication(guildId: string): Promise<void> {
	try {
		await http.delete(Endpoints.GUILD_DISCOVERY(guildId));
		logger.debug(`Withdrew discovery application for guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to withdraw discovery application for guild ${guildId}:`, error);
		throw error;
	}
}
