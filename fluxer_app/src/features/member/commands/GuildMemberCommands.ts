// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ME} from '@fluxer/constants/src/AppConstants';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

const logger = new Logger('GuildMembers');

type MemberUpdateParams = Partial<GuildMemberData> & {
	channel_id?: string | null;
	connection_id?: string;
};

interface GuildProfilePatch {
	avatar?: string | null;
	banner?: string | null;
	bio?: string | null;
	pronouns?: string | null;
	accent_color?: number | null;
	nick?: string | null;
	profile_flags?: number | null;
	mention_flags?: number | null;
}

interface TimeoutPatch {
	communication_disabled_until: string | null;
	timeout_reason?: string;
}

type MemberPatch = MemberUpdateParams | GuildProfilePatch | TimeoutPatch;

async function requestMemberPatch(guildId: string, target: string, body: MemberPatch): Promise<GuildMemberData> {
	const response = await http.patch<GuildMemberData>(Endpoints.GUILD_MEMBER(guildId, target), {body});
	const updated = response.body;
	GuildMembers.handleMemberAdd(guildId, updated);
	return updated;
}

async function requestRoleChange(
	action: 'add' | 'remove',
	guildId: string,
	userId: string,
	roleId: string,
): Promise<void> {
	const endpoint = Endpoints.GUILD_MEMBER_ROLE(guildId, userId, roleId);
	if (action === 'add') {
		await http.put(endpoint);
		return;
	}
	await http.delete(endpoint);
}

function timeoutPatch(communicationDisabledUntil: string | null, timeoutReason?: string | null): TimeoutPatch {
	const body: TimeoutPatch = {
		communication_disabled_until: communicationDisabledUntil,
	};
	if (timeoutReason) {
		body.timeout_reason = timeoutReason;
	}
	return body;
}

export async function update(guildId: string, userId: string, params: MemberUpdateParams): Promise<GuildMemberData> {
	try {
		const updated = await requestMemberPatch(guildId, userId, params);
		logger.debug(`Updated member ${userId} in guild ${guildId}`, {connection_id: params['connection_id']});
		return updated;
	} catch (error) {
		logger.error(`Failed to update member ${userId} in guild ${guildId}:`, error);
		throw error;
	}
}

export async function addRole(guildId: string, userId: string, roleId: string): Promise<void> {
	try {
		await requestRoleChange('add', guildId, userId, roleId);
		logger.debug(`Added role ${roleId} to member ${userId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to add role ${roleId} to member ${userId} in guild ${guildId}:`, error);
		throw error;
	}
}

export async function removeRole(guildId: string, userId: string, roleId: string): Promise<void> {
	try {
		await requestRoleChange('remove', guildId, userId, roleId);
		logger.debug(`Removed role ${roleId} from member ${userId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to remove role ${roleId} from member ${userId} in guild ${guildId}:`, error);
		throw error;
	}
}

export async function updateProfile(guildId: string, params: GuildProfilePatch): Promise<GuildMemberData> {
	try {
		const updated = await requestMemberPatch(guildId, ME, params);
		logger.debug(`Updated current user's per-guild profile in guild ${guildId}`);
		return updated;
	} catch (error) {
		logger.error(`Failed to update current user's per-guild profile in guild ${guildId}:`, error);
		throw error;
	}
}

export async function kick(guildId: string, userId: string, reason?: string): Promise<void> {
	try {
		await http.delete(Endpoints.GUILD_MEMBER(guildId, userId), {reason});
		logger.debug(`Kicked member ${userId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to kick member ${userId} from guild ${guildId}:`, error);
		throw error;
	}
}

export async function timeout(
	guildId: string,
	userId: string,
	communicationDisabledUntil: string | null,
	timeoutReason?: string | null,
): Promise<void> {
	try {
		await requestMemberPatch(guildId, userId, timeoutPatch(communicationDisabledUntil, timeoutReason));
		logger.debug(`Updated timeout for member ${userId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update timeout for member ${userId} in guild ${guildId}:`, error);
		throw error;
	}
}
