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

import {msg} from '@lingui/core/macro';
import {Permissions, StatusTypes} from '~/Constants';
import i18n from '~/i18n';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserRecord} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import PresenceStore from '~/stores/PresenceStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as PermissionUtils from '~/utils/PermissionUtils';

export interface MemberGroup {
	id: string;
	displayName: string;
	count: number;
	members: Array<GuildMemberRecord>;
	position: number;
}

export interface GroupDMMemberGroup {
	id: string;
	displayName: string;
	count: number;
	users: Array<UserRecord>;
}

function getVisibleMembers(guild: GuildRecord, channel: ChannelRecord): Array<GuildMemberRecord> {
	return GuildMemberStore.getMembers(guild.id).filter((member) =>
		PermissionUtils.can(Permissions.VIEW_CHANNEL, member.user, channel.toJSON()),
	);
}

function sortMembers(members: Array<GuildMemberRecord>, guildId: string): Array<GuildMemberRecord> {
	return [...members].sort((a, b) =>
		NicknameUtils.getNickname(a.user, guildId).localeCompare(NicknameUtils.getNickname(b.user, guildId)),
	);
}

function getHighestHoistedRole(member: GuildMemberRecord, guild: GuildRecord) {
	const hoistedRoles = [...member.roles].map((roleId) => guild.getRole(roleId)).filter((role) => role?.hoist);

	if (hoistedRoles.length === 0) return null;

	hoistedRoles.sort((a, b) => {
		const aPos = a!.effectiveHoistPosition;
		const bPos = b!.effectiveHoistPosition;
		if (bPos !== aPos) {
			return bPos - aPos;
		}
		return BigInt(a!.id) < BigInt(b!.id) ? -1 : 1;
	});

	const highestRole = hoistedRoles[0]!;
	return {
		id: highestRole.id,
		name: highestRole.name,
		hoistPosition: highestRole.effectiveHoistPosition,
	};
}

function groupMembersByRole(members: Array<GuildMemberRecord>, guild: GuildRecord): Array<MemberGroup> {
	const roleGroups: Record<string, Array<GuildMemberRecord>> = {};
	const roleHoistPositions: Record<string, number> = {};
	const roleNames: Record<string, string> = {};

	for (const member of members) {
		const highestRole = getHighestHoistedRole(member, guild);
		if (!highestRole) continue;

		if (!roleGroups[highestRole.id]) {
			roleGroups[highestRole.id] = [];
			roleHoistPositions[highestRole.id] = highestRole.hoistPosition;
			roleNames[highestRole.id] = highestRole.name;
		}

		roleGroups[highestRole.id].push(member);
	}

	return Object.keys(roleGroups)
		.map((roleId) => ({
			id: roleId,
			displayName: roleNames[roleId],
			count: roleGroups[roleId].length,
			members: sortMembers(roleGroups[roleId], guild.id),
			position: roleHoistPositions[roleId],
		}))
		.sort((a, b) => {
			if (b.position !== a.position) {
				return b.position - a.position;
			}
			return BigInt(a.id) < BigInt(b.id) ? -1 : 1;
		});
}

function getOnlineWithoutHoistedRole(members: Array<GuildMemberRecord>, guild: GuildRecord): Array<GuildMemberRecord> {
	return sortMembers(
		members.filter((member) => !getHighestHoistedRole(member, guild)),
		guild.id,
	);
}

export function getMemberGroups(guild: GuildRecord, channel: ChannelRecord): Array<MemberGroup> {
	const members = getVisibleMembers(guild, channel);

	const onlineMembers: Array<GuildMemberRecord> = [];
	const offlineMembers: Array<GuildMemberRecord> = [];

	for (const member of members) {
		const status = PresenceStore.getStatus(member.user.id);
		if (status === StatusTypes.OFFLINE || status === StatusTypes.INVISIBLE) {
			offlineMembers.push(member);
			continue;
		}
		onlineMembers.push(member);
	}

	const groupedMembers = groupMembersByRole(onlineMembers, guild);

	return [
		...groupedMembers,
		{
			id: 'online',
			displayName: i18n._(msg`Online`),
			count: onlineMembers.length - groupedMembers.reduce((acc, group) => acc + group.count, 0),
			members: getOnlineWithoutHoistedRole(onlineMembers, guild),
			position: 10000,
		},
		{
			id: 'offline',
			displayName: i18n._(msg`Offline`),
			count: offlineMembers.length,
			members: sortMembers(offlineMembers, guild.id),
			position: 20000,
		},
	].filter((group) => group.count > 0);
}

function sortUsersByDisplayName(users: Array<UserRecord>): Array<UserRecord> {
	return [...users].sort((a, b) => {
		const nameA = a.globalName ?? a.username;
		const nameB = b.globalName ?? b.username;
		return nameA.localeCompare(nameB);
	});
}

export function getGroupDMMemberGroups(users: Array<UserRecord>): Array<GroupDMMemberGroup> {
	const onlineUsers: Array<UserRecord> = [];
	const offlineUsers: Array<UserRecord> = [];

	for (const user of users) {
		const status = PresenceStore.getStatus(user.id);
		if (status === StatusTypes.OFFLINE || status === StatusTypes.INVISIBLE) {
			offlineUsers.push(user);
		} else {
			onlineUsers.push(user);
		}
	}

	return [
		{
			id: 'online',
			displayName: i18n._(msg`Online`),
			count: onlineUsers.length,
			users: sortUsersByDisplayName(onlineUsers),
		},
		{
			id: 'offline',
			displayName: i18n._(msg`Offline`),
			count: offlineUsers.length,
			users: sortUsersByDisplayName(offlineUsers),
		},
	].filter((group) => group.count > 0);
}
