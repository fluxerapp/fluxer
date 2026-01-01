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

import {ChannelTypes, Permissions} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import * as CodeLinkUtils from '~/utils/CodeLinkUtils';

const INVITE_CONFIG: CodeLinkUtils.CodeLinkConfig = {
	get shortHost() {
		return RuntimeConfigStore.inviteHost;
	},
	path: 'invite',
};

export function findInvites(content: string | null): Array<string> {
	return CodeLinkUtils.findCodes(content, INVITE_CONFIG);
}

export function findInvite(content: string | null): string | null {
	return CodeLinkUtils.findCode(content, INVITE_CONFIG);
}

const INVITABLE_CHANNEL_TYPES: Set<number> = new Set([ChannelTypes.GUILD_TEXT, ChannelTypes.GUILD_VOICE]);

export function getFirstInvitableChannel(guildId: string): string | undefined {
	const channels = ChannelStore.getGuildChannels(guildId);
	const invitableChannel = channels.find((channel) => INVITABLE_CHANNEL_TYPES.has(channel.type));
	return invitableChannel?.id;
}

export function getInvitableChannelId(guildId: string): string | undefined {
	const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(guildId);
	if (selectedChannelId) {
		const selectedChannel = ChannelStore.getChannel(selectedChannelId);
		if (selectedChannel && INVITABLE_CHANNEL_TYPES.has(selectedChannel.type)) {
			return selectedChannelId;
		}
	}
	return getFirstInvitableChannel(guildId);
}

export function isChannelVisibleToEveryone(channel: ChannelRecord, guild: GuildRecord): boolean {
	const everyoneOverwrite = channel.permissionOverwrites[guild.id];
	if (!everyoneOverwrite) {
		return true;
	}
	return (everyoneOverwrite.deny & Permissions.VIEW_CHANNEL) === 0n;
}

export interface InviteCapability {
	canInvite: boolean;
	useVanityUrl: boolean;
	vanityUrlCode: string | null;
}

export function getInviteCapability(channelId: string | undefined, guildId: string | undefined): InviteCapability {
	if (!channelId || !guildId) {
		return {canInvite: false, useVanityUrl: false, vanityUrlCode: null};
	}

	const canCreateInvite = PermissionStore.can(Permissions.CREATE_INSTANT_INVITE, {channelId, guildId});
	if (canCreateInvite) {
		return {canInvite: true, useVanityUrl: false, vanityUrlCode: null};
	}

	const guild = GuildStore.getGuild(guildId);
	const channel = ChannelStore.getChannel(channelId);
	if (!guild || !channel || !guild.vanityURLCode) {
		return {canInvite: false, useVanityUrl: false, vanityUrlCode: null};
	}

	if (isChannelVisibleToEveryone(channel, guild)) {
		return {canInvite: true, useVanityUrl: true, vanityUrlCode: guild.vanityURLCode};
	}

	return {canInvite: false, useVanityUrl: false, vanityUrlCode: null};
}

export function canInviteToChannel(channelId: string | undefined, guildId: string | undefined): boolean {
	return getInviteCapability(channelId, guildId).canInvite;
}

export function getVanityInviteUrl(vanityUrlCode: string): string {
	return `${RuntimeConfigStore.inviteEndpoint}/${vanityUrlCode}`;
}
