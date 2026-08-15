// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import * as CodeLinkUtils from '@app/features/messaging/utils/CodeLinkUtils';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Permission from '@app/features/permissions/state/Permission';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {compareChannelOrdering} from '@fluxer/schema/src/domains/channel/GuildChannelOrdering';

const OFFICIAL_INVITE_URL_BASES = Object.freeze([
	'https://fluxer.app/invite',
	'https://canary.fluxer.app/invite',
	'https://web.fluxer.app/invite',
	'https://web.canary.fluxer.app/invite',
	'https://fluxer.gg',
	'https://fluxer.gg/invite',
]);
const INVITE_CONFIG: CodeLinkUtils.CodeLinkConfig = {
	path: 'invite',
	get urlBases() {
		if (RuntimeConfig.isSelfHosted()) {
			return [RuntimeConfig.inviteUrlBase];
		}
		return [RuntimeConfig.inviteUrlBase, ...OFFICIAL_INVITE_URL_BASES];
	},
};
const isLayoutTextChannel = (channel: Channel): boolean =>
	channel.type === ChannelTypes.GUILD_TEXT || channel.type === ChannelTypes.GUILD_LINK;
const isLayoutVoiceChannel = (channel: Channel): boolean => channel.type === ChannelTypes.GUILD_VOICE;

function getChannelsInChannelListGroupOrder(channels: ReadonlyArray<Channel>): Array<Channel> {
	return [...channels.filter(isLayoutTextChannel), ...channels.filter(isLayoutVoiceChannel)];
}

export function findInvites(content: string | null): Array<string> {
	return CodeLinkUtils.findCodes(content, INVITE_CONFIG);
}

export function findInvite(content: string | null): string | null {
	return CodeLinkUtils.findCode(content, INVITE_CONFIG);
}

export function findSpoileredInvites(content: string | null): Array<CodeLinkUtils.CodeLinkMatch> {
	return CodeLinkUtils.findSpoileredCodeMatches(content, INVITE_CONFIG);
}

function getChannelsInChannelListOrder(channels: ReadonlyArray<Channel>): Array<Channel> {
	const categories: Array<Channel> = [];
	const rootChannels: Array<Channel> = [];
	const buckets = new Map<string, Array<Channel>>();
	const channelIds = new Set(channels.map((channel) => channel.id));
	const orderedChannels = [...channels].sort(compareChannelOrdering);
	for (const channel of orderedChannels) {
		if (channel.type === ChannelTypes.GUILD_CATEGORY) {
			categories.push(channel);
			continue;
		}
		if (channel.parentId === null) {
			rootChannels.push(channel);
			continue;
		}
		if (!channelIds.has(channel.parentId)) {
			continue;
		}
		const existingBucket = buckets.get(channel.parentId);
		if (existingBucket) {
			existingBucket.push(channel);
		} else {
			buckets.set(channel.parentId, [channel]);
		}
	}
	return [
		...getChannelsInChannelListGroupOrder(rootChannels),
		...categories.flatMap((category) => getChannelsInChannelListGroupOrder(buckets.get(category.id) ?? [])),
	];
}

export function getFirstInvitableChannel(guildId: string): string | undefined {
	const channels = getChannelsInChannelListOrder(Channels.getGuildChannels(guildId));
	const invitableChannel = channels.find(
		(channel) => GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) && canInviteToChannel(channel.id, channel.guildId),
	);
	return invitableChannel?.id;
}

export function getInvitableChannelId(
	guildId: string,
	options: {preferSelectedChannel?: boolean} = {},
): string | undefined {
	const preferSelectedChannel = options.preferSelectedChannel ?? true;
	const selectedChannelId = preferSelectedChannel ? SelectedChannel.selectedChannelIds.get(guildId) : null;
	if (selectedChannelId != null) {
		const selectedChannel = Channels.getChannel(selectedChannelId);
		if (
			selectedChannel &&
			GUILD_TEXT_BASED_CHANNEL_TYPES.has(selectedChannel.type) &&
			canInviteToChannel(selectedChannel.id, selectedChannel.guildId)
		) {
			return selectedChannelId;
		}
	}
	return getFirstInvitableChannel(guildId);
}

export function getDefaultCommunityInviteChannelId(guildId: string): string | undefined {
	return getInvitableChannelId(guildId, {preferSelectedChannel: false});
}

export function isChannelVisibleToEveryone(channel: Channel, guild: Guild): boolean {
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
	const canCreateInvite = Permission.can(Permissions.CREATE_INSTANT_INVITE, {channelId, guildId});
	if (canCreateInvite) {
		return {canInvite: true, useVanityUrl: false, vanityUrlCode: null};
	}
	const guild = Guilds.getGuild(guildId);
	const channel = Channels.getChannel(channelId);
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
	return `${RuntimeConfig.inviteEndpoint}/${vanityUrlCode}`;
}
