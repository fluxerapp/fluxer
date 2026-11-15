// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {
	getEffectiveChannelContentWarning,
	getEffectiveChannelMatureContent,
} from '@app/features/messaging/utils/ContentWarningUtils';
import Permission from '@app/features/permissions/state/Permission';
import {CHANNEL_FOLLOW_TARGET_TYPES, ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';

export const FOLLOW_DESCRIPTOR = msg({
	message: 'Follow',
	comment:
		'Button label that follows an announcement channel, so its published messages are copied into a channel the user picks.',
});
export const FOLLOW_CHANNEL_DESCRIPTOR = msg({
	message: 'Follow channel',
	comment:
		'Context menu action on an announcement channel. It opens a dialog to pick a channel that will receive its published messages.',
});

export type ChannelFollowSourceRestriction = 'age_restricted' | 'content_warning' | null;

export interface ChannelFollowTarget {
	channel: Channel;
	categoryName: string | null;
}

const FOLLOW_TARGET_CHANNEL_PERMISSIONS = Permissions.MANAGE_WEBHOOKS | Permissions.VIEW_CHANNEL;

function guildOf(channel: Channel): Guild | null {
	return channel.guildId ? (Guilds.getGuild(channel.guildId) ?? null) : null;
}

function hasContentWarning(channel: Channel): boolean {
	return getEffectiveChannelContentWarning(channel, guildOf(channel)).level === ContentWarningLevel.CONTENT_WARNING;
}

function isAgeRestricted(channel: Channel): boolean {
	return getEffectiveChannelMatureContent(channel, guildOf(channel));
}

export function getChannelFollowSourceRestriction(source: Channel): ChannelFollowSourceRestriction {
	if (isAgeRestricted(source)) return 'age_restricted';
	if (hasContentWarning(source)) return 'content_warning';
	return null;
}

function isAllowedFollowTarget(restriction: ChannelFollowSourceRestriction, target: Channel): boolean {
	if (restriction === 'age_restricted') return isAgeRestricted(target);
	if (restriction === 'content_warning') return isAgeRestricted(target) || hasContentWarning(target);
	return true;
}

export function getFollowTargetGuilds(restriction: ChannelFollowSourceRestriction): Array<Guild> {
	return Guilds.getGuilds()
		.filter((guild) => getFollowTargetChannels(guild.id, restriction).length > 0)
		.sort((a, b) => a.name.localeCompare(b.name, undefined, {numeric: true, sensitivity: 'base'}));
}

export function canFollowAnnouncementChannel(channel: Channel): boolean {
	if (channel.type !== ChannelTypes.GUILD_ANNOUNCEMENT) return false;
	return Guilds.getGuilds().some((guild) => Permission.can(Permissions.MANAGE_WEBHOOKS, {guildId: guild.id}));
}

export function getFollowTargetChannels(
	guildId: string,
	restriction: ChannelFollowSourceRestriction,
): Array<ChannelFollowTarget> {
	const result: Array<ChannelFollowTarget> = [];
	for (const channel of Channels.getGuildChannels(guildId)) {
		if (!CHANNEL_FOLLOW_TARGET_TYPES.has(channel.type)) continue;
		if (!Permission.can(FOLLOW_TARGET_CHANNEL_PERMISSIONS, channel)) continue;
		if (!isAllowedFollowTarget(restriction, channel)) continue;
		const category = channel.parentId ? Channels.getChannel(channel.parentId) : null;
		result.push({channel, categoryName: category?.name ?? null});
	}
	return result.sort((a, b) => {
		if (a.categoryName === b.categoryName) return (a.channel.position ?? 0) - (b.channel.position ?? 0);
		if (!a.categoryName) return -1;
		if (!b.categoryName) return 1;
		return a.categoryName.localeCompare(b.categoryName);
	});
}

export function getFollowerWebhookTargetChannels(
	guildId: string,
	sourceChannelId: string | null,
	currentChannelId: string,
): Array<Channel> {
	const source = sourceChannelId ? Channels.getChannel(sourceChannelId) : null;
	const restriction = source ? getChannelFollowSourceRestriction(source) : null;
	const targets = getFollowTargetChannels(guildId, restriction).map((target) => target.channel);
	if (!targets.some((channel) => channel.id === currentChannelId)) {
		const current = Channels.getChannel(currentChannelId);
		if (current) targets.unshift(current);
	}
	return targets;
}
