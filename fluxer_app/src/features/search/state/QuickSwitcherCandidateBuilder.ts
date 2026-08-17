// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {
	DIRECT_MESSAGES_DESCRIPTOR,
	FAVORITES_DESCRIPTOR,
	VOICE_CHANNEL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Favorites from '@app/features/messaging/state/Favorites';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Relationships from '@app/features/relationship/state/Relationships';
import type {
	CandidateSets,
	GroupDMCandidate,
	GuildCandidate,
	SettingsCandidate,
	TextChannelCandidate,
	UserCandidate,
	VirtualGuildCandidate,
	VoiceChannelCandidate,
} from '@app/features/search/state/QuickSwitcherTypes';
import {getSettingsSubtabs, getSettingsTabs} from '@app/features/user/components/settings_utils/SettingsConstants';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {DAYS_PER_WEEK, MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const GROUP_MESSAGE_DESCRIPTOR = msg({
	message: 'Group message',
	comment: 'Fallback display name for an unnamed group DM in quick switcher results.',
});
const HOME_DESCRIPTOR = msg({
	message: 'Home',
	comment: 'Action label in quick switcher for navigating to the Home tab.',
});
const UNREAD_SORT_WEIGHT_BOOST = DAYS_PER_WEEK * MS_PER_DAY;

export type ChannelBackedCandidate = UserCandidate | GroupDMCandidate | TextChannelCandidate | VoiceChannelCandidate;

function getChannelRecency(channel: {id: string; lastMessageId: string | null}): number {
	if (channel.lastMessageId) {
		return SnowflakeUtils.extractTimestamp(channel.lastMessageId);
	}
	return SnowflakeUtils.extractTimestamp(channel.id);
}

function getChannelSortWeight(channelId: string, baseWeight: number): number {
	const unreadCount = ReadStates.getUnreadCount(channelId);
	const mentionCount = ReadStates.getMentionCount(channelId);
	const hasUnread = unreadCount > 0 || mentionCount > 0;
	return hasUnread ? baseWeight + UNREAD_SORT_WEIGHT_BOOST : baseWeight;
}

export function buildChannelCandidate(
	channel: Channel,
	i18n: I18n,
	options: {
		guildMap?: ReadonlyMap<string, Guild>;
		currentUserId?: string | null;
	} = {},
): ChannelBackedCandidate | null {
	const currentUserId = options.currentUserId ?? Users.getCurrentUser()?.id ?? null;
	const getGuild = (guildId: string): Guild | null =>
		options.guildMap?.get(guildId) ?? Guilds.getGuild(guildId) ?? null;
	switch (channel.type) {
		case ChannelTypes.DM:
		case ChannelTypes.DM_PERSONAL_NOTES: {
			const recipientId =
				channel.recipientIds.find((recipientId) => recipientId !== currentUserId) ?? channel.recipientIds.at(0);
			if (!recipientId) return null;
			const user = Users.getUser(recipientId);
			if (!user) return null;
			const title = ChannelUtils.getDMDisplayName(channel);
			const subtitle = NicknameUtils.formatUserTagForStreamerMode(user);
			const searchValues = [title, subtitle, user.username, user.id].filter(Boolean);
			const baseWeight = getChannelRecency(channel);
			const sortWeight = getChannelSortWeight(channel.id, baseWeight);
			return {
				type: QuickSwitcherResultTypes.USER,
				id: user.id,
				title,
				subtitle,
				user,
				dmChannelId: channel.id,
				searchValues,
				sortWeight,
			};
		}
		case ChannelTypes.GROUP_DM: {
			const title = ChannelUtils.getDMDisplayName(channel);
			const participantNames = channel.recipientIds
				.map((recipientId) => {
					const user = Users.getUser(recipientId);
					return user ? NicknameUtils.getNickname(user, null) : null;
				})
				.filter(Boolean) as Array<string>;
			const subtitle = participantNames.length > 0 ? participantNames.join(', ') : i18n._(GROUP_MESSAGE_DESCRIPTOR);
			const searchValues = [title, ...participantNames];
			const baseWeight = getChannelRecency(channel);
			const sortWeight = getChannelSortWeight(channel.id, baseWeight);
			return {
				type: QuickSwitcherResultTypes.GROUP_DM,
				id: channel.id,
				title,
				subtitle,
				channel,
				searchValues,
				sortWeight,
			};
		}
		case ChannelTypes.GUILD_TEXT: {
			if (!channel.guildId) return null;
			const guild = getGuild(channel.guildId);
			const title = channel.name ? channel.name : i18n._(UNKNOWN_CHANNEL_DESCRIPTOR);
			const subtitle = guild?.name;
			const searchValues = [channel.name ?? '', channel.topic ?? '', guild?.name ?? '', channel.parentId ?? ''].filter(
				Boolean,
			);
			const baseWeight = getChannelRecency(channel);
			const sortWeight = getChannelSortWeight(channel.id, baseWeight);
			return {
				type: QuickSwitcherResultTypes.TEXT_CHANNEL,
				id: channel.id,
				title,
				subtitle,
				channel,
				guild,
				searchValues,
				sortWeight,
			};
		}
		case ChannelTypes.GUILD_VOICE: {
			if (!channel.guildId) return null;
			const guild = getGuild(channel.guildId);
			const title = channel.name ?? i18n._(VOICE_CHANNEL_DESCRIPTOR);
			const subtitle = guild?.name;
			const searchValues = [channel.name ?? '', guild?.name ?? ''].filter(Boolean);
			const baseWeight = getChannelRecency(channel);
			const sortWeight = getChannelSortWeight(channel.id, baseWeight);
			return {
				type: QuickSwitcherResultTypes.VOICE_CHANNEL,
				id: channel.id,
				title,
				subtitle,
				channel,
				guild,
				searchValues,
				sortWeight,
			};
		}
		default:
			return null;
	}
}

export function buildCandidateSets(i18n: I18n): CandidateSets {
	const guilds = Guilds.getGuilds();
	const guildMap = new Map<string, Guild>(guilds.map((guild) => [guild.id, guild]));
	const userCandidates = new Map<string, UserCandidate>();
	const userByChannelId = new Map<string, UserCandidate>();
	const groupDMCandidates: Array<GroupDMCandidate> = [];
	const groupDMByChannelId = new Map<string, GroupDMCandidate>();
	const textChannelCandidates: Array<TextChannelCandidate> = [];
	const voiceChannelCandidates: Array<VoiceChannelCandidate> = [];
	const channelById = new Map<string, TextChannelCandidate | VoiceChannelCandidate>();
	const currentUserId = Users.getCurrentUser()?.id ?? null;
	const directMessagesDisabled = RuntimeConfig.directMessagesDisabled;
	for (const channel of Channels.allChannels) {
		const candidate = buildChannelCandidate(channel, i18n, {guildMap, currentUserId});
		if (!candidate) {
			continue;
		}
		if (
			directMessagesDisabled &&
			(candidate.type === QuickSwitcherResultTypes.USER || candidate.type === QuickSwitcherResultTypes.GROUP_DM)
		) {
			continue;
		}
		switch (candidate.type) {
			case QuickSwitcherResultTypes.USER: {
				const existing = userCandidates.get(candidate.user.id);
				if (!existing || existing.sortWeight < candidate.sortWeight) {
					userCandidates.set(candidate.user.id, candidate);
				} else if (existing.dmChannelId == null) {
					userCandidates.set(candidate.user.id, {
						...existing,
						dmChannelId: channel.id,
						sortWeight: Math.max(existing.sortWeight, candidate.sortWeight),
					});
				}
				const resolvedCandidate = userCandidates.get(candidate.user.id);
				if (resolvedCandidate) {
					userByChannelId.set(channel.id, resolvedCandidate);
				}
				break;
			}
			case QuickSwitcherResultTypes.GROUP_DM:
				groupDMCandidates.push(candidate);
				groupDMByChannelId.set(channel.id, candidate);
				break;
			case QuickSwitcherResultTypes.TEXT_CHANNEL:
				textChannelCandidates.push(candidate);
				channelById.set(channel.id, candidate);
				break;
			case QuickSwitcherResultTypes.VOICE_CHANNEL:
				voiceChannelCandidates.push(candidate);
				channelById.set(channel.id, candidate);
				break;
			default:
				break;
		}
	}
	if (!directMessagesDisabled) {
		for (const relationship of Relationships.getRelationships()) {
			if (relationship.type !== RelationshipTypes.FRIEND) {
				continue;
			}
			const user = relationship.user;
			if (!userCandidates.has(user.id)) {
				const title = NicknameUtils.getNickname(user, null);
				const subtitle = NicknameUtils.formatUserTagForStreamerMode(user);
				const searchValues = [title, subtitle, user.username, user.id].filter(Boolean);
				userCandidates.set(user.id, {
					type: QuickSwitcherResultTypes.USER,
					id: user.id,
					title,
					subtitle,
					user,
					dmChannelId: null,
					searchValues,
					sortWeight: relationship.since.getTime(),
				});
			}
		}
		for (const user of Users.getUsers()) {
			if (user.id === currentUserId) continue;
			if (!userCandidates.has(user.id)) {
				const title = NicknameUtils.getNickname(user, null);
				const subtitle = NicknameUtils.formatUserTagForStreamerMode(user);
				const searchValues = [title, subtitle, user.username, user.id].filter(Boolean);
				userCandidates.set(user.id, {
					type: QuickSwitcherResultTypes.USER,
					id: user.id,
					title,
					subtitle,
					user,
					dmChannelId: null,
					searchValues,
					sortWeight: SnowflakeUtils.extractTimestamp(user.id),
				});
			}
		}
		const selectedGuildId = SelectedGuild.selectedGuildId;
		if (selectedGuildId) {
			const guildMembers = GuildMembers.getMembers(selectedGuildId);
			for (const member of guildMembers) {
				if (member.user.id === currentUserId) continue;
				if (!userCandidates.has(member.user.id)) {
					const title = member.nick
						? NicknameUtils.formatNicknameForStreamerMode(member.nick)
						: NicknameUtils.getNickname(member.user, member.guildId);
					const subtitle = NicknameUtils.formatUserTagForStreamerMode(member.user);
					const searchValues = [title, subtitle, member.user.username, member.user.id, member.nick].filter(
						Boolean,
					) as Array<string>;
					userCandidates.set(member.user.id, {
						type: QuickSwitcherResultTypes.USER,
						id: member.user.id,
						title,
						subtitle,
						user: member.user,
						dmChannelId: null,
						searchValues,
						sortWeight: member.joinedAt ? new Date(member.joinedAt).getTime() : 0,
					});
				}
			}
		}
	}
	const guildCandidates: Array<GuildCandidate> = guilds.map((guild) => ({
		type: QuickSwitcherResultTypes.GUILD,
		id: guild.id,
		title: guild.name,
		subtitle: undefined,
		guild,
		searchValues: [guild.name, guild.vanityURLCode ?? '', guild.id].filter(Boolean),
		sortWeight: guild.joinedAt ? new Date(guild.joinedAt).getTime() : 0,
	}));
	const virtualGuildCandidates: Array<VirtualGuildCandidate> = [];
	if (!directMessagesDisabled) {
		virtualGuildCandidates.push({
			type: QuickSwitcherResultTypes.VIRTUAL_GUILD,
			id: 'home',
			title: i18n._(HOME_DESCRIPTOR),
			subtitle: i18n._(DIRECT_MESSAGES_DESCRIPTOR),
			virtualGuildType: 'home',
			searchValues: ['Home', 'DM', 'DMs', 'Direct Messages', 'Messages'],
			sortWeight: Date.now(),
		});
	}
	if (Favorites.hasAnyFavorites) {
		virtualGuildCandidates.push({
			type: QuickSwitcherResultTypes.VIRTUAL_GUILD,
			id: 'favorites',
			title: i18n._(FAVORITES_DESCRIPTOR),
			subtitle: undefined,
			virtualGuildType: 'favorites',
			searchValues: ['Favorites', 'Fav', 'Starred', FAVORITES_GUILD_ID],
			sortWeight: Date.now(),
		});
	}
	const settingsCandidates: Array<SettingsCandidate> = [];
	const hasExpressionPackAccess =
		(Users.getCurrentUser()?.isStaff() ?? false) && DeveloperOptions.showExpressionPacksSettings;
	const accessibleTabs = getSettingsTabs(i18n).filter((tab) => {
		if (!hasExpressionPackAccess && tab.type === 'expression_packs') {
			return false;
		}
		if (!UserSettings.developerMode && (tab.type === 'embed_debugger' || tab.type === 'component_gallery')) {
			return false;
		}
		return true;
	});
	for (const tab of accessibleTabs) {
		settingsCandidates.push({
			type: QuickSwitcherResultTypes.SETTINGS,
			id: tab.type,
			title: tab.label,
			subtitle: undefined,
			settingsTab: tab,
			settingsSubtab: undefined,
			searchValues: [tab.label, 'settings', 'preferences', tab.type],
			sortWeight: 0,
		});
	}
	for (const subtab of getSettingsSubtabs(i18n)) {
		const parentTab = accessibleTabs.find((t) => t.type === subtab.parentTab);
		if (!parentTab) continue;
		settingsCandidates.push({
			type: QuickSwitcherResultTypes.SETTINGS,
			id: `${subtab.parentTab}_${subtab.type}`,
			title: subtab.label,
			subtitle: parentTab.label,
			settingsTab: parentTab,
			settingsSubtab: subtab,
			searchValues: [subtab.label, parentTab.label, 'settings', subtab.type],
			sortWeight: 0,
		});
	}
	return {
		users: Array.from(userCandidates.values()),
		userByChannelId,
		groupDMs: groupDMCandidates,
		groupDMByChannelId,
		textChannels: textChannelCandidates,
		voiceChannels: voiceChannelCandidates,
		guilds: guildCandidates,
		virtualGuilds: virtualGuildCandidates,
		settings: settingsCandidates,
		channelById,
	};
}
