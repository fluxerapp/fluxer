// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceBadgeActivity} from '@app/features/app/components/layout/sidebar_nav/VoiceBadge';
import {
	type SidebarVoiceRow,
	SidebarVoiceSummaryScope,
	useSidebarVoiceSummary,
} from '@app/features/app/hooks/useSidebarVoiceSummary';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildCount, {type GuildCounts} from '@app/features/guild/state/GuildCount';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {MENTION_COUNT_ARIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind, {type KeyCombo} from '@app/features/input/state/InputKeybind';
import Permission from '@app/features/permissions/state/Permission';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {Permissions as PermissionBits} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const MUTED_DESCRIPTOR = msg({
	message: 'Muted',
	comment: 'Short label in the sidebar navigation guild list item.',
});
const UNREAD_DESCRIPTOR = msg({
	message: 'unread',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const MUTED_UNTIL_DESCRIPTOR = msg({
	message: 'Muted until {dateUtilsGetFormattedDateTimeNewDateMuteConfigEndTime}',
	comment: 'Community sidebar tooltip showing when the per-community mute will expire. Date/time is interpolated.',
});
const SELECTED_DESCRIPTOR = msg({
	message: 'selected',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const MUTED_ARIA_DESCRIPTOR = msg({
	message: 'muted',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const TEMPORARILY_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'temporarily unavailable',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});
const VOICE_ACTIVITY_DESCRIPTOR = msg({
	message: 'voice activity',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list item.',
});

export interface GuildListItemState {
	readonly hasUnreadMessages: boolean;
	readonly mentionCount: number;
	readonly isMuted: boolean;
	readonly mutedText: string | null;
	readonly canManageGuild: boolean;
	readonly voiceRows: ReadonlyArray<SidebarVoiceRow>;
	readonly hasVoiceActivity: boolean;
	readonly voiceBadgeActivity: VoiceBadgeActivity | null;
	readonly guildCounts: GuildCounts | null;
	readonly currentLocale: string;
	readonly navigationKeybind: KeyCombo | null;
	readonly guildARIALabel: string;
}

interface UseGuildListItemStateOptions {
	readonly guild: Guild;
	readonly guildIndex: number | null;
	readonly i18n: I18n;
	readonly isSelected: boolean;
	readonly selectedGuildIndex: number | null;
}

interface ResolveMutedTextQuery {
	readonly i18n: I18n;
	readonly isMuted: boolean;
	readonly muteEndTime: string | null;
}

function resolveNavigationKeybind(guildIndex: number | null, selectedGuildIndex: number | null): KeyCombo | null {
	if (guildIndex == null || selectedGuildIndex == null || selectedGuildIndex === -1) return null;
	if (guildIndex < selectedGuildIndex) return Keybind.getByAction('nav_guild_prev').combo;
	if (guildIndex > selectedGuildIndex) return Keybind.getByAction('nav_guild_next').combo;
	return null;
}

function resolveMutedText({i18n, isMuted, muteEndTime}: ResolveMutedTextQuery): string | null {
	if (!isMuted) {
		return null;
	}
	if (muteEndTime == null) {
		return i18n._(MUTED_DESCRIPTOR);
	}
	if (new Date(muteEndTime).getTime() <= Date.now()) {
		return null;
	}
	return i18n._(MUTED_UNTIL_DESCRIPTOR, {
		dateUtilsGetFormattedDateTimeNewDateMuteConfigEndTime: DateUtils.getFormattedDateTime(new Date(muteEndTime)),
	});
}

export function useGuildListItemState({
	guild,
	guildIndex,
	i18n,
	isSelected,
	selectedGuildIndex,
}: UseGuildListItemStateOptions): GuildListItemState {
	const hasUnreadMessages = GuildReadState.guildIsUnread(guild.id);
	const mentionCount = GuildReadState.mentionCountForGuild(guild.id);
	const settings = UserGuildSettings.getSettingsForScope(guild.id);
	const isMuted = settings.muted;
	const muteConfig = settings.mute_config;
	let muteEndTime: string | null = null;
	if (muteConfig != null) {
		const configuredMuteEndTime = muteConfig.end_time;
		if (configuredMuteEndTime != null) {
			muteEndTime = configuredMuteEndTime;
		}
	}
	const mutedText = resolveMutedText({i18n, isMuted, muteEndTime});
	const canManageGuild = Permission.can(PermissionBits.MANAGE_GUILD, guild);
	const {
		voiceRows,
		hasVoiceActivity,
		badgeActivity: voiceBadgeActivity,
	} = useSidebarVoiceSummary({
		scope: SidebarVoiceSummaryScope.GUILD,
		voiceStates: MediaEngine.getAllVoiceStatesInGuild(guild.id),
		guildId: guild.id,
	});
	const guildARIAParts = [guild.name];
	if (isSelected) guildARIAParts.push(i18n._(SELECTED_DESCRIPTOR));
	if (mentionCount > 0) guildARIAParts.push(i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount}));
	else if (hasUnreadMessages) guildARIAParts.push(i18n._(UNREAD_DESCRIPTOR));
	if (isMuted) guildARIAParts.push(i18n._(MUTED_ARIA_DESCRIPTOR));
	if (guild.unavailable) guildARIAParts.push(i18n._(TEMPORARILY_UNAVAILABLE_DESCRIPTOR));
	if (hasVoiceActivity) guildARIAParts.push(i18n._(VOICE_ACTIVITY_DESCRIPTOR));
	return {
		hasUnreadMessages,
		mentionCount,
		isMuted,
		mutedText,
		canManageGuild,
		voiceRows,
		hasVoiceActivity,
		voiceBadgeActivity,
		guildCounts: GuildCount.getCounts(guild.id),
		currentLocale: getCurrentLocale(),
		navigationKeybind: resolveNavigationKeybind(guildIndex, selectedGuildIndex),
		guildARIALabel: guildARIAParts.join(', '),
	};
}
