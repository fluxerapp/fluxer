// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Permission from '@app/features/permissions/state/Permission';
import type {
	ChannelSettingsTab,
	ChannelSettingsTabType,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import {getChannelSettingsTabs} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';

export interface ChannelSettingsModalProps {
	channelId: string;
	initialMobileTab?: ChannelSettingsTabType;
}

export function getAvailableTabs(i18n: I18n, channelId: string): Array<ChannelSettingsTab> {
	const channel = Channels.getChannel(channelId);
	if (!channel) return getChannelSettingsTabs(i18n);
	let filteredTabs = getChannelSettingsTabs(i18n);
	if (channel.type === ChannelTypes.GUILD_CATEGORY) {
		filteredTabs = filteredTabs.filter((tab) => tab.type === 'overview' || tab.type === 'permissions');
	}
	if (channel.type === ChannelTypes.GUILD_LINK) {
		filteredTabs = filteredTabs.filter((tab) => tab.type !== 'webhooks');
	}
	const permissionContext = {channelId: channel.id, guildId: channel.guildId};
	const canUpdateRtcRegion =
		channel.type === ChannelTypes.GUILD_VOICE && Permission.can(Permissions.UPDATE_RTC_REGION, permissionContext);
	return filteredTabs.filter((tab) => {
		if (!tab.permission) return true;
		if (Permission.can(tab.permission, permissionContext)) {
			return true;
		}
		if (tab.type === 'overview' && canUpdateRtcRegion) {
			return true;
		}
		return false;
	});
}

export function getGroupedSettingsTabs(availableTabs: Array<ChannelSettingsTab>) {
	return availableTabs.reduce(
		(acc: Record<string, Array<ChannelSettingsTab>>, tab: ChannelSettingsTab) => {
			if (!acc[tab.category]) {
				acc[tab.category] = [];
			}
			acc[tab.category].push(tab);
			return acc;
		},
		{} as Record<string, Array<ChannelSettingsTab>>,
	);
}
