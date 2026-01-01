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

import type {MessageDescriptor} from '@lingui/core';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {ChannelTypes} from '~/Constants';
import {
	type ChannelSettingsTab,
	type ChannelSettingsTabType,
	getChannelSettingsTabs,
} from '~/components/modals/utils/channelSettingsConstants';
import ChannelStore from '~/stores/ChannelStore';
import PermissionStore from '~/stores/PermissionStore';
import UnsavedChangesStore from '~/stores/UnsavedChangesStore';

export type {ChannelSettingsTabType};

export interface ChannelSettingsModalProps {
	channelId: string;
	initialMobileTab?: ChannelSettingsTabType;
}

export const getAvailableTabs = (
	t: (msg: MessageDescriptor) => string,
	channelId: string,
): Array<ChannelSettingsTab> => {
	const channel = ChannelStore.getChannel(channelId);
	if (!channel) return getChannelSettingsTabs(t);

	let filteredTabs = getChannelSettingsTabs(t);

	if (channel.type === ChannelTypes.GUILD_CATEGORY) {
		filteredTabs = filteredTabs.filter((tab) => tab.type === 'overview' || tab.type === 'permissions');
	}

	if (channel.type === ChannelTypes.GUILD_VOICE) {
		filteredTabs = filteredTabs.filter((tab) => tab.type !== 'webhooks');
	}

	if (channel.type === ChannelTypes.GUILD_LINK) {
		filteredTabs = filteredTabs.filter((tab) => tab.type !== 'webhooks');
	}

	return filteredTabs.filter((tab) => {
		if (tab.permission && !PermissionStore.can(tab.permission, {guildId: channel.guildId})) {
			return false;
		}
		return true;
	});
};

export const getGroupedSettingsTabs = (availableTabs: Array<ChannelSettingsTab>) => {
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
};

export const createHandleClose = (selectedTab: ChannelSettingsTabType) => {
	return () => {
		const checkTabId = selectedTab;
		if (checkTabId && UnsavedChangesStore.unsavedChanges[checkTabId]) {
			UnsavedChangesActionCreators.triggerFlashEffect(checkTabId);
			return;
		}
		ModalActionCreators.pop();
	};
};
