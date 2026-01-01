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
import {msg} from '@lingui/core/macro';
import {GearIcon, type Icon, ShieldIcon, TicketIcon, WebhooksLogoIcon} from '@phosphor-icons/react';
import type React from 'react';
import {Permissions} from '~/Constants';
import ChannelInvitesTab from '../channelTabs/ChannelInvitesTab';
import ChannelOverviewTab from '../channelTabs/ChannelOverviewTab';
import ChannelPermissionsTab from '../channelTabs/ChannelPermissionsTab';
import ChannelWebhooksTab from '../channelTabs/ChannelWebhooksTab';

export type ChannelSettingsTabType = 'overview' | 'permissions' | 'invites' | 'webhooks';
type ChannelSettingsTabCategories = 'channel_settings';

export interface ChannelSettingsTab {
	type: ChannelSettingsTabType;
	category: ChannelSettingsTabCategories;
	label: string;
	icon: Icon;
	component: React.ComponentType<{channelId: string}>;
	permission?: bigint;
}

interface ChannelSettingsTabDescriptor {
	type: ChannelSettingsTabType;
	category: ChannelSettingsTabCategories;
	label: MessageDescriptor;
	icon: Icon;
	component: React.ComponentType<{channelId: string}>;
	permission?: bigint;
}

const CHANNEL_SETTINGS_TABS_DESCRIPTORS: Array<ChannelSettingsTabDescriptor> = [
	{
		type: 'overview',
		category: 'channel_settings',
		label: msg`General`,
		icon: GearIcon,
		component: ChannelOverviewTab,
		permission: Permissions.MANAGE_CHANNELS,
	},
	{
		type: 'permissions',
		category: 'channel_settings',
		label: msg`Access Control`,
		icon: ShieldIcon,
		component: ChannelPermissionsTab,
		permission: Permissions.MANAGE_ROLES,
	},
	{
		type: 'invites',
		category: 'channel_settings',
		label: msg`Invite Links`,
		icon: TicketIcon,
		component: ChannelInvitesTab,
		permission: Permissions.MANAGE_CHANNELS,
	},
	{
		type: 'webhooks',
		category: 'channel_settings',
		label: msg`Webhooks`,
		icon: WebhooksLogoIcon,
		component: ChannelWebhooksTab,
		permission: Permissions.MANAGE_WEBHOOKS,
	},
];

export const getChannelSettingsTabs = (t: (msg: MessageDescriptor) => string): Array<ChannelSettingsTab> => {
	return CHANNEL_SETTINGS_TABS_DESCRIPTORS.map((tab) => ({
		...tab,
		label: t(tab.label),
	}));
};
