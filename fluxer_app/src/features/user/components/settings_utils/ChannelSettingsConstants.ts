// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelInvitesTab from '@app/features/channel/components/modals/channel_tabs/ChannelInvitesTab';
import ChannelPermissionsTab from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab';
import ChannelWebhooksTab from '@app/features/channel/components/modals/channel_tabs/ChannelWebhooksTab';
import {ChannelOverviewTabForType} from '@app/features/forum/components/settings/ForumOverviewTab';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {GearIcon, type Icon, ShieldIcon, TicketIcon, WebhooksLogoIcon} from '@phosphor-icons/react';
import type React from 'react';

const OVERVIEW_DESCRIPTOR = msg({
	message: 'Overview',
	context: 'channel-settings-tab',
	comment: 'Channel settings tab for basic channel details.',
});
const PERMISSIONS_DESCRIPTOR = msg({
	message: 'Permissions',
	context: 'channel-settings-tab',
	comment: 'Channel settings tab for role and member permission overwrites.',
});
const INVITES_DESCRIPTOR = msg({
	message: 'Invites',
	context: 'channel-settings-tab',
	comment: 'Channel settings tab for invite management.',
});
const WEBHOOKS_DESCRIPTOR = msg({
	message: 'Webhooks',
	context: 'channel-settings-tab',
	comment: 'Channel settings tab for configuring channel webhooks.',
});

export type ChannelSettingsTabType = 'overview' | 'permissions' | 'invites' | 'webhooks';
type ChannelSettingsTabCategories = 'channel_settings';

export interface ChannelSettingsTab {
	type: ChannelSettingsTabType;
	category: ChannelSettingsTabCategories;
	label: string;
	icon: Icon;
	component: React.ComponentType<{
		channelId: string;
	}>;
	permission?: bigint;
}

interface ChannelSettingsTabDescriptor {
	type: ChannelSettingsTabType;
	category: ChannelSettingsTabCategories;
	label: MessageDescriptor;
	icon: Icon;
	component: React.ComponentType<{
		channelId: string;
	}>;
	permission?: bigint;
}

const CHANNEL_SETTINGS_TABS_DESCRIPTORS: Array<ChannelSettingsTabDescriptor> = [
	{
		type: 'overview',
		category: 'channel_settings',
		label: OVERVIEW_DESCRIPTOR,
		icon: GearIcon,
		component: ChannelOverviewTabForType,
		permission: Permissions.MANAGE_CHANNELS,
	},
	{
		type: 'permissions',
		category: 'channel_settings',
		label: PERMISSIONS_DESCRIPTOR,
		icon: ShieldIcon,
		component: ChannelPermissionsTab,
		permission: Permissions.MANAGE_ROLES,
	},
	{
		type: 'invites',
		category: 'channel_settings',
		label: INVITES_DESCRIPTOR,
		icon: TicketIcon,
		component: ChannelInvitesTab,
		permission: Permissions.MANAGE_CHANNELS,
	},
	{
		type: 'webhooks',
		category: 'channel_settings',
		label: WEBHOOKS_DESCRIPTOR,
		icon: WebhooksLogoIcon,
		component: ChannelWebhooksTab,
		permission: Permissions.MANAGE_WEBHOOKS,
	},
];
export const CHANNEL_SETTINGS_LABEL_DESCRIPTOR = msg({
	message: 'Channel settings',
	comment: 'Root label for channel settings modal and settings search paths.',
});
export const CATEGORY_SETTINGS_LABEL_DESCRIPTOR = msg({
	message: 'Category settings',
	comment: 'Root label for category settings modal and settings search paths.',
});

function getChannelSettingsTabLabel(i18n: I18n, tabType: ChannelSettingsTabType): string {
	const tab = CHANNEL_SETTINGS_TABS_DESCRIPTORS.find((candidate) => candidate.type === tabType);
	return tab ? i18n._(tab.label) : '';
}

export function formatChannelSettingsPath(
	i18n: I18n,
	tabType: ChannelSettingsTabType,
	kind: 'channel' | 'category' = 'channel',
): string {
	const rootLabel =
		kind === 'category' ? i18n._(CATEGORY_SETTINGS_LABEL_DESCRIPTOR) : i18n._(CHANNEL_SETTINGS_LABEL_DESCRIPTOR);
	return [rootLabel, getChannelSettingsTabLabel(i18n, tabType)].filter(Boolean).join(' > ');
}

export const getChannelSettingsTabs = (i18n: I18n): Array<ChannelSettingsTab> => {
	return CHANNEL_SETTINGS_TABS_DESCRIPTORS.map((tab) => ({
		...tab,
		label: getChannelSettingsTabLabel(i18n, tab.type),
	}));
};
