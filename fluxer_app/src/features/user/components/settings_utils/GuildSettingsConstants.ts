// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildAuditLogTab from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTab';
import GuildBansTab from '@app/features/guild/components/modals/guild_tabs/GuildBansTab';
import GuildDiscoveryTab from '@app/features/guild/components/modals/guild_tabs/GuildDiscoveryTab';
import GuildEmojiTab from '@app/features/guild/components/modals/guild_tabs/GuildEmojiTab';
import GuildEventsTab from '@app/features/guild/components/modals/guild_tabs/GuildEventsTab';
import GuildInvitesTab from '@app/features/guild/components/modals/guild_tabs/GuildInvitesTab';
import GuildModerationTab from '@app/features/guild/components/modals/guild_tabs/GuildModerationTab';
import GuildRolesTab from '@app/features/guild/components/modals/guild_tabs/GuildRolesTab';
import GuildStickersTab from '@app/features/guild/components/modals/guild_tabs/GuildStickersTab';
import GuildVanityURLTab from '@app/features/guild/components/modals/guild_tabs/GuildVanityURLTab';
import GuildWebhooksTab from '@app/features/guild/components/modals/guild_tabs/GuildWebhooksTab';
import GuildOverviewTab from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {
	BookOpenIcon,
	CalendarIcon,
	CompassIcon,
	GearIcon,
	HammerIcon,
	type Icon,
	type IconWeight,
	LinkIcon,
	ProhibitIcon,
	ShieldIcon,
	SmileyIcon,
	StickerIcon,
	TicketIcon,
	UserIcon,
	WebhooksLogoIcon,
} from '@phosphor-icons/react';
import type React from 'react';

const EMOJI_DESCRIPTOR = msg({
	message: 'Emoji',
	context: 'community-settings-tab',
	comment: 'Community settings tab for managing custom emoji uploaded to the community.',
});
const STICKERS_DESCRIPTOR = msg({
	message: 'Stickers',
	context: 'community-settings-tab',
	comment: 'Community settings tab for managing custom stickers uploaded to the community.',
});
const OVERVIEW_DESCRIPTOR = msg({
	message: 'Overview',
	context: 'community-settings-tab',
	comment: 'Community settings tab for basic server/community details.',
});
const ROLES_DESCRIPTOR = msg({
	message: 'Roles',
	context: 'community-settings-tab',
	comment: 'Community settings tab for role hierarchy and permission rules.',
});
const MODERATION_DESCRIPTOR = msg({
	message: 'Moderation',
	context: 'community-settings-tab',
	comment: 'Community settings tab for moderation and safety controls.',
});
const ACTIVITY_LOG_DESCRIPTOR = msg({
	message: 'Activity log',
	context: 'community-settings-tab',
	comment: 'Community settings tab showing activity log entries and moderation history.',
});
const WEBHOOKS_DESCRIPTOR = msg({
	message: 'Webhooks',
	context: 'community-settings-tab',
	comment: 'Community settings tab for configuring webhooks.',
});
const VANITY_URL_DESCRIPTOR = msg({
	message: 'Vanity URL',
	context: 'community-settings-tab',
	comment: 'Community settings tab for the community vanity/custom invite link.',
});
const EVENTS_DESCRIPTOR = msg({
	message: 'Events',
	context: 'community-settings-tab',
	comment: 'Community settings tab for creating and managing community calendar events.',
});
const DISCOVERY_DESCRIPTOR = msg({
	message: 'Discovery',
	context: 'community-settings-tab',
	comment: 'Community settings tab for public community discovery/listing.',
});
const MEMBERS_DESCRIPTOR = msg({
	message: 'Members',
	context: 'community-settings-tab',
	comment: 'Community settings tab for managing community members.',
});
const INVITES_DESCRIPTOR = msg({
	message: 'Invites',
	context: 'community-settings-tab',
	comment: 'Community settings tab for invite link management.',
});
const BANS_DESCRIPTOR = msg({
	message: 'Bans',
	context: 'community-settings-tab',
	comment: 'Community settings tab for banned users.',
});

export type GuildSettingsTabType =
	| 'overview'
	| 'roles'
	| 'emoji'
	| 'stickers'
	| 'moderation'
	| 'audit_log'
	| 'webhooks'
	| 'vanity_url'
	| 'discovery'
	| 'events'
	| 'members'
	| 'invites'
	| 'bans';
export type GuildSettingsTabCategories =
	| 'guild_settings'
	| 'expressions'
	| 'community'
	| 'integrations'
	| 'user_management';

export interface GuildSettingsTab {
	type: GuildSettingsTabType;
	category: GuildSettingsTabCategories;
	label: string;
	icon: Icon;
	iconWeight?: IconWeight;
	component: React.ComponentType<{
		guildId: string;
	}>;
	permission?: bigint | ReadonlyArray<bigint>;
	requireFeature?: string;
}

interface GuildSettingsTabDescriptor {
	type: GuildSettingsTabType;
	category: GuildSettingsTabCategories;
	label: MessageDescriptor;
	icon: Icon;
	iconWeight?: IconWeight;
	component: React.ComponentType<{
		guildId: string;
	}>;
	permission?: bigint | ReadonlyArray<bigint>;
	requireFeature?: string;
}

const GUILD_SETTINGS_TABS_DESCRIPTORS: Array<GuildSettingsTabDescriptor> = [
	{
		type: 'overview',
		category: 'guild_settings',
		label: OVERVIEW_DESCRIPTOR,
		icon: GearIcon,
		component: GuildOverviewTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'roles',
		category: 'guild_settings',
		label: ROLES_DESCRIPTOR,
		icon: ShieldIcon,
		component: GuildRolesTab,
		permission: Permissions.MANAGE_ROLES,
	},
	{
		type: 'moderation',
		category: 'guild_settings',
		label: MODERATION_DESCRIPTOR,
		icon: HammerIcon,
		component: GuildModerationTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'audit_log',
		category: 'guild_settings',
		label: ACTIVITY_LOG_DESCRIPTOR,
		icon: BookOpenIcon,
		component: GuildAuditLogTab,
		permission: Permissions.VIEW_AUDIT_LOG,
	},
	{
		type: 'emoji',
		category: 'expressions',
		label: EMOJI_DESCRIPTOR,
		icon: SmileyIcon,
		component: GuildEmojiTab,
		permission: [Permissions.CREATE_EXPRESSIONS, Permissions.MANAGE_EXPRESSIONS],
	},
	{
		type: 'stickers',
		category: 'expressions',
		label: STICKERS_DESCRIPTOR,
		icon: StickerIcon,
		component: GuildStickersTab,
		permission: [Permissions.CREATE_EXPRESSIONS, Permissions.MANAGE_EXPRESSIONS],
	},
	{
		type: 'events',
		category: 'community',
		label: EVENTS_DESCRIPTOR,
		icon: CalendarIcon,
		component: GuildEventsTab,
		permission: [Permissions.CREATE_EVENTS, Permissions.MANAGE_EVENTS],
	},
	{
		type: 'discovery',
		category: 'community',
		label: DISCOVERY_DESCRIPTOR,
		icon: CompassIcon,
		iconWeight: 'fill',
		component: GuildDiscoveryTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'vanity_url',
		category: 'community',
		label: VANITY_URL_DESCRIPTOR,
		icon: LinkIcon,
		iconWeight: 'bold',
		component: GuildVanityURLTab,
		permission: Permissions.MANAGE_GUILD,
		requireFeature: GuildFeatures.VANITY_URL,
	},
	{
		type: 'webhooks',
		category: 'integrations',
		label: WEBHOOKS_DESCRIPTOR,
		icon: WebhooksLogoIcon,
		component: GuildWebhooksTab,
		permission: Permissions.MANAGE_WEBHOOKS,
	},
	{
		type: 'members',
		category: 'user_management',
		label: MEMBERS_DESCRIPTOR,
		icon: UserIcon,
		component: () => null,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'invites',
		category: 'user_management',
		label: INVITES_DESCRIPTOR,
		icon: TicketIcon,
		component: GuildInvitesTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'bans',
		category: 'user_management',
		label: BANS_DESCRIPTOR,
		icon: ProhibitIcon,
		component: GuildBansTab,
		permission: Permissions.BAN_MEMBERS,
	},
];
export const GUILD_SETTINGS_LABEL_DESCRIPTOR = msg({
	message: 'Community settings',
	comment: 'Root label for community settings modal and settings search paths.',
});

const EXPRESSIONS_CATEGORY_DESCRIPTOR = msg({
	message: 'Expressions',
	comment: 'Community settings sidebar category grouping the emoji and stickers tabs.',
});
const COMMUNITY_CATEGORY_DESCRIPTOR = msg({
	message: 'Community',
	comment: 'Community settings sidebar category grouping discovery and the vanity URL.',
});
const INTEGRATIONS_CATEGORY_DESCRIPTOR = msg({
	message: 'Integrations',
	comment: 'Community settings sidebar category grouping webhooks.',
});
const PEOPLE_CATEGORY_DESCRIPTOR = msg({
	message: 'People',
	comment: 'Community settings sidebar category grouping members, invites, and bans.',
});

export function getGuildSettingsCategoryLabel(i18n: I18n, category: GuildSettingsTabCategories): string {
	switch (category) {
		case 'guild_settings':
			return '';
		case 'expressions':
			return i18n._(EXPRESSIONS_CATEGORY_DESCRIPTOR);
		case 'community':
			return i18n._(COMMUNITY_CATEGORY_DESCRIPTOR);
		case 'integrations':
			return i18n._(INTEGRATIONS_CATEGORY_DESCRIPTOR);
		case 'user_management':
			return i18n._(PEOPLE_CATEGORY_DESCRIPTOR);
	}
}

export function getGuildSettingsTabLabel(i18n: I18n, tabType: GuildSettingsTabType): string {
	const tab = GUILD_SETTINGS_TABS_DESCRIPTORS.find((candidate) => candidate.type === tabType);
	return tab ? i18n._(tab.label) : '';
}

export function formatGuildSettingsPath(i18n: I18n, tabType: GuildSettingsTabType): string {
	return [i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR), getGuildSettingsTabLabel(i18n, tabType)].filter(Boolean).join(' > ');
}

export const getGuildSettingsTabs = (i18n: I18n): Array<GuildSettingsTab> => {
	return GUILD_SETTINGS_TABS_DESCRIPTORS.map((tab) => ({
		...tab,
		label: getGuildSettingsTabLabel(i18n, tab.type),
	}));
};
