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
import {
	BookOpenIcon,
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
import {GuildFeatures, Permissions} from '~/Constants';
import GuildAuditLogTab from '../guildTabs/GuildAuditLogTab';
import GuildBansTab from '../guildTabs/GuildBansTab';
import GuildEmojiTab from '../guildTabs/GuildEmojiTab';
import GuildInvitesTab from '../guildTabs/GuildInvitesTab';
import GuildMembersTab from '../guildTabs/GuildMembersTab';
import GuildModerationTab from '../guildTabs/GuildModerationTab';
import GuildOverviewTab from '../guildTabs/GuildOverviewTab';
import GuildRolesTab from '../guildTabs/GuildRolesTab';
import GuildStickersTab from '../guildTabs/GuildStickersTab';
import GuildVanityURLTab from '../guildTabs/GuildVanityURLTab';
import GuildWebhooksTab from '../guildTabs/GuildWebhooksTab';

export type GuildSettingsTabType =
	| 'overview'
	| 'roles'
	| 'emoji'
	| 'stickers'
	| 'moderation'
	| 'audit_log'
	| 'webhooks'
	| 'vanity_url'
	| 'members'
	| 'invites'
	| 'bans';
type GuildSettingsTabCategories = 'guild_settings' | 'user_management';

export interface GuildSettingsTab {
	type: GuildSettingsTabType;
	category: GuildSettingsTabCategories;
	label: string;
	icon: Icon;
	iconWeight?: IconWeight;
	component: React.ComponentType<{guildId: string}>;
	permission?: bigint;
	requireFeature?: string;
}

interface GuildSettingsTabDescriptor {
	type: GuildSettingsTabType;
	category: GuildSettingsTabCategories;
	label: MessageDescriptor;
	icon: Icon;
	iconWeight?: IconWeight;
	component: React.ComponentType<{guildId: string}>;
	permission?: bigint;
	requireFeature?: string;
}

const GUILD_SETTINGS_TABS_DESCRIPTORS: Array<GuildSettingsTabDescriptor> = [
	{
		type: 'overview',
		category: 'guild_settings',
		label: msg`General`,
		icon: GearIcon,
		component: GuildOverviewTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'roles',
		category: 'guild_settings',
		label: msg`Roles & Permissions`,
		icon: ShieldIcon,
		component: GuildRolesTab,
		permission: Permissions.MANAGE_ROLES,
	},
	{
		type: 'emoji',
		category: 'guild_settings',
		label: msg`Custom Emoji`,
		icon: SmileyIcon,
		component: GuildEmojiTab,
		permission: Permissions.MANAGE_EXPRESSIONS,
	},
	{
		type: 'stickers',
		category: 'guild_settings',
		label: msg`Custom Stickers`,
		icon: StickerIcon,
		component: GuildStickersTab,
		permission: Permissions.MANAGE_EXPRESSIONS,
	},
	{
		type: 'moderation',
		category: 'guild_settings',
		label: msg`Safety & Moderation`,
		icon: HammerIcon,
		component: GuildModerationTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'audit_log',
		category: 'guild_settings',
		label: msg`Activity Log`,
		icon: BookOpenIcon,
		component: GuildAuditLogTab,
		permission: Permissions.VIEW_AUDIT_LOG,
	},
	{
		type: 'webhooks',
		category: 'guild_settings',
		label: msg`Webhooks`,
		icon: WebhooksLogoIcon,
		component: GuildWebhooksTab,
		permission: Permissions.MANAGE_WEBHOOKS,
	},
	{
		type: 'vanity_url',
		category: 'guild_settings',
		label: msg`Custom Invite URL`,
		icon: LinkIcon,
		iconWeight: 'bold',
		component: GuildVanityURLTab,
		permission: Permissions.MANAGE_GUILD,
		requireFeature: GuildFeatures.VANITY_URL,
	},
	{
		type: 'members',
		category: 'user_management',
		label: msg`Members`,
		icon: UserIcon,
		component: GuildMembersTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'invites',
		category: 'user_management',
		label: msg`Invite Links`,
		icon: TicketIcon,
		component: GuildInvitesTab,
		permission: Permissions.MANAGE_GUILD,
	},
	{
		type: 'bans',
		category: 'user_management',
		label: msg`Bans`,
		icon: ProhibitIcon,
		component: GuildBansTab,
		permission: Permissions.BAN_MEMBERS,
	},
];

export const getGuildSettingsTabs = (t: (msg: MessageDescriptor) => string): Array<GuildSettingsTab> => {
	return GUILD_SETTINGS_TABS_DESCRIPTORS.map((tab) => ({
		...tab,
		label: t(tab.label),
	}));
};
