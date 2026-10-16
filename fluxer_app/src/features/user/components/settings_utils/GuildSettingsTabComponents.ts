// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildAuditLogTab from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTab';
import GuildBansTab from '@app/features/guild/components/modals/guild_tabs/GuildBansTab';
import GuildDiscoveryTab from '@app/features/guild/components/modals/guild_tabs/GuildDiscoveryTab';
import GuildEmojiTab from '@app/features/guild/components/modals/guild_tabs/GuildEmojiTab';
import GuildInvitesTab from '@app/features/guild/components/modals/guild_tabs/GuildInvitesTab';
import GuildModerationTab from '@app/features/guild/components/modals/guild_tabs/GuildModerationTab';
import GuildRolesTab from '@app/features/guild/components/modals/guild_tabs/GuildRolesTab';
import GuildStickersTab from '@app/features/guild/components/modals/guild_tabs/GuildStickersTab';
import GuildVanityURLTab from '@app/features/guild/components/modals/guild_tabs/GuildVanityURLTab';
import GuildWebhooksTab from '@app/features/guild/components/modals/guild_tabs/GuildWebhooksTab';
import GuildOverviewTab from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab';
import type {GuildSettingsTabType} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import type React from 'react';

type GuildSettingsTabComponent = React.ComponentType<{guildId: string}>;

const EmptyGuildSettingsTab: GuildSettingsTabComponent = () => null;

const GUILD_SETTINGS_TAB_COMPONENTS: Record<GuildSettingsTabType, GuildSettingsTabComponent> = {
	overview: GuildOverviewTab,
	roles: GuildRolesTab,
	moderation: GuildModerationTab,
	audit_log: GuildAuditLogTab,
	emoji: GuildEmojiTab,
	stickers: GuildStickersTab,
	discovery: GuildDiscoveryTab,
	vanity_url: GuildVanityURLTab,
	webhooks: GuildWebhooksTab,
	members: EmptyGuildSettingsTab,
	invites: GuildInvitesTab,
	bans: GuildBansTab,
};

export function getGuildSettingsTabComponent(tabType: GuildSettingsTabType): GuildSettingsTabComponent {
	return GUILD_SETTINGS_TAB_COMPONENTS[tabType];
}
