// SPDX-License-Identifier: AGPL-3.0-or-later

import {DesktopEntitySettingsView} from '@app/features/app/components/dialogs/shared/DesktopEntitySettingsView';

import {
	SettingsModalSidebarItem,
	settingsModalStyles,
} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import Authentication from '@app/features/auth/state/Authentication';
import EmojiStickerLayout from '@app/features/emoji/state/EmojiStickerLayout';
import {GuildDeleteModal} from '@app/features/guild/components/modals/GuildDeleteModal';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildMemberLayout from '@app/features/guild/state/GuildMemberLayout';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';

import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';

import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import type {
	GuildSettingsTab,
	GuildSettingsTabCategories,
	GuildSettingsTabType,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {
	GUILD_SETTINGS_LABEL_DESCRIPTOR,
	getGuildSettingsCategoryLabel,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {getGuildSettingsTabComponent} from '@app/features/user/components/settings_utils/GuildSettingsTabComponents';

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, TrashIcon} from '@phosphor-icons/react';

import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const BACK_TO_ROLES_DESCRIPTOR = msg({
	message: 'Back to roles',
	comment: 'Back-navigation label that returns from an individual role editor to the roles list.',
});
const BACK_TO_OVERRIDES_DESCRIPTOR = msg({
	message: 'Back to overrides',
	comment: 'Back-navigation label that returns from permission override editing to the overrides list.',
});
const DELETE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Delete community',
	comment: 'Destructive footer button in community settings. Opens the delete-community confirmation.',
});
const MEMBERS_PAGE_ACTION_DESCRIPTOR = msg({
	message: 'Opens the Members page. Press Space or Enter to open.',
	comment:
		'Screen-reader-only hint for the Members entry in community settings. The entry receives keyboard focus without automatically opening.',
});

interface DesktopGuildSettingsViewProps {
	guild: Guild;
	groupedSettingsTabs: Record<string, Array<GuildSettingsTab>>;
	currentTab?: GuildSettingsTab;
	selectedTab: GuildSettingsTabType;
	onTabSelect: (tab: GuildSettingsTabType) => void;
}

export const DesktopGuildSettingsView: React.FC<DesktopGuildSettingsViewProps> = observer(
	({guild, groupedSettingsTabs, currentTab, selectedTab, onTabSelect}) => {
		const {i18n} = useLingui();
		const CurrentTabComponent = currentTab ? getGuildSettingsTabComponent(currentTab.type) : null;
		const guildOverrideOwnerId = useMemo(() => `guild-roles-${guild.id}`, [guild.id]);
		const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId: guild.id});
		const handleDeleteGuild = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<GuildDeleteModal
						guildId={guild.id}
						data-flx="app.desktop-guild-settings-view.handle-delete-guild.guild-delete-modal"
					/>
				)),
			);
		}, [guild.id]);
		const useOverride = SettingsSidebar.useOverride;
		const emojiLayout = EmojiStickerLayout.getEmojiLayout();
		const stickerViewMode = EmojiStickerLayout.getStickerViewMode();
		const memberViewMode = GuildMemberLayout.getViewMode();
		const scrollKey = useMemo(() => {
			const baseKey = `guild-settings-${guild.id}-${selectedTab ?? 'none'}`;
			switch (selectedTab) {
				case 'emoji':
					return `${baseKey}-emoji-${emojiLayout}`;
				case 'stickers':
					return `${baseKey}-stickers-${stickerViewMode}`;
				case 'members':
					return `${baseKey}-members-${memberViewMode}`;
				default:
					return baseKey;
			}
		}, [guild.id, selectedTab, emojiLayout, stickerViewMode, memberViewMode]);
		return (
			<DesktopEntitySettingsView
				groupedSettingsTabs={groupedSettingsTabs}
				selectedTab={selectedTab}
				onTabSelect={onTabSelect}
				overrideOwnerId={guildOverrideOwnerId}
				overrideTab="roles"
				overrideBackLabel={
					selectedTab === 'roles' ? i18n._(BACK_TO_ROLES_DESCRIPTOR) : i18n._(BACK_TO_OVERRIDES_DESCRIPTOR)
				}
				sidebarTitle={guild.name}
				settingsTitle={currentTab?.label || i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR)}
				scrollKey={scrollKey}
				idPrefix="guild-settings"
				hiddenCategory="guild_settings"
				categoryLabel={(category) => getGuildSettingsCategoryLabel(i18n, category as GuildSettingsTabCategories)}
				presentTab={(tab) => ({
					label:
						tab.type === 'members' ? (
							<span
								className={settingsModalStyles.externalTabLabel}
								data-flx="app.desktop-guild-settings-view.external-tab-label"
							>
								<span
									className={settingsModalStyles.externalTabLabelText}
									data-flx="app.desktop-guild-settings-view.external-tab-label-text"
								>
									{tab.label}
								</span>
								<span
									className={settingsModalStyles.srOnly}
									data-flx="app.desktop-guild-settings-view.external-tab-screen-reader-hint"
								>
									{i18n._(MEMBERS_PAGE_ACTION_DESCRIPTOR)}
								</span>
								<ArrowSquareOutIcon
									size={remFromPx(16)}
									weight="bold"
									className={settingsModalStyles.externalTabIcon}
									aria-hidden="true"
									data-flx="app.desktop-guild-settings-view.external-tab-icon"
								/>
							</span>
						) : (
							tab.label
						),
					requiresExplicitKeyboardActivation: tab.type === 'roles' || tab.type === 'members',
					focusContent: tab.type !== 'members',
				})}
				content={
					CurrentTabComponent ? (
						<CurrentTabComponent guildId={guild.id} data-flx="app.desktop-guild-settings-view.current-tab-component" />
					) : null
				}
				dangerAction={(checkUnsavedChanges) =>
					guild.isOwner(Authentication.currentUserId) && !isStockCommunityGuild(guild.id) ? (
						<SettingsModalSidebarItem
							icon={TrashIcon}
							label={i18n._(DELETE_COMMUNITY_DESCRIPTOR)}
							danger={true}
							onClick={() => {
								if (checkUnsavedChanges()) return;
								handleDeleteGuild();
							}}
							data-flx="app.desktop-guild-settings-view.settings-modal-sidebar-item.delete-guild"
						/>
					) : null
				}
				thresholdTargetId={selectedTab === 'roles' && canManageGuild && useOverride ? guild.id : null}
				data-flx="app.desktop-guild-settings-view.desktop-entity-settings-view"
			/>
		);
	},
);
