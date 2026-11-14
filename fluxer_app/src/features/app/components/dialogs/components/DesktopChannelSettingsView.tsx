// SPDX-License-Identifier: AGPL-3.0-or-later

import {DesktopEntitySettingsView} from '@app/features/app/components/dialogs/shared/DesktopEntitySettingsView';

import {resolveSettingsTitle} from '@app/features/app/components/dialogs/shared/SettingsContentPresentation';
import {SettingsModalSidebarItem} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import {ChannelDeleteModal} from '@app/features/channel/components/modals/ChannelDeleteModal';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import styles from '@app/features/guild/components/modals/GuildSettingsModal.module.css';

import Permission from '@app/features/permissions/state/Permission';

import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import type {
	ChannelSettingsTab,
	ChannelSettingsTabType,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import {
	CATEGORY_SETTINGS_LABEL_DESCRIPTOR,
	CHANNEL_SETTINGS_LABEL_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';

import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';

import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const BACK_TO_OVERRIDES_DESCRIPTOR = msg({
	message: 'Back to overrides',
	comment: 'Back-navigation label that returns from permission override editing to the overrides list.',
});
const DELETE_CATEGORY_DESCRIPTOR = msg({
	message: 'Delete category',
	comment: 'Destructive footer button in category settings. Opens the delete-category confirmation.',
});
const DELETE_CHANNEL_DESCRIPTOR = msg({
	message: 'Delete channel',
	comment: 'Destructive footer button in channel settings. Opens the delete-channel confirmation.',
});
interface DesktopChannelSettingsViewProps {
	channel: Channel;
	groupedSettingsTabs: Record<string, Array<ChannelSettingsTab>>;
	currentTab?: ChannelSettingsTab;
	selectedTab: ChannelSettingsTabType;
	onTabSelect: (tab: ChannelSettingsTabType) => void;
}

const CATEGORY_LABELS = {
	channel_settings: '',
};
export const DesktopChannelSettingsView: React.FC<DesktopChannelSettingsViewProps> = observer(
	({channel, groupedSettingsTabs, currentTab, selectedTab, onTabSelect}) => {
		const {i18n} = useLingui();
		const channelPermissionsOverrideOwnerId = useMemo(() => `channel-permissions-${channel.id}`, [channel.id]);
		const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId: channel.guildId ?? ''});
		const canManageChannel = Permission.can(Permissions.MANAGE_CHANNELS, {
			channelId: channel.id,
			guildId: channel.guildId,
		});
		const handleDeleteChannel = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<ChannelDeleteModal
						channelId={channel.id}
						data-flx="app.desktop-channel-settings-view.handle-delete-channel.channel-delete-modal"
					/>
				)),
			);
		}, [channel.id]);
		const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
		const useOverride = SettingsSidebar.useOverride;
		const fallbackSettingsTitle = i18n._(
			isCategory ? CATEGORY_SETTINGS_LABEL_DESCRIPTOR : CHANNEL_SETTINGS_LABEL_DESCRIPTOR,
		);
		const currentTabLabel = currentTab ? currentTab.label : null;
		const settingsTitle = resolveSettingsTitle(currentTabLabel, fallbackSettingsTitle);
		const scrollKey = useMemo(
			() => `channel-settings-${channel.id}-${selectedTab ?? 'none'}`,
			[channel.id, selectedTab],
		);
		return (
			<DesktopEntitySettingsView
				groupedSettingsTabs={groupedSettingsTabs}
				selectedTab={selectedTab}
				onTabSelect={onTabSelect}
				overrideOwnerId={channelPermissionsOverrideOwnerId}
				overrideTab="permissions"
				overrideBackLabel={i18n._(BACK_TO_OVERRIDES_DESCRIPTOR)}
				sidebarTitle={
					<span
						className={styles.channelNameWithIcon}
						data-flx="app.desktop-channel-settings-view.channel-name-with-icon"
					>
						{ChannelUtils.getIcon(channel, {className: styles.channelNameIcon, weight: 'bold'})}
						<span className={styles.channelNameText} data-flx="app.desktop-channel-settings-view.channel-name">
							{channel.name}
						</span>
					</span>
				}
				settingsTitle={settingsTitle}
				pageLinkHref={null}
				scrollKey={scrollKey}
				idPrefix="channel-settings"
				hiddenCategory="channel_settings"
				categoryLabel={(category) => CATEGORY_LABELS[category as keyof typeof CATEGORY_LABELS]}
				presentTab={(tab) => ({label: tab.label, requiresExplicitKeyboardActivation: tab.type === 'permissions'})}
				content={
					currentTab ? (
						<currentTab.component
							channelId={channel.id}
							data-flx="app.desktop-channel-settings-view.current-tab-component"
						/>
					) : null
				}
				dangerAction={(checkUnsavedChanges) =>
					!useOverride && canManageChannel ? (
						<SettingsModalSidebarItem
							icon={TrashIcon}
							label={isCategory ? i18n._(DELETE_CATEGORY_DESCRIPTOR) : i18n._(DELETE_CHANNEL_DESCRIPTOR)}
							danger={true}
							onClick={() => {
								if (checkUnsavedChanges()) return;
								handleDeleteChannel();
							}}
							data-flx="app.desktop-channel-settings-view.settings-modal-sidebar-item.delete-channel"
						/>
					) : null
				}
				thresholdTargetId={
					selectedTab === 'permissions' && canManageGuild && channel.guildId && useOverride ? channel.guildId : null
				}
				data-flx="app.desktop-channel-settings-view.desktop-entity-settings-view"
			/>
		);
	},
);
