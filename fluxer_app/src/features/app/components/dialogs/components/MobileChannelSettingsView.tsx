// SPDX-License-Identifier: AGPL-3.0-or-later

import {MobileEntitySettingsView} from '@app/features/app/components/dialogs/shared/MobileEntitySettingsView';

import {MobileSettingsDangerItem} from '@app/features/app/components/dialogs/shared/MobileSettingsComponents';
import {resolveSettingsTitle} from '@app/features/app/components/dialogs/shared/SettingsContentPresentation';

import {ChannelDeleteModal} from '@app/features/channel/components/modals/ChannelDeleteModal';
import type {Channel} from '@app/features/channel/models/Channel';
import {DELETE_CATEGORY_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import channelStyles from '@app/features/guild/components/modals/GuildSettingsModal.module.css';
import Permission from '@app/features/permissions/state/Permission';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';

import styles from '@app/features/user/components/modals/UserSettingsModal.module.css';
import type {
	ChannelSettingsTab,
	ChannelSettingsTabType,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import type {MobileNavigationState} from '@app/features/user/hooks/useMobileNavigation';

import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';

import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const DELETE_CHANNEL_DESCRIPTOR = msg({
	message: 'Delete channel',
	comment: 'Short label in the settings dialog mobile channel settings view. Keep the tone plain and specific.',
});

interface MobileChannelSettingsViewProps {
	channel: Channel;
	groupedSettingsTabs: Record<string, Array<ChannelSettingsTab>>;
	currentTab?: ChannelSettingsTab;
	mobileNav: MobileNavigationState<ChannelSettingsTabType>;
	onBack: () => void;
	onTabSelect: (tabType: string, title: string) => void;
}

export const MobileChannelSettingsView: React.FC<MobileChannelSettingsViewProps> = observer(
	({channel, groupedSettingsTabs, currentTab, mobileNav, onBack, onTabSelect}) => {
		const {i18n} = useLingui();
		const handleDeleteChannel = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<ChannelDeleteModal
						channelId={channel.id}
						data-flx="app.mobile-channel-settings-view.handle-delete-channel.channel-delete-modal"
					/>
				)),
			);
		}, [channel.id]);
		const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
		const canManageChannel = Permission.can(Permissions.MANAGE_CHANNELS, {
			channelId: channel.id,
			guildId: channel.guildId,
		});
		const currentTabLabel = currentTab ? currentTab.label : null;
		const currentViewTitle = mobileNav.currentView ? mobileNav.currentView.title : null;
		const mobileTitle = resolveSettingsTitle(currentTabLabel, currentViewTitle);
		const dangerAction = canManageChannel ? (
			<MobileSettingsDangerItem
				icon={TrashIcon}
				label={isCategory ? i18n._(DELETE_CATEGORY_DESCRIPTOR) : i18n._(DELETE_CHANNEL_DESCRIPTOR)}
				onClick={handleDeleteChannel}
				data-flx="app.mobile-channel-settings-view.mobile-settings-danger-item.delete-channel"
			/>
		) : null;
		return (
			<MobileEntitySettingsView
				groupedSettingsTabs={groupedSettingsTabs}
				currentTab={currentTab}
				mobileNav={mobileNav}
				onBack={onBack}
				onTabSelect={onTabSelect}
				rootTitle={
					<span
						className={channelStyles.channelNameWithIcon}
						data-flx="app.mobile-channel-settings-view.channel-name-with-icon"
					>
						{ChannelUtils.getIcon(channel, {className: channelStyles.channelNameIcon, weight: 'bold'})}
						<span className={channelStyles.channelNameText} data-flx="app.mobile-channel-settings-view.channel-name">
							{channel.name}
						</span>
					</span>
				}
				mobileTitle={mobileTitle}
				pageLinkHref={null}
				content={
					currentTab ? (
						<currentTab.component
							channelId={channel.id}
							data-flx="app.mobile-channel-settings-view.current-tab-component"
						/>
					) : null
				}
				dangerAction={dangerAction}
				hiddenCategories={['channel_settings']}
				contentScrollerClassName={styles.mobileContentScroller}
				contentInnerClassName={styles.mobileContentInner}
				contentScrollerKey="mobile-channel-settings-content-scroller"
				data-flx="app.mobile-channel-settings-view.mobile-entity-settings-view"
			/>
		);
	},
);
