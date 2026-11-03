// SPDX-License-Identifier: AGPL-3.0-or-later

import {DesktopChannelSettingsView} from '@app/features/app/components/dialogs/components/DesktopChannelSettingsView';
import {MobileChannelSettingsView} from '@app/features/app/components/dialogs/components/MobileChannelSettingsView';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SettingsModalContainer} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import Channels from '@app/features/channel/state/Channels';
import {
	type ChannelSettingsModalProps,
	getAvailableTabs,
	getGroupedSettingsTabs,
} from '@app/features/channel/utils/ChannelSettingsModalUtils';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import UnsavedChanges from '@app/features/ui/state/UnsavedChanges';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {
	CATEGORY_SETTINGS_LABEL_DESCRIPTOR,
	CHANNEL_SETTINGS_LABEL_DESCRIPTOR,
	type ChannelSettingsTabType,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import {useMobileNavigation} from '@app/features/user/hooks/useMobileNavigation';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

export const ChannelSettingsModal: React.FC<ChannelSettingsModalProps> = observer(({channelId, initialMobileTab}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const guildId = channel?.guildId;
	const [selectedTab, setSelectedTab] = useState<ChannelSettingsTabType>('overview');
	const availableTabs = useMemo(() => {
		return getAvailableTabs(i18n, channelId);
	}, [i18n.locale, channelId]);
	const isMobileExp = isMobileExperienceEnabled();
	const initialTab = useMemo(() => {
		if (!isMobileExp || !initialMobileTab) return;
		const targetTab = availableTabs.find((tab) => tab.type === initialMobileTab);
		if (!targetTab) return;
		return {tab: initialMobileTab, title: targetTab.label};
	}, [initialMobileTab, availableTabs, isMobileExp]);
	const mobileNav = useMobileNavigation<ChannelSettingsTabType>(initialTab);
	const {enabled: isMobile} = MobileLayout;
	const unsavedChangesState = UnsavedChanges;
	useEffect(() => {
		if (guildId) {
			GatewaySessions.foregroundConnection.syncGuildIfNeeded(guildId, 'channel-settings-modal');
		}
	}, [guildId]);
	useEffect(() => {
		if (!channel) {
			ModalCommands.pop();
		}
	}, [channel]);
	const groupedSettingsTabs = useMemo(() => {
		return getGroupedSettingsTabs(availableTabs);
	}, [availableTabs]);
	const currentTab = useMemo(() => {
		if (!isMobile) {
			return availableTabs.find((tab) => tab.type === selectedTab);
		}
		if (mobileNav.isRootView) return;
		return availableTabs.find((tab) => tab.type === mobileNav.currentView?.tab);
	}, [isMobile, selectedTab, mobileNav.isRootView, mobileNav.currentView, availableTabs]);
	const handleMobileBack = useCallback(() => {
		if (mobileNav.isRootView) {
			ModalCommands.pop();
		} else {
			mobileNav.navigateBack();
		}
	}, [mobileNav]);
	const handleTabSelect = useCallback(
		(tabType: string, title: string) => {
			mobileNav.navigateTo(tabType as ChannelSettingsTabType, title);
		},
		[mobileNav],
	);
	const currentMobileTab = mobileNav.currentView?.tab;
	const handleClose = useCallback(() => {
		const checkTabId = isMobile ? currentMobileTab : selectedTab;
		if (checkTabId && unsavedChangesState.unsavedChanges[checkTabId]) {
			UnsavedChangesCommands.triggerFlashEffect(checkTabId);
			return;
		}
		ModalCommands.pop();
	}, [currentMobileTab, isMobile, selectedTab, unsavedChangesState.unsavedChanges]);
	if (!channel) {
		return null;
	}
	const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
	return (
		<Modal.Root size="fullscreen" onClose={handleClose} data-flx="channel.channel-settings-modal.modal-root">
			<Modal.ScreenReaderLabel
				text={i18n._(isCategory ? CATEGORY_SETTINGS_LABEL_DESCRIPTOR : CHANNEL_SETTINGS_LABEL_DESCRIPTOR)}
				data-flx="channel.channel-settings-modal.modal-screen-reader-label"
			/>
			<SettingsModalContainer fullscreen={true} data-flx="channel.channel-settings-modal.settings-modal-container">
				{isMobile ? (
					<MobileChannelSettingsView
						channel={channel}
						groupedSettingsTabs={groupedSettingsTabs}
						currentTab={currentTab}
						mobileNav={mobileNav}
						onBack={handleMobileBack}
						onTabSelect={handleTabSelect}
						data-flx="channel.channel-settings-modal.mobile-channel-settings-view"
					/>
				) : (
					<DesktopChannelSettingsView
						channel={channel}
						groupedSettingsTabs={groupedSettingsTabs}
						currentTab={currentTab}
						selectedTab={selectedTab}
						onTabSelect={setSelectedTab}
						data-flx="channel.channel-settings-modal.desktop-channel-settings-view"
					/>
				)}
			</SettingsModalContainer>
		</Modal.Root>
	);
});
