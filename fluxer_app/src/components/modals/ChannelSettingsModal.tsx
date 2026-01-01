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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {ChannelTypes} from '~/Constants';
import * as Modal from '~/components/modals/Modal';
import ChannelStore from '~/stores/ChannelStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import {
	type ChannelSettingsModalProps,
	createHandleClose,
	getAvailableTabs,
	getGroupedSettingsTabs,
} from '~/utils/modals/ChannelSettingsModalUtils';
import {DesktopChannelSettingsView} from './components/DesktopChannelSettingsView';
import {MobileChannelSettingsView} from './components/MobileChannelSettingsView';
import {useMobileNavigation} from './hooks/useMobileNavigation';
import {SettingsModalContainer} from './shared/SettingsModalLayout';
import type {ChannelSettingsTabType} from './utils/channelSettingsConstants';

export const ChannelSettingsModal: React.FC<ChannelSettingsModalProps> = observer(({channelId, initialMobileTab}) => {
	const {t} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const [selectedTab, setSelectedTab] = React.useState<ChannelSettingsTabType>('overview');

	const availableTabs = React.useMemo(() => {
		return getAvailableTabs(t, channelId);
	}, [t, channelId]);

	const isMobileExperience = isMobileExperienceEnabled();

	const initialTab = React.useMemo(() => {
		if (!isMobileExperience || !initialMobileTab) return;
		const targetTab = availableTabs.find((tab) => tab.type === initialMobileTab);
		if (!targetTab) return;
		return {tab: initialMobileTab, title: targetTab.label};
	}, [initialMobileTab, availableTabs, isMobileExperience]);

	const mobileNav = useMobileNavigation<ChannelSettingsTabType>(initialTab);
	const {enabled: isMobile} = MobileLayoutStore;

	React.useEffect(() => {
		if (!channel) {
			ModalActionCreators.pop();
		}
	}, [channel]);

	const groupedSettingsTabs = React.useMemo(() => {
		return getGroupedSettingsTabs(availableTabs);
	}, [availableTabs]);

	const currentTab = React.useMemo(() => {
		if (!isMobile) {
			return availableTabs.find((tab) => tab.type === selectedTab);
		}
		if (mobileNav.isRootView) return;
		return availableTabs.find((tab) => tab.type === mobileNav.currentView?.tab);
	}, [isMobile, selectedTab, mobileNav.isRootView, mobileNav.currentView, availableTabs]);

	const handleMobileBack = React.useCallback(() => {
		if (mobileNav.isRootView) {
			ModalActionCreators.pop();
		} else {
			mobileNav.navigateBack();
		}
	}, [mobileNav]);

	const handleTabSelect = React.useCallback(
		(tabType: string, title: string) => {
			mobileNav.navigateTo(tabType as ChannelSettingsTabType, title);
		},
		[mobileNav],
	);

	const handleClose = React.useCallback(createHandleClose(selectedTab), [selectedTab]);

	if (!channel) {
		return null;
	}

	const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;

	return (
		<Modal.Root size="fullscreen" onClose={handleClose}>
			<Modal.ScreenReaderLabel text={isCategory ? t`Category Settings` : t`Channel Settings`} />
			<SettingsModalContainer fullscreen={true}>
				{isMobile ? (
					<MobileChannelSettingsView
						channel={channel}
						groupedSettingsTabs={groupedSettingsTabs}
						currentTab={currentTab}
						mobileNav={mobileNav}
						onBack={handleMobileBack}
						onTabSelect={handleTabSelect}
					/>
				) : (
					<DesktopChannelSettingsView
						channel={channel}
						groupedSettingsTabs={groupedSettingsTabs}
						currentTab={currentTab}
						selectedTab={selectedTab}
						onTabSelect={setSelectedTab}
					/>
				)}
			</SettingsModalContainer>
		</Modal.Root>
	);
});
