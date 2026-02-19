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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import * as UnsavedChangesActionCreators from '@app/actions/UnsavedChangesActionCreators';
import {DesktopGuildSettingsView} from '@app/components/modals/components/DesktopGuildSettingsView';
import {MobileGuildSettingsView} from '@app/components/modals/components/MobileGuildSettingsView';
import {useMobileNavigation} from '@app/components/modals/hooks/useMobileNavigation';
import * as Modal from '@app/components/modals/Modal';
import {SettingsModalContainer} from '@app/components/modals/shared/SettingsModalLayout';
import {type GuildSettingsTabType, getGuildSettingsTabs} from '@app/components/modals/utils/GuildSettingsConstants';
import {Routes} from '@app/Routes';
import GuildSettingsModalStore from '@app/stores/GuildSettingsModalStore';
import GuildStore from '@app/stores/GuildStore';
import GatewayConnectionStore from '@app/stores/gateway/GatewayConnectionStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import PermissionStore from '@app/stores/PermissionStore';
import UnsavedChangesStore from '@app/stores/UnsavedChangesStore';
import {isMobileExperienceEnabled} from '@app/utils/MobileExperience';
import * as RouterUtils from '@app/utils/RouterUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

interface GuildSettingsModalProps {
	guildId: string;
	initialTab?: GuildSettingsTabType;
	initialMobileTab?: GuildSettingsTabType;
}

export const GuildSettingsModal: React.FC<GuildSettingsModalProps> = observer(
	({guildId, initialTab: initialTabProp, initialMobileTab}) => {
		const {t, i18n} = useLingui();
		const guild = GuildStore.getGuild(guildId);
		const [selectedTab, setSelectedTab] = useState<GuildSettingsTabType>(initialTabProp ?? 'overview');

		const availableTabs = useMemo(() => {
			const guildSettingsTabs = getGuildSettingsTabs(i18n);
			if (!guild) return guildSettingsTabs;

			return guildSettingsTabs.filter((tab) => {
				if (tab.permission) {
					const perms = Array.isArray(tab.permission) ? tab.permission : [tab.permission];
					if (!perms.some((p) => PermissionStore.can(p, {guildId}))) {
						return false;
					}
				}
				if (tab.requireFeature && !guild.features.has(tab.requireFeature)) {
					return false;
				}
				return true;
			});
		}, [guild, guildId, i18n]);

		const isMobileExperience = isMobileExperienceEnabled();

		const initialMobileTabObject = useMemo(() => {
			if (!isMobileExperience || !initialMobileTab) return;
			const targetTab = availableTabs.find((tab) => tab.type === initialMobileTab);
			if (!targetTab) return;
			return {tab: initialMobileTab, title: targetTab.label};
		}, [initialMobileTab, availableTabs, isMobileExperience]);

		const mobileNav = useMobileNavigation<GuildSettingsTabType>(initialMobileTabObject);
		const mobileNavigateTo = mobileNav.navigateTo;
		const mobileResetToRoot = mobileNav.resetToRoot;
		const mobileIsRootView = mobileNav.isRootView;
		const {enabled: isMobile} = MobileLayoutStore;

		const unsavedChangesStore = UnsavedChangesStore;
		const currentMobileTab = mobileNav.currentView?.tab;

		useEffect(() => {
			GatewayConnectionStore.syncGuildIfNeeded(guildId, 'guild-settings-modal');
		}, [guildId]);

		useEffect(() => {
			if (!guild) {
				ModalActionCreators.popByType(GuildSettingsModal);
			}
		}, [guild]);

		useEffect(() => {
			if (availableTabs.length > 0 && !availableTabs.find((tab) => tab.type === selectedTab)) {
				setSelectedTab(availableTabs[0].type);
			}
		}, [availableTabs, selectedTab]);

		const groupedSettingsTabs = useMemo(() => {
			return availableTabs.reduce(
				(acc, tab) => {
					if (!acc[tab.category]) {
						acc[tab.category] = [];
					}
					acc[tab.category].push(tab);
					return acc;
				},
				{} as Record<string, Array<(typeof availableTabs)[number]>>,
			);
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
				ModalActionCreators.pop();
			} else {
				mobileNav.navigateBack();
			}
		}, [mobileNav]);

		const handleDesktopTabSelect = useCallback(
			(tabType: GuildSettingsTabType) => {
				if (tabType === 'members') {
					ModalActionCreators.pop();
					RouterUtils.transitionTo(Routes.guildMembers(guildId));
					return;
				}
				setSelectedTab(tabType);
			},
			[guildId],
		);

		const handleTabSelect = useCallback(
			(tabType: string, title: string) => {
				if (tabType === 'members') {
					ModalActionCreators.pop();
					RouterUtils.transitionTo(Routes.guildMembers(guildId));
					return;
				}
				mobileNav.navigateTo(tabType as GuildSettingsTabType, title);
			},
			[mobileNav, guildId],
		);

		const handleClose = useCallback(() => {
			const checkTabId = isMobile ? currentMobileTab : selectedTab;
			if (checkTabId && unsavedChangesStore.unsavedChanges[checkTabId]) {
				UnsavedChangesActionCreators.triggerFlashEffect(checkTabId);
				return;
			}
			ModalActionCreators.pop();
		}, [currentMobileTab, isMobile, selectedTab, unsavedChangesStore.unsavedChanges]);

		const handleExternalNavigate = useCallback(
			(targetTab: GuildSettingsTabType) => {
				const tabMeta = availableTabs.find((tab) => tab.type === targetTab);
				if (!tabMeta) return;
				if (isMobile) {
					if (!mobileIsRootView) {
						mobileResetToRoot();
					}
					mobileNavigateTo(tabMeta.type, tabMeta.label);
				} else {
					setSelectedTab(tabMeta.type);
				}
			},
			[availableTabs, isMobile, mobileIsRootView, mobileNavigateTo, mobileResetToRoot],
		);

		useEffect(() => {
			GuildSettingsModalStore.register({guildId, navigate: handleExternalNavigate});
			return () => {
				GuildSettingsModalStore.unregister(guildId);
			};
		}, [guildId, handleExternalNavigate]);

		if (!guild) {
			return null;
		}

		return (
			<Modal.Root size="fullscreen" onClose={handleClose}>
				<Modal.ScreenReaderLabel text={t`Community Settings`} />
				<SettingsModalContainer fullscreen={true}>
					{isMobile ? (
						<MobileGuildSettingsView
							guild={guild}
							groupedSettingsTabs={groupedSettingsTabs}
							currentTab={currentTab}
							mobileNav={mobileNav}
							onBack={handleMobileBack}
							onTabSelect={handleTabSelect}
						/>
					) : (
						<DesktopGuildSettingsView
							guild={guild}
							groupedSettingsTabs={groupedSettingsTabs}
							currentTab={currentTab}
							selectedTab={selectedTab}
							onTabSelect={handleDesktopTabSelect}
						/>
					)}
				</SettingsModalContainer>
			</Modal.Root>
		);
	},
);
