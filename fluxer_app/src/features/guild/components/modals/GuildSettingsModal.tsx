// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {DesktopGuildSettingsView} from '@app/features/app/components/dialogs/components/DesktopGuildSettingsView';
import {MobileGuildSettingsView} from '@app/features/app/components/dialogs/components/MobileGuildSettingsView';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SettingsModalContainer} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import GuildSettingsModalState from '@app/features/guild/state/GuildSettingsModal';
import Guilds from '@app/features/guild/state/Guilds';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import Permission from '@app/features/permissions/state/Permission';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import UnsavedChanges from '@app/features/ui/state/UnsavedChanges';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {
	GUILD_SETTINGS_LABEL_DESCRIPTOR,
	type GuildSettingsTabType,
	getGuildSettingsTabs,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {useMobileNavigation} from '@app/features/user/hooks/useMobileNavigation';
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
		const {i18n} = useLingui();
		const guild = Guilds.getGuild(guildId);
		const [selectedTab, setSelectedTab] = useState<GuildSettingsTabType>(initialTabProp ?? 'overview');
		const availableTabs = useMemo(() => {
			const guildSettingsTabs = getGuildSettingsTabs(i18n);
			if (!guild) return guildSettingsTabs;
			return guildSettingsTabs.filter((tab) => {
				if (tab.permission) {
					const perms = Array.isArray(tab.permission) ? tab.permission : [tab.permission];
					if (!perms.some((p) => Permission.can(p, {guildId}))) {
						return false;
					}
				}
				if (tab.requireFeature && !guild.features.has(tab.requireFeature)) {
					return false;
				}
				return true;
			});
		}, [guild, guildId, i18n.locale]);
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
		const {enabled: isMobile} = MobileLayout;
		const unsavedChangesState = UnsavedChanges;
		const currentMobileTab = mobileNav.currentView?.tab;
		useEffect(() => {
			GatewayConnection.syncGuildIfNeeded(guildId, 'guild-settings-modal');
		}, [guildId]);
		useEffect(() => {
			if (!guild) {
				ModalCommands.popByModalType('guild-settings');
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
				ModalCommands.pop();
			} else {
				mobileNav.navigateBack();
			}
		}, [mobileNav]);
		const handleDesktopTabSelect = useCallback(
			(tabType: GuildSettingsTabType) => {
				if (tabType === 'members') {
					ModalCommands.pop();
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
					ModalCommands.pop();
					RouterUtils.transitionTo(Routes.guildMembers(guildId));
					return;
				}
				mobileNav.navigateTo(tabType as GuildSettingsTabType, title);
			},
			[mobileNav, guildId],
		);
		const handleClose = useCallback(() => {
			const checkTabId = isMobile ? currentMobileTab : selectedTab;
			if (checkTabId && unsavedChangesState.unsavedChanges[checkTabId]) {
				UnsavedChangesCommands.triggerFlashEffect(checkTabId);
				return;
			}
			ModalCommands.pop();
		}, [currentMobileTab, isMobile, selectedTab, unsavedChangesState.unsavedChanges]);
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
			GuildSettingsModalState.register({guildId, navigate: handleExternalNavigate});
			return () => {
				GuildSettingsModalState.unregister(guildId);
			};
		}, [guildId, handleExternalNavigate]);
		if (!guild) {
			return null;
		}
		return (
			<Modal.Root size="fullscreen" onClose={handleClose} data-flx="guild.guild-settings-modal.modal-root">
				<Modal.ScreenReaderLabel
					text={i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR)}
					data-flx="guild.guild-settings-modal.modal-screen-reader-label"
				/>
				<SettingsModalContainer fullscreen={true} data-flx="guild.guild-settings-modal.settings-modal-container">
					{isMobile ? (
						<MobileGuildSettingsView
							guild={guild}
							groupedSettingsTabs={groupedSettingsTabs}
							currentTab={currentTab}
							mobileNav={mobileNav}
							onBack={handleMobileBack}
							onTabSelect={handleTabSelect}
							data-flx="guild.guild-settings-modal.mobile-guild-settings-view"
						/>
					) : (
						<DesktopGuildSettingsView
							guild={guild}
							groupedSettingsTabs={groupedSettingsTabs}
							currentTab={currentTab}
							selectedTab={selectedTab}
							onTabSelect={handleDesktopTabSelect}
							data-flx="guild.guild-settings-modal.desktop-guild-settings-view"
						/>
					)}
				</SettingsModalContainer>
			</Modal.Root>
		);
	},
);
