// SPDX-License-Identifier: AGPL-3.0-or-later

import {DesktopSettingsView} from '@app/features/app/components/dialogs/components/DesktopSettingsView';
import {MobileSettingsView} from '@app/features/app/components/dialogs/components/MobileSettingsView';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SettingsModalContainer} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import UnsavedChanges from '@app/features/ui/state/UnsavedChanges';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {
	getSettingsTabs,
	USER_SETTINGS_LABEL_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/SettingsConstants';
import {
	ACCOUNT_SETTINGS_TAB,
	getAccountSectionForLegacySection,
	getAccountSectionForNestedTab,
} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import type {UserSettingsTabType} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {useMobileNavigation} from '@app/features/user/hooks/useMobileNavigation';
import {SettingsContentKeyProvider} from '@app/features/user/hooks/useSettingsContentKey';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

interface UserSettingsModalProps {
	initialTab?: UserSettingsTabType;
	initialSubtab?: string;
	initialGuildId?: string;
}

export const UserSettingsModal: React.FC<UserSettingsModalProps> = observer(
	({initialTab, initialSubtab, initialGuildId}) => {
		const {i18n} = useLingui();
		const isMobileExperience = isMobileExperienceEnabled();
		const settingsTabs = useMemo(() => getSettingsTabs(i18n), [i18n.locale]);
		const initialAccountSection =
			getAccountSectionForNestedTab(initialTab) ?? getAccountSectionForLegacySection(initialSubtab);
		const normalizedInitialTab = initialAccountSection ? ACCOUNT_SETTINGS_TAB : initialTab;
		const normalizedInitialSubtab = initialAccountSection ?? initialSubtab;
		const isStaff = Users.getCurrentUser()?.isStaff() ?? false;
		const hasExpressionPackAccess = isStaff && DeveloperOptions.showExpressionPacksSettings;
		const isDeveloperModeEnabled = UserSettings.developerMode;
		const visibleSettingsTabs = useMemo(() => {
			return settingsTabs.filter((tab) => {
				if (!hasExpressionPackAccess && tab.type === 'expression_packs') {
					return false;
				}
				if (!isDeveloperModeEnabled && (tab.type === 'embed_debugger' || tab.type === 'component_gallery')) {
					return false;
				}
				return true;
			});
		}, [hasExpressionPackAccess, isDeveloperModeEnabled, settingsTabs]);
		const resolveVisibleTab = useCallback(
			(tabType?: UserSettingsTabType): UserSettingsTabType => {
				if (tabType && visibleSettingsTabs.some((tab) => tab.type === tabType)) {
					return tabType;
				}
				return visibleSettingsTabs[0]?.type ?? 'account_security';
			},
			[visibleSettingsTabs],
		);
		const [selectedTab, setSelectedTab] = useState<UserSettingsTabType>(() => resolveVisibleTab(normalizedInitialTab));
		const [pendingSection, setPendingSection] = useState<string | null>(null);
		const consumePendingSection = useCallback(() => setPendingSection(null), []);
		const mobileInitialTab = useMemo(() => {
			if (!isMobileExperience) return;
			if (!normalizedInitialTab) return;
			const resolvedTab = resolveVisibleTab(normalizedInitialTab);
			const targetTab = visibleSettingsTabs.find((tab) => tab.type === resolvedTab);
			if (!targetTab) return;
			return {tab: targetTab.type, title: targetTab.label};
		}, [isMobileExperience, normalizedInitialTab, resolveVisibleTab, visibleSettingsTabs]);
		const mobileNav = useMobileNavigation(mobileInitialTab);
		const {enabled: isMobile} = MobileLayout;
		const unsavedChangesState = UnsavedChanges;
		const groupedSettingsTabs = useMemo(() => {
			return visibleSettingsTabs.reduce(
				(acc, tab) => {
					if (!acc[tab.category]) {
						acc[tab.category] = [];
					}
					acc[tab.category].push(tab);
					return acc;
				},
				{} as Record<string, Array<(typeof settingsTabs)[number]>>,
			);
		}, [visibleSettingsTabs]);
		const currentTab = useMemo(() => {
			if (!isMobile) {
				return visibleSettingsTabs.find((tab) => tab.type === selectedTab);
			}
			if (mobileNav.isRootView) return;
			return visibleSettingsTabs.find((tab) => tab.type === mobileNav.currentView?.tab);
		}, [isMobile, selectedTab, mobileNav.isRootView, mobileNav.currentView, visibleSettingsTabs]);
		const handleMobileBack = useCallback(() => {
			const checkTabId = currentTab?.type;
			if (checkTabId && unsavedChangesState.unsavedChanges[checkTabId]) {
				UnsavedChangesCommands.triggerFlashEffect(checkTabId);
				return;
			}
			if (mobileNav.isRootView) {
				ModalCommands.pop();
			} else {
				mobileNav.navigateBack();
			}
		}, [currentTab?.type, mobileNav, unsavedChangesState.unsavedChanges]);
		const handleTabSelect = useCallback(
			(tabType: string, title: string) => {
				mobileNav.navigateTo(tabType as UserSettingsTabType, title);
			},
			[mobileNav],
		);
		const handleClose = useCallback(() => {
			const checkTabId = selectedTab;
			if (checkTabId && unsavedChangesState.unsavedChanges[checkTabId]) {
				UnsavedChangesCommands.triggerFlashEffect(checkTabId);
				return;
			}
			ModalCommands.pop();
		}, [selectedTab, unsavedChangesState.unsavedChanges]);
		const handleModalClose = useCallback(() => {
			if (isMobile) {
				handleMobileBack();
				return;
			}
			handleClose();
		}, [handleClose, handleMobileBack, isMobile]);
		useEffect(() => {
			const resolvedTab = resolveVisibleTab(selectedTab);
			if (resolvedTab !== selectedTab) {
				setSelectedTab(resolvedTab);
			}
		}, [resolveVisibleTab, selectedTab]);
		useEffect(() => {
			const unsubscribe = ComponentBus.subscribe('USER_SETTINGS_TAB_SELECT', (args?: unknown) => {
				const {tab, section} = (args ?? {}) as {tab?: string; section?: string};
				if (tab && typeof tab === 'string') {
					const accountSection =
						getAccountSectionForNestedTab(tab as UserSettingsTabType) ?? getAccountSectionForLegacySection(section);
					const requestedTab = accountSection ? ACCOUNT_SETTINGS_TAB : (tab as UserSettingsTabType);
					const resolvedTab = resolveVisibleTab(requestedTab);
					if (isMobile) {
						const targetTab = visibleSettingsTabs.find((t) => t.type === resolvedTab);
						if (targetTab) {
							mobileNav.navigateTo(targetTab.type, targetTab.label);
						}
					} else {
						setSelectedTab(resolvedTab);
					}
					const requestedSection = accountSection ?? section;
					if (requestedSection && typeof requestedSection === 'string') {
						setPendingSection(requestedSection);
					}
				}
			});
			return unsubscribe;
		}, [isMobile, mobileNav, resolveVisibleTab, visibleSettingsTabs]);
		return (
			<SettingsContentKeyProvider data-flx="user.user-settings-modal.settings-content-key-provider">
				<Modal.Root size="fullscreen" onClose={handleModalClose} data-flx="user.user-settings-modal.modal-root">
					<Modal.ScreenReaderLabel
						text={i18n._(USER_SETTINGS_LABEL_DESCRIPTOR)}
						data-flx="user.user-settings-modal.modal-screen-reader-label"
					/>
					<SettingsModalContainer fullscreen={true} data-flx="user.user-settings-modal.settings-modal-container">
						{isMobile ? (
							<MobileSettingsView
								groupedSettingsTabs={groupedSettingsTabs}
								currentTab={currentTab}
								mobileNav={mobileNav}
								onBack={handleMobileBack}
								onTabSelect={handleTabSelect}
								initialGuildId={initialGuildId}
								initialSubtab={normalizedInitialSubtab}
								pendingSection={pendingSection}
								onPendingSectionConsumed={consumePendingSection}
								data-flx="user.user-settings-modal.mobile-settings-view"
							/>
						) : (
							<DesktopSettingsView
								groupedSettingsTabs={groupedSettingsTabs}
								currentTab={currentTab}
								selectedTab={selectedTab}
								onTabSelect={setSelectedTab}
								initialGuildId={initialGuildId}
								initialSubtab={normalizedInitialSubtab}
								pendingSection={pendingSection}
								onPendingSectionConsumed={consumePendingSection}
								data-flx="user.user-settings-modal.desktop-settings-view"
							/>
						)}
					</SettingsModalContainer>
				</Modal.Root>
			</SettingsContentKeyProvider>
		);
	},
);
