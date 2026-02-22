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
import {DesktopSettingsView} from '@app/components/modals/components/DesktopSettingsView';
import {MobileSettingsView} from '@app/components/modals/components/MobileSettingsView';
import {useMobileNavigation} from '@app/components/modals/hooks/useMobileNavigation';
import {SettingsContentKeyProvider} from '@app/components/modals/hooks/useSettingsContentKey';
import * as Modal from '@app/components/modals/Modal';
import {SettingsModalContainer} from '@app/components/modals/shared/SettingsModalLayout';
import type {UserSettingsSubtabType} from '@app/components/modals/utils/SettingsConstants';
import {getSettingsTabs} from '@app/components/modals/utils/SettingsConstants';
import type {UserSettingsTabType} from '@app/components/modals/utils/SettingsSectionRegistry';
import {ComponentDispatch} from '@app/lib/ComponentDispatch';
import DeveloperModeStore from '@app/stores/DeveloperModeStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import UnsavedChangesStore from '@app/stores/UnsavedChangesStore';
import UserStore from '@app/stores/UserStore';
import {isMobileExperienceEnabled} from '@app/utils/MobileExperience';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

interface UserSettingsModalProps {
	initialTab?: UserSettingsTabType;
	initialSubtab?: UserSettingsSubtabType;
	initialGuildId?: string;
}

export const UserSettingsModal: React.FC<UserSettingsModalProps> = observer(
	({initialTab, initialSubtab, initialGuildId}) => {
		const {t, i18n} = useLingui();
		const [selectedTab, setSelectedTab] = useState<UserSettingsTabType>(initialTab || 'my_profile');

		const isMobileExperience = isMobileExperienceEnabled();

		const settingsTabs = useMemo(() => getSettingsTabs(i18n), [i18n]);

		const mobileInitialTab = useMemo(() => {
			if (!isMobileExperience || !initialTab) return;
			const targetTab = settingsTabs.find((tab) => tab.type === initialTab);
			if (!targetTab) return;
			return {tab: initialTab, title: targetTab.label};
		}, [initialTab, isMobileExperience, settingsTabs]);

		const mobileNav = useMobileNavigation(mobileInitialTab);
		const {enabled: isMobile} = MobileLayoutStore;

		const unsavedChangesStore = UnsavedChangesStore;
		const isDeveloper = DeveloperModeStore.isDeveloper;
		const hasExpressionPackAccess = UserStore.getCurrentUser()?.isStaff() ?? false;

		const groupedSettingsTabs = useMemo(() => {
			const tabsToGroup = settingsTabs.filter((tab) => {
				if (!isDeveloper && tab.category === 'staff_only') {
					return false;
				}
				if (!hasExpressionPackAccess && tab.type === 'expression_packs') {
					return false;
				}
				return true;
			});

			return tabsToGroup.reduce(
				(acc, tab) => {
					if (!acc[tab.category]) {
						acc[tab.category] = [];
					}
					acc[tab.category].push(tab);
					return acc;
				},
				{} as Record<string, Array<(typeof settingsTabs)[number]>>,
			);
		}, [isDeveloper, hasExpressionPackAccess, settingsTabs]);

		const currentTab = useMemo(() => {
			if (!isMobile) {
				return settingsTabs.find((tab) => tab.type === selectedTab);
			}
			if (mobileNav.isRootView) return;
			return settingsTabs.find((tab) => tab.type === mobileNav.currentView?.tab);
		}, [isMobile, selectedTab, mobileNav.isRootView, mobileNav.currentView, settingsTabs]);

		const handleMobileBack = useCallback(() => {
			if (mobileNav.isRootView) {
				ModalActionCreators.pop();
			} else {
				mobileNav.navigateBack();
			}
		}, [mobileNav]);

		const handleTabSelect = useCallback(
			(tabType: string, title: string) => {
				mobileNav.navigateTo(tabType as UserSettingsTabType, title);
			},
			[mobileNav],
		);

		const handleClose = useCallback(() => {
			const checkTabId = selectedTab;
			if (checkTabId && unsavedChangesStore.unsavedChanges[checkTabId]) {
				UnsavedChangesActionCreators.triggerFlashEffect(checkTabId);
				return;
			}
			ModalActionCreators.pop();
		}, [selectedTab, unsavedChangesStore.unsavedChanges]);

		useEffect(() => {
			if (!hasExpressionPackAccess && selectedTab === 'expression_packs') {
				setSelectedTab('my_profile');
			}
		}, [hasExpressionPackAccess, selectedTab]);

		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('USER_SETTINGS_TAB_SELECT', (args?: unknown) => {
				const {tab} = (args ?? {}) as {tab?: string};
				if (tab && typeof tab === 'string') {
					if (isMobile) {
						const targetTab = settingsTabs.find((t) => t.type === tab);
						if (targetTab) {
							mobileNav.navigateTo(tab as UserSettingsTabType, targetTab.label);
						}
					} else {
						setSelectedTab(tab as UserSettingsTabType);
					}
				}
			});

			return unsubscribe;
		}, [isMobile, mobileNav, settingsTabs]);

		return (
			<SettingsContentKeyProvider>
				<Modal.Root size="fullscreen" onClose={handleClose}>
					<Modal.ScreenReaderLabel text={t`User Settings`} />
					<SettingsModalContainer fullscreen={true}>
						{isMobile ? (
							<MobileSettingsView
								groupedSettingsTabs={groupedSettingsTabs}
								currentTab={currentTab}
								mobileNav={mobileNav}
								onBack={handleMobileBack}
								onTabSelect={handleTabSelect}
								initialGuildId={initialGuildId}
								initialSubtab={initialSubtab}
							/>
						) : (
							<DesktopSettingsView
								groupedSettingsTabs={groupedSettingsTabs}
								currentTab={currentTab}
								selectedTab={selectedTab}
								onTabSelect={setSelectedTab}
								initialGuildId={initialGuildId}
								initialSubtab={initialSubtab}
							/>
						)}
					</SettingsModalContainer>
				</Modal.Root>
			</SettingsContentKeyProvider>
		);
	},
);
