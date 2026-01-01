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

import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowRightIcon, SignOutIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {AllSettingsRenderer} from '~/components/modals/components/AllSettingsRenderer';
import {ClientInfo} from '~/components/modals/components/ClientInfo';
import {LogoutModal} from '~/components/modals/components/LogoutModal';
import {SettingsModalHeader} from '~/components/modals/components/SettingsModalHeader';
import {SettingsSearch} from '~/components/modals/components/SettingsSearch';
import {ScrollSpyProvider, useScrollSpyContext} from '~/components/modals/hooks/ScrollSpyContext';
import {useSettingsContentKey} from '~/components/modals/hooks/useSettingsContentKey';
import {useUnsavedChangesFlash} from '~/components/modals/hooks/useUnsavedChangesFlash';
import {
	SettingsModalDesktopContent,
	SettingsModalDesktopScroll,
	SettingsModalDesktopSidebar,
	SettingsModalSidebarCategory,
	SettingsModalSidebarCategoryTitle,
	SettingsModalSidebarFooter,
	SettingsModalSidebarItem,
	SettingsModalSidebarNav,
	SettingsModalSidebarSubItem,
	SettingsModalSidebarSubItems,
	settingsModalStyles,
} from '~/components/modals/shared/SettingsModalLayout';
import {AccessibilityTabPreview} from '~/components/modals/tabs/AccessibilityTab';
import {AppearanceTabPreview} from '~/components/modals/tabs/AppearanceTab';
import userSettingsStyles from '~/components/modals/UserSettingsModal.module.css';
import {getSettingsTabComponent} from '~/components/modals/utils/desktopSettingsTabs';
import {
	getCategoryLabel,
	getSectionIdsForTab,
	getSectionsForTab,
	type SettingsTab,
	tabHasSections,
	type UserSettingsTabType,
} from '~/components/modals/utils/settingsConstants';
import {
	type FilteredSettingsResult,
	filterSettingsTabsByQuery,
	filterSettingsTabsForDeveloperMode,
} from '~/components/modals/utils/settingsTabFilters';
import {Button} from '~/components/uikit/Button/Button';
import {MentionBadgeAnimated} from '~/components/uikit/MentionBadge';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Routes} from '~/Routes';
import AccessibilityStore from '~/stores/AccessibilityStore';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import SettingsSidebarStore from '~/stores/SettingsSidebarStore';
import UserStore from '~/stores/UserStore';
import '~/components/modals/components/SettingsSearchHighlight.css';
import styles from '~/components/modals/components/DesktopSettingsView.module.css';

interface DesktopSettingsViewProps {
	groupedSettingsTabs: Record<string, Array<SettingsTab>>;
	currentTab: SettingsTab | undefined;
	selectedTab: UserSettingsTabType | null;
	onTabSelect: (tab: UserSettingsTabType) => void;
	initialGuildId?: string;
	initialSubtab?: string;
}

interface SidebarSectionsProps {
	tabType: UserSettingsTabType;
}

const SidebarSections: React.FC<SidebarSectionsProps> = observer(({tabType}) => {
	const {t} = useLingui();
	const scrollSpyContext = useScrollSpyContext();
	const sections = getSectionsForTab(tabType, t);

	if (!scrollSpyContext || sections.length === 0) {
		return null;
	}

	const {activeSectionId, scrollToSection} = scrollSpyContext;

	return (
		<SettingsModalSidebarSubItems>
			{sections.map((section) => (
				<SettingsModalSidebarSubItem
					key={section.id}
					label={section.label}
					sectionId={section.id}
					isActive={activeSectionId === section.id}
					onClick={() => scrollToSection(section.id)}
				/>
			))}
		</SettingsModalSidebarSubItems>
	);
});

export const DesktopSettingsView: React.FC<DesktopSettingsViewProps> = observer(
	({groupedSettingsTabs, currentTab, selectedTab, onTabSelect, initialGuildId, initialSubtab}) => {
		const {t} = useLingui();
		const currentUser = UserStore.currentUser;
		const mobileLayout = MobileLayoutStore;
		const prefersReducedMotion = AccessibilityStore.useReducedMotion;
		const contentRef = React.useRef<HTMLDivElement>(null);
		const scrollContainerRef = React.useRef<HTMLElement | null>(null);
		const focusContentPanel = React.useCallback(() => {
			contentRef.current?.focus();
		}, []);
		const {contentKey} = useSettingsContentKey();

		const sectionIds = React.useMemo(() => (selectedTab ? getSectionIdsForTab(selectedTab) : []), [selectedTab]);
		const hasSections = selectedTab ? tabHasSections(selectedTab) : false;

		const [searchQuery, setSearchQuery] = React.useState('');
		const [debouncedSearchQuery, setDebouncedSearchQuery] = React.useState('');

		React.useEffect(() => {
			if (!searchQuery.trim()) {
				setDebouncedSearchQuery('');
				return;
			}

			const timer = setTimeout(() => {
				setDebouncedSearchQuery(searchQuery);
			}, 300);

			return () => clearTimeout(timer);
		}, [searchQuery]);

		const isSearchActive = debouncedSearchQuery.trim().length > 0;

		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(selectedTab);

		const handleLogout = () => {
			if (checkUnsavedChanges()) return;
			ModalActionCreators.push(modal(() => <LogoutModal />));
		};

		const handleTabSelect = (tabType: UserSettingsTabType) => {
			if (checkUnsavedChanges()) return;
			onTabSelect(tabType);
			setSearchQuery('');
		};

		const handleClose = () => {
			if (checkUnsavedChanges()) return;
			ModalActionCreators.pop();
		};

		const isDeveloper = DeveloperModeStore.isDeveloper;

		const filterResult: FilteredSettingsResult = React.useMemo(() => {
			const developerFilteredTabs = filterSettingsTabsForDeveloperMode(groupedSettingsTabs, isDeveloper);

			if (!isSearchActive) {
				return {groupedTabs: developerFilteredTabs, searchResults: []};
			}

			return filterSettingsTabsByQuery(developerFilteredTabs, debouncedSearchQuery);
		}, [groupedSettingsTabs, isDeveloper, isSearchActive, debouncedSearchQuery]);

		const filteredGroupedTabs = filterResult.groupedTabs;
		const searchResults = filterResult.searchResults;

		const useOverride = SettingsSidebarStore.useOverride;
		const activeTabPanelId = selectedTab ? `settings-tabpanel-${selectedTab}` : undefined;
		const activeTabId = selectedTab ? `settings-tab-${selectedTab}` : undefined;
		const hasSelectedTabInSidebar = React.useMemo(() => {
			if (!selectedTab || useOverride) {
				return false;
			}
			return Object.values(filteredGroupedTabs).some((tabs) => tabs.some((tab) => tab.type === selectedTab));
		}, [filteredGroupedTabs, selectedTab, useOverride]);
		const scrollKey = React.useMemo(() => {
			const subtabKey = contentKey ?? initialSubtab ?? 'root';

			if (isSearchActive) {
				return `user-settings-search-${subtabKey}`;
			}

			const tabKey = selectedTab ?? 'settings';
			return `user-settings-${tabKey}-${subtabKey}`;
		}, [contentKey, initialSubtab, isSearchActive, selectedTab]);
		const activeTabComponent = currentTab ? getSettingsTabComponent(currentTab.type) : null;

		const sidebarFooter = (
			<AnimatePresence>
				{!useOverride && (
					<motion.div
						initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
						animate={{opacity: 1}}
						exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
						transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
					>
						<SettingsModalSidebarFooter>
							<div className={styles.footerContent}>
								<ClientInfo />
								<div className={styles.legalLinks}>
									<a href={Routes.terms()} target="_blank" rel="noopener noreferrer" className={styles.legalLink}>
										<Trans>Terms of Service</Trans>
									</a>
									<a href={Routes.privacy()} target="_blank" rel="noopener noreferrer" className={styles.legalLink}>
										<Trans>Privacy Policy</Trans>
									</a>
								</div>
								<div className={styles.footerSpacer} />
							</div>
						</SettingsModalSidebarFooter>
					</motion.div>
				)}
			</AnimatePresence>
		);

		const sidebarHeader = (
			<>
				<div className={styles.searchContainer}>
					<SettingsSearch
						value={searchQuery}
						onChange={setSearchQuery}
						placeholder={t`Search settings...`}
						className={styles.fullWidth}
					/>
				</div>

				<div className={userSettingsStyles.userProfile}>
					<div className={userSettingsStyles.userProfileInfo}>
						<StatusAwareAvatar size={32} user={currentUser!} />
						<div className={userSettingsStyles.userProfileName}>{currentUser?.displayName}</div>
					</div>
				</div>
			</>
		);

		const content = (
			<>
				<SettingsModalDesktopSidebar>
					<SettingsModalSidebarNav
						header={sidebarHeader}
						hasSelectedTabInView={hasSelectedTabInSidebar}
						footer={sidebarFooter}
					>
						<AnimatePresence initial={false}>
							{SettingsSidebarStore.hasOverride && useOverride ? (
								<motion.div
									key="custom"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
								>
									<div className={styles.backButtonContainer}>
										<Button
											variant="secondary"
											leftIcon={<ArrowLeftIcon className={styles.backIcon} />}
											onClick={() => SettingsSidebarStore.setUseOverride(false)}
										>
											{t`Back to Settings`}
										</Button>
									</div>
									{SettingsSidebarStore.overrideContent}
								</motion.div>
							) : (
								<motion.div
									key="global"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
								>
									{SettingsSidebarStore.hasOverride && (
										<div className={styles.backButtonContainer}>
											<Button
												variant="secondary"
												rightIcon={<ArrowRightIcon className={styles.backIcon} />}
												onClick={() => SettingsSidebarStore.setUseOverride(true)}
											>
												{t`Back to Overrides`}
											</Button>
										</div>
									)}
									{isSearchActive && Object.keys(filteredGroupedTabs).length === 0 ? (
										<div className={styles.noResults}>
											<Trans>No settings found</Trans>
										</div>
									) : (
										<>
											{Object.entries(filteredGroupedTabs).map(([category, tabs]) => (
												<SettingsModalSidebarCategory key={category}>
													<SettingsModalSidebarCategoryTitle>
														{getCategoryLabel(category as SettingsTab['category'])}
													</SettingsModalSidebarCategoryTitle>
													{tabs.map((tab) => {
														const tabId = `settings-tab-${tab.type}`;
														const panelId = `settings-tabpanel-${tab.type}`;
														const isSelected = tab.type === selectedTab;
														const showSubItems = isSelected && tabHasSections(tab.type);
														return (
															<React.Fragment key={tab.type}>
																<SettingsModalSidebarItem
																	icon={tab.icon}
																	iconWeight={tab.iconWeight}
																	label={
																		<div className={styles.tabLabel}>
																			<span>{tab.label}</span>
																			{tab.type === 'gift_inventory' && currentUser?.hasUnreadGiftInventory && (
																				<span className={styles.badgeContainer}>
																					<MentionBadgeAnimated
																						mentionCount={currentUser.unreadGiftInventoryCount ?? 1}
																					/>
																				</span>
																			)}
																		</div>
																	}
																	selected={isSelected}
																	onClick={() => handleTabSelect(tab.type)}
																	onRequestContentFocus={focusContentPanel}
																	id={tabId}
																	controlsId={panelId}
																/>
																{showSubItems && <SidebarSections tabType={tab.type} />}
															</React.Fragment>
														);
													})}
												</SettingsModalSidebarCategory>
											))}
											<SettingsModalSidebarItem
												icon={SignOutIcon}
												label={t`Log Out`}
												danger={true}
												onClick={handleLogout}
											/>
										</>
									)}
								</motion.div>
							)}
						</AnimatePresence>
					</SettingsModalSidebarNav>
				</SettingsModalDesktopSidebar>

				<SettingsModalDesktopContent ref={contentRef} tabpanelId={activeTabPanelId} labelledBy={activeTabId}>
					<SettingsModalHeader
						title={isSearchActive ? t`Search Results` : currentTab?.label || t`User Settings`}
						showUnsavedBanner={showUnsavedBanner}
						flashBanner={flashBanner}
						tabData={tabData}
						onClose={handleClose}
					/>

					{!mobileLayout.enabled && selectedTab === 'appearance' && !isSearchActive && (
						<div className={styles.previewDivider}>
							<div className={settingsModalStyles.previewContainer}>
								<AppearanceTabPreview />
							</div>
						</div>
					)}
					{!mobileLayout.enabled && selectedTab === 'accessibility' && !isSearchActive && (
						<div className={styles.previewDivider}>
							<div className={settingsModalStyles.previewContainer}>
								<AccessibilityTabPreview />
							</div>
						</div>
					)}

					<SettingsModalDesktopScroll scrollKey={scrollKey} scrollerRef={scrollContainerRef}>
						{isSearchActive ? (
							<AllSettingsRenderer
								searchQuery={debouncedSearchQuery}
								searchResults={searchResults}
								groupedSettingsTabs={filteredGroupedTabs}
								initialGuildId={initialGuildId}
							/>
						) : (
							currentTab &&
							activeTabComponent &&
							React.createElement(activeTabComponent, {
								...(initialGuildId ? {initialGuildId} : {}),
								...(initialSubtab ? {initialSubtab} : {}),
							} as any)
						)}
					</SettingsModalDesktopScroll>
				</SettingsModalDesktopContent>
			</>
		);

		const scrollSpySectionIds = React.useMemo(
			() => (hasSections && !isSearchActive ? sectionIds : []),
			[hasSections, isSearchActive, sectionIds],
		);

		return (
			<ScrollSpyProvider sectionIds={scrollSpySectionIds} containerRef={scrollContainerRef}>
				{content}
			</ScrollSpyProvider>
		);
	},
);
