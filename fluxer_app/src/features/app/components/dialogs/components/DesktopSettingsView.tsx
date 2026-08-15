// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import '@app/features/app/components/dialogs/components/SettingsSearchHighlight.css';
import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {AllSettingsRenderer} from '@app/features/app/components/dialogs/components/AllSettingsRenderer';
import {ClientInfo} from '@app/features/app/components/dialogs/components/ClientInfo';
import styles from '@app/features/app/components/dialogs/components/DesktopSettingsView.module.css';
import {LogoutModal} from '@app/features/app/components/dialogs/components/LogoutModal';
import {SettingsModalHeader} from '@app/features/app/components/dialogs/components/SettingsModalHeader';
import {SettingsSearch} from '@app/features/app/components/dialogs/components/SettingsSearch';
import {SettingsCurrentTabProvider} from '@app/features/app/components/dialogs/shared/SettingsCurrentTabContext';
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
	SettingsTreeProvider,
} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import {hasWhatsNewEntries} from '@app/features/app/components/whats_new/WhatsNewEntries';
import {openWhatsNewModal} from '@app/features/app/components/whats_new/WhatsNewModal';
import {
	BACK_TO_SETTINGS_DESCRIPTOR,
	SEARCH_SETTINGS_PLACEHOLDER_DESCRIPTOR,
	SIGN_OUT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import {
	ADVANCED_SETTINGS_TAG_LABELS,
	ADVANCED_SETTINGS_TAG_ORDER,
	getAdvancedSettingsCategory,
	getAdvancedSettingsCategorySectionId,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsCategories';
import ApplicationsTabState from '@app/features/user/components/modals/tabs/applications_tab/ApplicationsTabState';
import userSettingsStyles from '@app/features/user/components/modals/UserSettingsModal.module.css';
import {getSettingsTabComponent} from '@app/features/user/components/settings_utils/DesktopSettingsTabs';
import type {SettingsTab} from '@app/features/user/components/settings_utils/SettingsConstants';
import {
	getCategoryLabel,
	getSectionIdsForTab,
	getSectionsForTab,
	getUserSettingsTabLabel,
	tabHasMultipleLinkableSections,
	USER_SETTINGS_LABEL_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/SettingsConstants';
import {
	isSettingsItemNew,
	type SettingsCategoryTag,
} from '@app/features/user/components/settings_utils/SettingsMetadata';
import {
	PRIMARY_SETTINGS_NAV_HIDDEN_TAB_TYPES,
	PROFILE_SETTINGS_TAB,
} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import {getAdvancedSettingItems} from '@app/features/user/components/settings_utils/SettingsSearchIndex';
import type {
	SettingsSectionConfig,
	UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {SettingsStatusBadge} from '@app/features/user/components/settings_utils/SettingsStatusBadge';
import {
	type FilteredSettingsResult,
	filterSettingsTabsByQuery,
} from '@app/features/user/components/settings_utils/SettingsTabFilters';
import {buildUserSettingsDeepLink} from '@app/features/user/components/settings_utils/UserSettingsDeepLinks';
import {useSettingsContentKey} from '@app/features/user/hooks/useSettingsContentKey';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import {ScrollSpyProvider, useScrollSpyContext} from '@app/features/user/state/ScrollSpyContext';
import Users from '@app/features/user/state/Users';
import {
	collapseSettingsTreeTab,
	expandSettingsTreeTab,
	type SettingsTreeApi,
	syncSettingsTreeExpansionToActiveTab,
	toggleSettingsTreeTab,
} from '@app/features/user/utils/SettingsModalLayoutUtils';
import type {I18n} from '@lingui/core';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowRightIcon, CaretRightIcon, MegaphoneIcon, SignOutIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const BACK_TO_OVERRIDES_DESCRIPTOR = msg({
	message: 'Back to overrides',
	comment: 'Back-navigation label that returns from permission overrides to the main settings pane.',
});
const WHAT_S_NEW_DESCRIPTOR = msg({
	message: "What's new",
	comment: 'Sidebar item in settings that opens the app update notes modal.',
});
const SEARCH_RESULTS_DESCRIPTOR = msg({
	message: 'Search results',
	comment: 'Settings content title shown above results for a settings search query.',
});
const APPLICATION_DETAILS_DESCRIPTOR = msg({
	message: 'Application details',
	comment: 'Settings content title shown while a developer application detail page is loading.',
});

interface DesktopSettingsViewProps {
	groupedSettingsTabs: Record<string, Array<SettingsTab>>;
	currentTab: SettingsTab | undefined;
	selectedTab: UserSettingsTabType | null;
	onTabSelect: (tab: UserSettingsTabType) => void;
	initialGuildId?: string;
	initialSubtab?: string;
	pendingSection?: string | null;
	onPendingSectionConsumed?: () => void;
}

const PendingSectionScroller: React.FC<{section: string | null; onConsumed: () => void}> = ({section, onConsumed}) => {
	const ctx = useScrollSpyContext();
	useEffect(() => {
		if (!section || !ctx) return;
		const frame = window.requestAnimationFrame(() => {
			if (ctx.scrollToSection(section)) {
				onConsumed();
			}
		});
		return () => window.cancelAnimationFrame(frame);
	}, [section, ctx, onConsumed]);
	return null;
};

const getSectionsGroupId = (tabType: UserSettingsTabType): string => `settings-tab-sections-${tabType}`;

function filterPrimarySidebarTabs(groupedTabs: Record<string, Array<SettingsTab>>): Record<string, Array<SettingsTab>> {
	const filtered: Record<string, Array<SettingsTab>> = {};
	for (const [category, tabs] of Object.entries(groupedTabs)) {
		const visibleTabs = tabs.filter((tab) => !PRIMARY_SETTINGS_NAV_HIDDEN_TAB_TYPES.has(tab.type));
		if (visibleTabs.length > 0) {
			filtered[category] = visibleTabs;
		}
	}
	return filtered;
}

function getAdvancedSettingsSections(i18n: I18n, currentUserCreatedAt: Date | null): Array<SettingsSectionConfig> {
	const presentTags = new Set<SettingsCategoryTag>();
	for (const item of getAdvancedSettingItems()) {
		if (isSettingsItemNew(item, Date.now(), currentUserCreatedAt)) {
			continue;
		}
		presentTags.add(getAdvancedSettingsCategory(item));
	}
	return ADVANCED_SETTINGS_TAG_ORDER.filter((tag) => presentTags.has(tag)).map((tag) => ({
		id: getAdvancedSettingsCategorySectionId(tag),
		label: i18n._(ADVANCED_SETTINGS_TAG_LABELS[tag]),
		isAdvanced: false,
	}));
}

function getAdvancedSettingsSectionIds(currentUserCreatedAt: Date | null): Array<string> {
	const presentTags = new Set<SettingsCategoryTag>();
	for (const item of getAdvancedSettingItems()) {
		if (isSettingsItemNew(item, Date.now(), currentUserCreatedAt)) {
			continue;
		}
		presentTags.add(getAdvancedSettingsCategory(item));
	}
	return ADVANCED_SETTINGS_TAG_ORDER.filter((tag) => presentTags.has(tag)).map(getAdvancedSettingsCategorySectionId);
}

function getSidebarSectionsForTab(
	tabType: UserSettingsTabType,
	i18n: I18n,
	currentUserCreatedAt: Date | null,
): Array<SettingsSectionConfig> {
	if (tabType === 'advanced_settings') {
		return getAdvancedSettingsSections(i18n, currentUserCreatedAt);
	}
	return getSectionsForTab(tabType, i18n);
}

function getSidebarSectionIdsForTab(tabType: UserSettingsTabType, currentUserCreatedAt: Date | null): Array<string> {
	if (tabType === 'advanced_settings') {
		return getAdvancedSettingsSectionIds(currentUserCreatedAt);
	}
	return getSectionIdsForTab(tabType);
}

function sidebarTabHasSections(tabType: UserSettingsTabType, currentUserCreatedAt: Date | null): boolean {
	if (tabType === 'advanced_settings') {
		return getAdvancedSettingsSectionIds(currentUserCreatedAt).length > 1;
	}
	return tabHasMultipleLinkableSections(tabType);
}

interface SidebarSectionsProps {
	tabType: UserSettingsTabType;
	tabId: string;
	isSelectedTab: boolean;
	expanded: boolean;
	currentUserCreatedAt: Date | null;
}

const SidebarSections: React.FC<SidebarSectionsProps> = observer(
	({tabType, tabId, isSelectedTab, expanded, currentUserCreatedAt}) => {
		const {i18n} = useLingui();
		const scrollSpyContext = useScrollSpyContext();
		const sections = getSidebarSectionsForTab(tabType, i18n, currentUserCreatedAt);
		if (sections.length === 0) {
			return null;
		}
		const activeSectionId = isSelectedTab ? (scrollSpyContext?.activeSectionId ?? null) : null;
		const handleSectionClick = (sectionId: string) => {
			if (isSelectedTab && scrollSpyContext) {
				scrollSpyContext.scrollToSection(sectionId);
				return;
			}
			ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {tab: tabType, section: sectionId});
		};
		return (
			<SettingsModalSidebarSubItems
				expanded={expanded}
				groupId={getSectionsGroupId(tabType)}
				labelledBy={tabId}
				data-flx="app.desktop-settings-view.sidebar-sections.settings-modal-sidebar-sub-items"
			>
				{sections.map((section: SettingsSectionConfig) => (
					<SettingsModalSidebarSubItem
						key={section.id}
						label={
							<span
								className={styles.sectionLabelWithBadge}
								data-flx="app.desktop-settings-view.section-label-with-badge"
							>
								<span data-flx="app.desktop-settings-view.section-label">{section.label}</span>
								{isSettingsItemNew(section, Date.now(), currentUserCreatedAt) && (
									<SettingsStatusBadge kind="new" data-flx="app.desktop-settings-view.section-new-badge" />
								)}
							</span>
						}
						sectionId={section.id}
						isActive={activeSectionId === section.id}
						onClick={() => handleSectionClick(section.id)}
						data-flx="app.desktop-settings-view.sidebar-sections.settings-modal-sidebar-sub-item.scroll-to-section"
					/>
				))}
			</SettingsModalSidebarSubItems>
		);
	},
);

interface BreadcrumbTitleProps {
	parentLabel: React.ReactNode;
	currentLabel: React.ReactNode;
	onParentClick: () => void;
	parentDataFlx: string;
}

const BreadcrumbTitle: React.FC<BreadcrumbTitleProps> = ({parentLabel, currentLabel, onParentClick, parentDataFlx}) => (
	<span className={styles.breadcrumbTitle} data-flx="app.desktop-settings-view.breadcrumb-title">
		<button type="button" className={styles.breadcrumbButton} onClick={onParentClick} data-flx={parentDataFlx}>
			{parentLabel}
		</button>
		<CaretRightIcon
			size={remFromPx(16)}
			weight="bold"
			className={styles.breadcrumbChevron}
			aria-hidden="true"
			data-flx="app.desktop-settings-view.breadcrumb-chevron"
		/>
		<span className={styles.breadcrumbCurrent} data-flx="app.desktop-settings-view.breadcrumb-current">
			{currentLabel}
		</span>
	</span>
);
export const DesktopSettingsView: React.FC<DesktopSettingsViewProps> = observer(
	({
		groupedSettingsTabs,
		currentTab,
		selectedTab,
		onTabSelect,
		initialGuildId,
		initialSubtab,
		pendingSection,
		onPendingSectionConsumed,
	}) => {
		const {i18n} = useLingui();
		const currentUser = Users.currentUser;
		const currentUserCreatedAt = currentUser?.createdAt ?? null;
		const prefersReducedMotion = Accessibility.useReducedMotion;
		const contentRef = useRef<HTMLDivElement>(null);
		const [scrollContainer, setScrollContainer] = useState<HTMLElement | null>(null);
		const focusContentPanel = useCallback(() => {
			contentRef.current?.focus();
		}, []);
		const {contentKey} = useSettingsContentKey();
		const sectionIds = useMemo(
			() => (selectedTab ? getSidebarSectionIdsForTab(selectedTab, currentUserCreatedAt) : []),
			[selectedTab, currentUserCreatedAt],
		);
		const hasSections = selectedTab ? sidebarTabHasSections(selectedTab, currentUserCreatedAt) : false;
		const activeExpandableTab = hasSections ? selectedTab : null;
		const [expandedTab, setExpandedTab] = useState<string | null>(() => activeExpandableTab);
		useLayoutEffect(() => {
			setExpandedTab((prev) => syncSettingsTreeExpansionToActiveTab(prev, activeExpandableTab));
		}, [activeExpandableTab]);
		const treeApi = useMemo<SettingsTreeApi>(
			() => ({
				isExpanded: (tabId) => expandedTab === tabId,
				expand: (tabId) => setExpandedTab((prev) => expandSettingsTreeTab(prev, tabId)),
				collapse: (tabId) => setExpandedTab((prev) => collapseSettingsTreeTab(prev, tabId)),
				toggle: (tabId) => setExpandedTab((prev) => toggleSettingsTreeTab(prev, tabId)),
			}),
			[expandedTab],
		);
		const [initialPendingSection, setInitialPendingSection] = useState<string | null>(() => initialSubtab ?? null);
		const [searchQuery, setSearchQuery] = useState('');
		const [debouncedSearchQuery, setDebouncedSearchQuery] = useState('');
		useEffect(() => {
			setInitialPendingSection(initialSubtab ?? null);
		}, [initialSubtab]);
		useEffect(() => {
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
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(
			selectedTab ?? undefined,
		);
		const handleLogout = () => {
			if (checkUnsavedChanges()) return;
			ModalCommands.push(modal(() => <LogoutModal data-flx="app.desktop-settings-view.handle-logout.logout-modal" />));
		};
		const handleTabSelect = (tabType: UserSettingsTabType) => {
			if (checkUnsavedChanges()) return;
			onTabSelect(tabType);
			setSearchQuery('');
		};
		const handleApplicationsBreadcrumbClick = useCallback(() => {
			if (checkUnsavedChanges()) return;
			void ApplicationsTabState.navigateToList();
		}, [checkUnsavedChanges]);
		const handleClose = () => {
			if (checkUnsavedChanges()) return;
			ModalCommands.pop();
		};
		const pendingScrollSection = pendingSection ?? initialPendingSection;
		const handlePendingSectionConsumed = useCallback(() => {
			if (pendingSection) {
				onPendingSectionConsumed?.();
			}
			if (initialPendingSection) {
				setInitialPendingSection(null);
			}
		}, [initialPendingSection, onPendingSectionConsumed, pendingSection]);
		const filterResult: FilteredSettingsResult = useMemo(() => {
			if (!isSearchActive) {
				return {groupedTabs: groupedSettingsTabs, searchResults: []};
			}
			return filterSettingsTabsByQuery(groupedSettingsTabs, debouncedSearchQuery);
		}, [groupedSettingsTabs, isSearchActive, debouncedSearchQuery]);
		const filteredGroupedTabs = filterResult.groupedTabs;
		const sidebarGroupedTabs = useMemo(() => filterPrimarySidebarTabs(filteredGroupedTabs), [filteredGroupedTabs]);
		const searchResults = filterResult.searchResults;
		const useOverride = SettingsSidebar.useOverride;
		const activeTabPanelId = selectedTab ? `settings-tabpanel-${selectedTab}` : undefined;
		const activeTabId = selectedTab ? `settings-tab-${selectedTab}` : undefined;
		const shouldShowWhatsNew = hasWhatsNewEntries();
		const scrollKey = useMemo(() => {
			const subtabKey = contentKey ?? initialSubtab ?? 'root';
			if (isSearchActive) {
				return `user-settings-search-${subtabKey}`;
			}
			const tabKey = selectedTab ?? 'settings';
			return `user-settings-${tabKey}-${subtabKey}`;
		}, [contentKey, initialSubtab, isSearchActive, selectedTab]);
		const activeTabComponent = currentTab ? getSettingsTabComponent(currentTab.type) : null;
		const isApplicationsDetailTitle =
			selectedTab === 'applications' &&
			ApplicationsTabState.isDetailView &&
			ApplicationsTabState.selectedAppId !== null;
		const pageLinkHref =
			selectedTab && !isSearchActive && !isApplicationsDetailTitle ? buildUserSettingsDeepLink(selectedTab) : null;
		const applicationsTabLabel = getUserSettingsTabLabel(i18n, 'applications');
		const applicationDetailTitle =
			ApplicationsTabState.selectedApplication?.name ?? i18n._(APPLICATION_DETAILS_DESCRIPTOR);
		const desktopTitle = isSearchActive ? (
			i18n._(SEARCH_RESULTS_DESCRIPTOR)
		) : isApplicationsDetailTitle ? (
			<BreadcrumbTitle
				parentLabel={applicationsTabLabel}
				currentLabel={applicationDetailTitle}
				onParentClick={handleApplicationsBreadcrumbClick}
				parentDataFlx="app.desktop-settings-view.breadcrumb-button.applications"
				data-flx="app.desktop-settings-view.breadcrumb-title"
			/>
		) : (
			currentTab?.label || i18n._(USER_SETTINGS_LABEL_DESCRIPTOR)
		);
		const sidebarFooter = (
			<AnimatePresence data-flx="app.desktop-settings-view.animate-presence">
				{!useOverride && (
					<motion.div
						initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
						animate={{opacity: 1}}
						exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
						transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
						data-flx="app.desktop-settings-view.div"
					>
						<SettingsModalSidebarFooter data-flx="app.desktop-settings-view.settings-modal-sidebar-footer">
							<div className={styles.footerContent} data-flx="app.desktop-settings-view.footer-content">
								<ClientInfo data-flx="app.desktop-settings-view.client-info" />
								<div className={styles.legalLinks} data-flx="app.desktop-settings-view.legal-links">
									<a
										href={Routes.terms()}
										target="_blank"
										rel="noopener noreferrer"
										className={styles.legalLink}
										data-flx="app.desktop-settings-view.legal-link"
									>
										<Trans>Terms of service</Trans>
									</a>
									<a
										href={Routes.privacy()}
										target="_blank"
										rel="noopener noreferrer"
										className={styles.legalLink}
										data-flx="app.desktop-settings-view.legal-link--2"
									>
										<Trans>Privacy policy</Trans>
									</a>
								</div>
								<div className={styles.footerSpacer} data-flx="app.desktop-settings-view.footer-spacer" />
							</div>
						</SettingsModalSidebarFooter>
					</motion.div>
				)}
			</AnimatePresence>
		);
		const sidebarHeader = (
			<>
				<div className={styles.searchContainer} data-flx="app.desktop-settings-view.search-container">
					<SettingsSearch
						value={searchQuery}
						onChange={setSearchQuery}
						placeholder={i18n._(SEARCH_SETTINGS_PLACEHOLDER_DESCRIPTOR)}
						className={styles.fullWidth}
						data-flx="app.desktop-settings-view.full-width.set-search-query"
					/>
				</div>
				<button
					type="button"
					className={clsx(
						userSettingsStyles.userProfile,
						selectedTab === PROFILE_SETTINGS_TAB && userSettingsStyles.userProfileSelected,
					)}
					onClick={() => handleTabSelect(PROFILE_SETTINGS_TAB)}
					data-flx="app.desktop-settings-view.user-profile.profile-tab-select"
				>
					<div className={userSettingsStyles.userProfileInfo} data-flx="app.desktop-settings-view.div--3">
						<StatusAwareAvatar size={32} user={currentUser!} data-flx="app.desktop-settings-view.status-aware-avatar" />
						<div className={userSettingsStyles.userProfileName} data-flx="app.desktop-settings-view.div--4">
							{currentUser?.displayName}
						</div>
					</div>
				</button>
			</>
		);
		const content = (
			<>
				<SettingsModalDesktopSidebar data-flx="app.desktop-settings-view.settings-modal-desktop-sidebar">
					<SettingsModalSidebarNav
						header={sidebarHeader}
						footer={sidebarFooter}
						data-flx="app.desktop-settings-view.settings-modal-sidebar-nav"
					>
						<AnimatePresence initial={false} data-flx="app.desktop-settings-view.animate-presence--2">
							{SettingsSidebar.hasOverride && useOverride ? (
								<motion.div
									key="custom"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
									data-flx="app.desktop-settings-view.div--5"
								>
									<div
										className={styles.backButtonContainer}
										data-flx="app.desktop-settings-view.back-button-container"
									>
										<Button
											variant="secondary"
											leftIcon={
												<ArrowLeftIcon className={styles.backIcon} data-flx="app.desktop-settings-view.back-icon" />
											}
											onClick={() => SettingsSidebar.setUseOverride(false)}
											data-flx="app.desktop-settings-view.button.set-use-override"
										>
											{i18n._(BACK_TO_SETTINGS_DESCRIPTOR)}
										</Button>
									</div>
									{SettingsSidebar.overrideContent}
								</motion.div>
							) : (
								<motion.div
									key="global"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
									data-flx="app.desktop-settings-view.div--6"
								>
									{SettingsSidebar.hasOverride && (
										<div
											className={styles.backButtonContainer}
											data-flx="app.desktop-settings-view.back-button-container--2"
										>
											<Button
												variant="secondary"
												rightIcon={
													<ArrowRightIcon
														className={styles.backIcon}
														data-flx="app.desktop-settings-view.back-icon--2"
													/>
												}
												onClick={() => SettingsSidebar.setUseOverride(true)}
												data-flx="app.desktop-settings-view.button.set-use-override--2"
											>
												{i18n._(BACK_TO_OVERRIDES_DESCRIPTOR)}
											</Button>
										</div>
									)}
									{isSearchActive && Object.keys(filteredGroupedTabs).length === 0 ? (
										<div className={styles.noResults} data-flx="app.desktop-settings-view.no-results">
											<Trans>No settings found</Trans>
										</div>
									) : (
										<>
											{Object.entries(sidebarGroupedTabs).map(([category, tabs]) => (
												<SettingsModalSidebarCategory
													key={category}
													data-flx="app.desktop-settings-view.settings-modal-sidebar-category"
												>
													<SettingsModalSidebarCategoryTitle data-flx="app.desktop-settings-view.settings-modal-sidebar-category-title">
														{getCategoryLabel(category as SettingsTab['category'])}
													</SettingsModalSidebarCategoryTitle>
													{tabs.map((tab) => {
														const tabId = `settings-tab-${tab.type}`;
														const panelId = `settings-tabpanel-${tab.type}`;
														const isSelected = tab.type === selectedTab;
														const hasTabSections = sidebarTabHasSections(tab.type, currentUserCreatedAt);
														return (
															<React.Fragment key={tab.type}>
																<SettingsModalSidebarItem
																	icon={tab.icon}
																	iconWeight={tab.iconWeight}
																	label={
																		<div className={styles.tabLabel} data-flx="app.desktop-settings-view.tab-label">
																			<span data-flx="app.desktop-settings-view.span">{tab.label}</span>
																			{tab.type === 'gift_inventory' && currentUser?.hasUnreadGiftInventory && (
																				<span
																					className={styles.badgeContainer}
																					data-flx="app.desktop-settings-view.badge-container"
																				>
																					<MentionBadgeAnimated
																						mentionCount={currentUser.unreadGiftInventoryCount ?? 1}
																						data-flx="app.desktop-settings-view.mention-badge-animated"
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
																	expandableId={hasTabSections ? tab.type : undefined}
																	sectionsGroupId={hasTabSections ? getSectionsGroupId(tab.type) : undefined}
																	data-flx="app.desktop-settings-view.settings-modal-sidebar-item.tab-select"
																/>
																{hasTabSections && (
																	<SidebarSections
																		tabType={tab.type}
																		tabId={tabId}
																		isSelectedTab={isSelected}
																		expanded={expandedTab === tab.type}
																		currentUserCreatedAt={currentUserCreatedAt}
																		data-flx="app.desktop-settings-view.sidebar-sections"
																	/>
																)}
															</React.Fragment>
														);
													})}
												</SettingsModalSidebarCategory>
											))}
											{shouldShowWhatsNew && (
												<SettingsModalSidebarItem
													icon={MegaphoneIcon}
													label={i18n._(WHAT_S_NEW_DESCRIPTOR)}
													onClick={openWhatsNewModal}
													data-flx="app.desktop-settings-view.settings-modal-sidebar-item.open-whats-new-modal"
												/>
											)}
											<SettingsModalSidebarItem
												icon={SignOutIcon}
												label={i18n._(SIGN_OUT_DESCRIPTOR)}
												danger={true}
												onClick={handleLogout}
												data-flx="app.desktop-settings-view.settings-modal-sidebar-item.logout"
											/>
										</>
									)}
								</motion.div>
							)}
						</AnimatePresence>
					</SettingsModalSidebarNav>
				</SettingsModalDesktopSidebar>
				<SettingsModalDesktopContent
					ref={contentRef}
					tabpanelId={activeTabPanelId}
					labelledBy={activeTabId}
					data-flx="app.desktop-settings-view.settings-modal-desktop-content"
				>
					<SettingsModalHeader
						title={desktopTitle}
						pageLinkHref={pageLinkHref}
						showUnsavedBanner={showUnsavedBanner}
						flashBanner={flashBanner}
						tabData={tabData}
						onClose={handleClose}
						data-flx="app.desktop-settings-view.settings-modal-header"
					/>
					<SettingsModalDesktopScroll
						scrollKey={scrollKey}
						scrollerRef={setScrollContainer}
						data-flx="app.desktop-settings-view.settings-modal-desktop-scroll"
					>
						{isSearchActive ? (
							<AllSettingsRenderer
								searchQuery={debouncedSearchQuery}
								searchResults={searchResults}
								groupedSettingsTabs={filteredGroupedTabs}
								initialGuildId={initialGuildId}
								data-flx="app.desktop-settings-view.all-settings-renderer"
							/>
						) : (
							currentTab &&
							activeTabComponent &&
							(() => {
								const tabType = selectedTab ?? currentTab.type;
								return (
									<SettingsCurrentTabProvider
										value={tabType}
										data-flx="app.desktop-settings-view.settings-current-tab-provider"
									>
										{React.createElement(activeTabComponent, {
											settingsTabType: tabType,
											...(initialGuildId ? {initialGuildId} : {}),
											...(initialSubtab ? {initialSubtab} : {}),
										} as Record<string, unknown>)}
									</SettingsCurrentTabProvider>
								);
							})()
						)}
					</SettingsModalDesktopScroll>
				</SettingsModalDesktopContent>
			</>
		);
		const scrollSpySectionIds = useMemo(
			() => (hasSections && !isSearchActive ? sectionIds : []),
			[hasSections, isSearchActive, sectionIds],
		);
		return (
			<ScrollSpyProvider
				sectionIds={scrollSpySectionIds}
				container={scrollContainer}
				data-flx="app.desktop-settings-view.scroll-spy-provider"
			>
				<SettingsTreeProvider value={treeApi} data-flx="app.desktop-settings-view.settings-tree-provider">
					{content}
				</SettingsTreeProvider>
				<PendingSectionScroller
					section={pendingScrollSection}
					onConsumed={handlePendingSectionConsumed}
					data-flx="app.desktop-settings-view.pending-section-scroller"
				/>
			</ScrollSpyProvider>
		);
	},
);
