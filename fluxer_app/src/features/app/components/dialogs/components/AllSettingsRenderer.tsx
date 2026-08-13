// SPDX-License-Identifier: AGPL-3.0-or-later

import '@app/features/app/components/dialogs/components/SettingsSearchHighlight.css';
import styles from '@app/features/app/components/dialogs/components/AllSettingsRenderer.module.css';
import {
	clearHighlights,
	createRangesForSection,
	setHighlightRanges,
} from '@app/features/messaging/utils/CSSHighlightSearch';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {AccessibilityInlineContent} from '@app/features/user/components/modals/tabs/accessibility_tab/AccessibilityTabInline';
import {AccountSecurityInlineTab} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityTabInline';
import {AppearanceInlineContent} from '@app/features/user/components/modals/tabs/appearance_tab/AppearanceTabInline';
import ApplicationsTab from '@app/features/user/components/modals/tabs/applications_tab';
import {ChatSettingsInlineContent} from '@app/features/user/components/modals/tabs/chat_settings_tab/ChatSettingsTabInline';
import DesktopSettingsTab from '@app/features/user/components/modals/tabs/DesktopSettingsTab';
import ExpressionPacksTab from '@app/features/user/components/modals/tabs/ExpressionPacksTab';
import GiftInventoryTab from '@app/features/user/components/modals/tabs/GiftInventoryTab';
import KeybindsTab from '@app/features/user/components/modals/tabs/KeybindsTab';
import LanguageTab from '@app/features/user/components/modals/tabs/LanguageTab';
import LinkedAccountsTab from '@app/features/user/components/modals/tabs/LinkedAccountsTab';
import MyProfileTab from '@app/features/user/components/modals/tabs/MyProfileTab';
import {NotificationsInlineContent} from '@app/features/user/components/modals/tabs/notifications_tab/NotificationsTabInline';
import PlutoniumTab from '@app/features/user/components/modals/tabs/PlutoniumTab';
import {PrivacyDashboardContent} from '@app/features/user/components/modals/tabs/privacy_safety_tab/PrivacySafetyTabInline';
import {VoiceVideoInlineContent} from '@app/features/user/components/modals/tabs/voice_video_tab/VoiceVideoTabInline';
import {getSettingsTabComponent} from '@app/features/user/components/settings_utils/DesktopSettingsTabs';
import type {SettingsTab} from '@app/features/user/components/settings_utils/SettingsConstants';
import {
	ACCOUNT_SETTINGS_TAB,
	getAccountSectionForLegacySection,
	getAccountSectionForNestedTab,
} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import type {SettingsSearchResult} from '@app/features/user/components/settings_utils/SettingsSearchIndex';
import type {
	SearchableSettingItem,
	UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {SettingsItemStatusBadges} from '@app/features/user/components/settings_utils/SettingsStatusBadge';
import Users from '@app/features/user/state/Users';
import {Plural, Trans} from '@lingui/react/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useRef, useState} from 'react';

interface AllSettingsRendererProps {
	searchQuery: string;
	searchResults: Array<SettingsSearchResult>;
	groupedSettingsTabs: Record<string, Array<SettingsTab>>;
	initialGuildId?: string;
}

interface SettingsSectionProps {
	tab: SettingsTab;
	matchedItems: Array<SearchableSettingItem>;
	searchQuery: string;
	initialGuildId?: string;
	isExpanded: boolean;
	onToggleExpand: () => void;
}

const INLINE_TAB_COMPONENTS: Partial<Record<UserSettingsTabType, React.ComponentType<Record<string, unknown>>>> = {
	my_profile: MyProfileTab,
	account_security: AccountSecurityInlineTab,
	plutonium: PlutoniumTab,
	gift_inventory: GiftInventoryTab,
	expression_packs: ExpressionPacksTab,
	privacy_safety: PrivacyDashboardContent,
	authorized_apps: AccountSecurityInlineTab,
	blocked_users: AccountSecurityInlineTab,
	devices: AccountSecurityInlineTab,
	appearance: AppearanceInlineContent,
	accessibility: AccessibilityInlineContent,
	chat_settings: ChatSettingsInlineContent,
	voice_video: VoiceVideoInlineContent,
	notifications: NotificationsInlineContent,
	language: LanguageTab,
	desktop_settings: DesktopSettingsTab,
	applications: ApplicationsTab,
	keybinds: KeybindsTab,
	linked_accounts: LinkedAccountsTab,
};
const getInlineTabComponent = (tab: SettingsTab): React.ComponentType<Record<string, unknown>> | null => {
	const inlineComponent = INLINE_TAB_COMPONENTS[tab.type as UserSettingsTabType];
	return inlineComponent ?? getSettingsTabComponent(tab.type) ?? null;
};

function getAccountSearchTargetSection(matchedItems: Array<SearchableSettingItem>): string | null {
	const targetSections = new Set<string>();
	for (const item of matchedItems) {
		const sourceTabType = item.sourceTabType ?? item.tabType;
		const targetSection =
			getAccountSectionForNestedTab(sourceTabType) ??
			getAccountSectionForLegacySection(item.sourceSectionId ?? item.sectionId);
		if (targetSection) {
			targetSections.add(targetSection);
		}
	}
	return targetSections.size === 1 ? (Array.from(targetSections)[0] ?? null) : null;
}

const SettingsSection: React.FC<SettingsSectionProps> = observer(
	({tab, matchedItems, initialGuildId, isExpanded, onToggleExpand}) => {
		const contentRef = useRef<HTMLDivElement>(null);
		const matchCount = matchedItems.length;
		const currentUserCreatedAt = Users.currentUser?.createdAt ?? null;
		useEffect(() => {
			if (contentRef.current) {
				contentRef.current.setAttribute('data-settings-section', tab.type);
			}
		}, [tab.type]);
		const InlineComponent = getInlineTabComponent(tab);
		if (!InlineComponent) {
			return null;
		}
		const accountTargetSection = tab.type === ACCOUNT_SETTINGS_TAB ? getAccountSearchTargetSection(matchedItems) : null;
		const settingsTabType = tab.type;
		return (
			<div
				ref={contentRef}
				className={styles.settingsSection}
				data-has-matches="true"
				data-flx="app.all-settings-renderer.settings-section.settings-section"
			>
				<button
					type="button"
					className={styles.sectionHeader}
					onClick={onToggleExpand}
					data-flx="app.all-settings-renderer.settings-section.section-header.toggle-expand.button"
				>
					<div
						className={styles.sectionTitleRow}
						data-flx="app.all-settings-renderer.settings-section.section-title-row"
					>
						<tab.icon
							className={styles.sectionIcon}
							weight={tab.iconWeight ?? 'fill'}
							data-flx="app.all-settings-renderer.settings-section.section-icon"
						/>
						<h2 className={styles.sectionTitle} data-flx="app.all-settings-renderer.settings-section.section-title">
							{tab.label}
						</h2>
						<span className={styles.matchCount} data-flx="app.all-settings-renderer.settings-section.match-count">
							<Plural
								value={matchCount}
								one="# match"
								other="# matches"
								data-flx="app.all-settings-renderer.settings-section.plural"
							/>
						</span>
						<CaretRightIcon
							size={remFromPx(16)}
							weight="bold"
							className={clsx(styles.expandIcon, isExpanded && styles.expandIconExpanded)}
							data-flx="app.all-settings-renderer.settings-section.expand-icon"
						/>
					</div>
					<div
						className={styles.matchedItemsPreview}
						data-flx="app.all-settings-renderer.settings-section.matched-items-preview"
					>
						{matchedItems.slice(0, 3).map((item) => (
							<span
								key={item.id}
								className={styles.matchPreviewChip}
								data-flx="app.all-settings-renderer.settings-section.match-preview-chip"
							>
								<span data-flx="app.all-settings-renderer.settings-section.match-preview-label">{item.label}</span>
								<SettingsItemStatusBadges
									item={item}
									userCreatedAt={currentUserCreatedAt}
									data-flx="app.all-settings-renderer.settings-section.match-preview-badges"
								/>
							</span>
						))}
						{matchCount > 3 && (
							<span
								className={styles.matchPreviewMore}
								data-flx="app.all-settings-renderer.settings-section.match-preview-more"
							>
								+{matchCount - 3}
							</span>
						)}
					</div>
				</button>
				{isExpanded && (
					<div className={styles.sectionContent} data-flx="app.all-settings-renderer.settings-section.section-content">
						{React.createElement(InlineComponent, {
							settingsTabType,
							...(accountTargetSection ? {initialSubtab: accountTargetSection} : {}),
							...(initialGuildId ? {initialGuildId} : {}),
						} as Record<string, unknown>)}
					</div>
				)}
			</div>
		);
	},
);
export const AllSettingsRenderer: React.FC<AllSettingsRendererProps> = observer(
	({searchQuery, searchResults, initialGuildId}) => {
		const containerRef = useRef<HTMLDivElement>(null);
		const [expandedTabs, setExpandedTabs] = useState<Set<UserSettingsTabType>>(new Set());
		const previousQueryRef = useRef<string>('');
		const isSearchActive = searchQuery.trim().length > 0;
		useEffect(() => {
			if (searchQuery !== previousQueryRef.current) {
				setExpandedTabs(new Set(searchResults.map((r) => r.tab.type)));
				previousQueryRef.current = searchQuery;
			}
		}, [searchQuery, searchResults]);
		useEffect(() => {
			if (!searchQuery.trim() || !containerRef.current) {
				clearHighlights();
				return;
			}
			const timer = setTimeout(() => {
				if (!containerRef.current) return;
				const allRanges: Array<Range> = [];
				const sections = containerRef.current.querySelectorAll('[data-settings-section]');
				for (const section of sections) {
					if (!(section instanceof HTMLElement)) continue;
					const ranges = createRangesForSection(section, searchQuery);
					if (ranges.length > 0) {
						allRanges.push(...ranges);
					}
				}
				setHighlightRanges(allRanges);
			}, 50);
			return () => clearTimeout(timer);
		}, [searchQuery, expandedTabs]);
		const handleToggleExpand = useCallback((tabType: UserSettingsTabType) => {
			setExpandedTabs((prev) => {
				const next = new Set(prev);
				if (next.has(tabType)) {
					next.delete(tabType);
				} else {
					next.add(tabType);
				}
				return next;
			});
		}, []);
		if (!isSearchActive) {
			return null;
		}
		if (searchResults.length === 0) {
			return (
				<div className={styles.emptyState} data-flx="app.all-settings-renderer.empty-state">
					<div className={styles.emptyStateContent} data-flx="app.all-settings-renderer.empty-state-content">
						<div className={styles.emptyStateTitle} data-flx="app.all-settings-renderer.empty-state-title">
							<Trans>No settings found</Trans>
						</div>
						<p className={styles.emptyStateDescription} data-flx="app.all-settings-renderer.empty-state-description">
							<Trans>Try searching for something like "theme", "notifications", or "privacy"</Trans>
						</p>
					</div>
				</div>
			);
		}
		const resultCount = searchResults.reduce((acc, result) => acc + result.matchedItems.length, 0);
		const categoryCount = searchResults.length;
		return (
			<div
				ref={containerRef}
				className={styles.searchResultsContainer}
				data-flx="app.all-settings-renderer.search-results-container"
			>
				<div className={styles.resultsHeader} data-flx="app.all-settings-renderer.results-header">
					<Trans>
						Found {resultCount} results in {categoryCount} categories
					</Trans>
				</div>
				{searchResults.map((result) => (
					<SettingsSection
						key={result.tab.type}
						tab={result.tab}
						matchedItems={result.matchedItems}
						searchQuery={searchQuery}
						initialGuildId={initialGuildId}
						isExpanded={expandedTabs.has(result.tab.type)}
						onToggleExpand={() => handleToggleExpand(result.tab.type)}
						data-flx="app.all-settings-renderer.settings-section"
					/>
				))}
			</div>
		);
	},
);
