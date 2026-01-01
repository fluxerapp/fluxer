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

import './SettingsSearchHighlight.css';

import {Trans} from '@lingui/react/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import {clearHighlights, createRangesForSection, setHighlightRanges} from '~/utils/CSSHighlightSearch';
import {AccessibilityInlineTab} from '../tabs/AccessibilityTab/Inline';
import {AccountSecurityInlineTab} from '../tabs/AccountSecurityTab/Inline';
import AdvancedTab from '../tabs/AdvancedTab';
import {AppearanceInlineTab} from '../tabs/AppearanceTab/Inline';
import ApplicationsTab from '../tabs/ApplicationsTab';
import AuthorizedAppsTab from '../tabs/AuthorizedAppsTab';
import BetaCodesTab from '../tabs/BetaCodesTab';
import BlockedUsersTab from '../tabs/BlockedUsersTab';
import {ChatSettingsInlineTab} from '../tabs/ChatSettingsTab/Inline';
import {ComponentGalleryInlineTab} from '../tabs/ComponentGalleryTab/Inline';
import {DeveloperOptionsInlineTab} from '../tabs/DeveloperOptionsTab/Inline';
import DevicesTab from '../tabs/DevicesTab';
import ExpressionPacksTab from '../tabs/ExpressionPacksTab';
import FeatureFlagsTab from '../tabs/FeatureFlagsTab';
import GiftInventoryTab from '../tabs/GiftInventoryTab';
import KeybindsTab from '../tabs/KeybindsTab';
import LanguageTab from '../tabs/LanguageTab';
import MyProfileTab from '../tabs/MyProfileTab';
import {NotificationsInlineTab} from '../tabs/NotificationsTab/Inline';
import PlutoniumTab from '../tabs/PlutoniumTab';
import {PrivacySafetyInlineTab} from '../tabs/PrivacySafetyTab/Inline';
import {VoiceVideoInlineTab} from '../tabs/VoiceVideoTab/Inline';
import {getSettingsTabComponent} from '../utils/desktopSettingsTabs';
import type {SettingsTab, UserSettingsTabType} from '../utils/settingsConstants';
import type {SearchableSettingItem, SettingsSearchResult} from '../utils/settingsSearchIndex';
import styles from './AllSettingsRenderer.module.css';

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
	beta_codes: BetaCodesTab,
	plutonium: PlutoniumTab,
	gift_inventory: GiftInventoryTab,
	expression_packs: ExpressionPacksTab,
	privacy_safety: PrivacySafetyInlineTab,
	authorized_apps: AuthorizedAppsTab,
	blocked_users: BlockedUsersTab,
	devices: DevicesTab,
	appearance: AppearanceInlineTab,
	accessibility: AccessibilityInlineTab,
	chat_settings: ChatSettingsInlineTab,
	voice_video: VoiceVideoInlineTab,
	notifications: NotificationsInlineTab,
	language: LanguageTab,
	advanced: AdvancedTab,
	applications: ApplicationsTab,
	keybinds: KeybindsTab,
	developer_options: DeveloperOptionsInlineTab,
	component_gallery: ComponentGalleryInlineTab,
	feature_flags: FeatureFlagsTab,
};

const getInlineTabComponent = (tab: SettingsTab): React.ComponentType<Record<string, unknown>> | null => {
	const inlineComponent = INLINE_TAB_COMPONENTS[tab.type as UserSettingsTabType];
	return inlineComponent ?? getSettingsTabComponent(tab.type) ?? null;
};

const SettingsSection: React.FC<SettingsSectionProps> = observer(
	({tab, matchedItems, initialGuildId, isExpanded, onToggleExpand}) => {
		const contentRef = React.useRef<HTMLDivElement>(null);

		React.useEffect(() => {
			if (contentRef.current) {
				contentRef.current.setAttribute('data-settings-section', tab.type);
			}
		}, [tab.type]);

		if (tab.category === 'staff_only' && !DeveloperModeStore.isDeveloper) {
			return null;
		}

		const InlineComponent = getInlineTabComponent(tab);

		if (!InlineComponent) {
			return null;
		}

		return (
			<div ref={contentRef} className={styles.settingsSection} data-has-matches="true">
				<button type="button" className={styles.sectionHeader} onClick={onToggleExpand}>
					<div className={styles.sectionTitleRow}>
						<tab.icon className={styles.sectionIcon} weight={tab.iconWeight ?? 'fill'} />
						<h2 className={styles.sectionTitle}>{tab.label}</h2>
						<span className={styles.matchCount}>
							{matchedItems.length === 1 ? <Trans>1 match</Trans> : <Trans>{matchedItems.length} matches</Trans>}
						</span>
						<CaretRightIcon
							size={16}
							weight="bold"
							className={clsx(styles.expandIcon, isExpanded && styles.expandIconExpanded)}
						/>
					</div>
					<div className={styles.matchedItemsPreview}>
						{matchedItems.slice(0, 3).map((item) => (
							<span key={item.id} className={styles.matchPreviewChip}>
								{item.label}
							</span>
						))}
						{matchedItems.length > 3 && <span className={styles.matchPreviewMore}>+{matchedItems.length - 3}</span>}
					</div>
				</button>

				{isExpanded && (
					<div className={styles.sectionContent}>
						{React.createElement(
							InlineComponent,
							(initialGuildId ? {initialGuildId} : undefined) as Record<string, unknown>,
						)}
					</div>
				)}
			</div>
		);
	},
);

export const AllSettingsRenderer: React.FC<AllSettingsRendererProps> = observer(
	({searchQuery, searchResults, initialGuildId}) => {
		const containerRef = React.useRef<HTMLDivElement>(null);
		const [expandedTabs, setExpandedTabs] = React.useState<Set<UserSettingsTabType>>(new Set());
		const previousQueryRef = React.useRef<string>('');

		const isSearchActive = searchQuery.trim().length > 0;

		React.useEffect(() => {
			if (searchQuery !== previousQueryRef.current) {
				setExpandedTabs(new Set(searchResults.map((r) => r.tab.type)));
				previousQueryRef.current = searchQuery;
			}
		}, [searchQuery, searchResults]);

		React.useEffect(() => {
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

		const handleToggleExpand = React.useCallback((tabType: UserSettingsTabType) => {
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
				<div className={styles.emptyState}>
					<div className={styles.emptyStateContent}>
						<div className={styles.emptyStateTitle}>
							<Trans>No settings found</Trans>
						</div>
						<p className={styles.emptyStateDescription}>
							<Trans>Try searching for something like "theme", "notifications", or "privacy"</Trans>
						</p>
					</div>
				</div>
			);
		}

		return (
			<div ref={containerRef} className={styles.searchResultsContainer}>
				<div className={styles.resultsHeader}>
					<Trans>
						Found {searchResults.reduce((acc, r) => acc + r.matchedItems.length, 0)} results in {searchResults.length}{' '}
						categories
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
					/>
				))}
			</div>
		);
	},
);
