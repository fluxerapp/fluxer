// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {DiscoveryGuildCard} from '@app/features/discovery/discovery/DiscoveryGuildCard';
import styles from '@app/features/discovery/discovery/DiscoveryPage.module.css';
import Discovery from '@app/features/discovery/state/Discovery';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import {getSortedDiscoveryLanguages} from '@app/features/user/utils/LocaleUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {useVirtualizer} from '@tanstack/react-virtual';
import {observer} from 'mobx-react-lite';
import {type KeyboardEvent, useCallback, useEffect, useMemo, useRef, useState} from 'react';

const EXPLORE_PUBLIC_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Explore public communities',
	comment: 'Main heading on the Discovery page.',
});
const DISCOVERY_LISTING_SUBHEADING_DESCRIPTOR = msg({
	message:
		"Want to list your community on here? Apply if you meet the requirements in your community's settings > Discovery.",
	comment:
		'Subheading on the Discovery page. "settings > Discovery" refers to the navigation path in community settings.',
});
const ALL_DESCRIPTOR = msg({
	message: 'All',
	comment: 'Label for the all-categories tab in the Discovery page.',
});
const DISCOVERY_CATEGORIES_DESCRIPTOR = msg({
	message: 'Discovery categories',
	comment: 'Accessible label for the category tabs in the Discovery page.',
});
const ALL_LANGUAGES_DESCRIPTOR = msg({
	message: 'All languages',
	comment: 'Short label in the discovery page. Keep it concise.',
});
const SEARCH_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Search communities',
	comment: 'Accessible label and placeholder for the community search field in the Discovery page.',
});
const FILTER_BY_LANGUAGE_DESCRIPTOR = msg({
	message: 'Filter by language',
	comment: 'Short label in the discovery page. Keep it concise.',
});
const DISCOVERY_SEARCH_AND_FILTERS_DESCRIPTOR = msg({
	message: 'Discovery search and filters',
	comment: 'Accessible label for the search and filter controls in the Discovery page.',
});
const DISCOVERY_RESULTS_DESCRIPTOR = msg({
	message: 'Discovery results',
	comment: 'Accessible label for the Discovery community results region.',
});
const DISCOVERY_RESULTS_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# community found} other {# communities found}}',
	comment: 'Screen-reader status text in Discovery. {count} is the total number of matching communities.',
});
const LOADING_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Loading communities',
	comment: 'Screen-reader status text shown while Discovery communities are loading.',
});
const FILTER_DISCOVERY_BY_LANGUAGE_DESCRIPTOR = msg({
	message: 'Filter Discovery by language',
	comment: 'Label in the discovery page.',
});
const NO_COMMUNITIES_FOUND_DESCRIPTOR = msg({
	message: 'No communities match.',
	comment: 'Empty-state text in the discovery page.',
});

const PAGE_SIZE = 36;
const GRID_MIN_CARD_WIDTH_PX = 280;
const GRID_MAX_COLUMNS = 4;
const GRID_GAP_PX = 16;
const ESTIMATED_ROW_HEIGHT_PX = 276;
const SEARCH_DEBOUNCE_MS = 300;
const OVERSCAN_ROWS = 3;
const ALL_CATEGORY_TAB_KEY = 'all';
const CATEGORY_TAB_PREFIX = 'category:';

type DiscoveryCategoryTabKey = typeof ALL_CATEGORY_TAB_KEY | `${typeof CATEGORY_TAB_PREFIX}${number}`;

interface DiscoveryCategoryTab {
	key: DiscoveryCategoryTabKey;
	categoryId: number | null;
	label: string;
}

function getCategoryTabKey(categoryId: number | null): DiscoveryCategoryTabKey {
	return categoryId === null ? ALL_CATEGORY_TAB_KEY : `${CATEGORY_TAB_PREFIX}${categoryId}`;
}

export const DiscoveryPage = observer(function DiscoveryPage() {
	const {i18n} = useLingui();
	const searchTimerRef = useRef<NodeJS.Timeout | null>(null);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const containerRef = useRef<HTMLDivElement | null>(null);
	const categoryTabRefs = useRef<Map<DiscoveryCategoryTabKey, HTMLButtonElement>>(new Map());
	const [containerWidth, setContainerWidth] = useState(0);
	const zoomLevel = Accessibility.zoomLevel;
	const columns = useMemo(() => {
		if (containerWidth <= 0) {
			return 0;
		}
		const logicalWidth = containerWidth / getAppZoomFactor();
		const columnsThatFit = Math.floor((logicalWidth + GRID_GAP_PX) / (GRID_MIN_CARD_WIDTH_PX + GRID_GAP_PX));
		return Math.min(GRID_MAX_COLUMNS, Math.max(1, columnsThatFit));
	}, [containerWidth, zoomLevel]);
	const guilds = Discovery.guilds;
	const rowCount = columns > 0 ? Math.ceil(guilds.length / columns) : 0;
	const hasMore = guilds.length < Discovery.total;
	const virtualizer = useVirtualizer({
		count: rowCount,
		getScrollElement: () => scrollerRef.current?.getScrollerNode() ?? null,
		estimateSize: () => ESTIMATED_ROW_HEIGHT_PX,
		overscan: OVERSCAN_ROWS,
	});
	const loadMore = useCallback(() => {
		if (Discovery.loading || Discovery.error || !hasMore) {
			return;
		}
		void Discovery.search({
			offset: guilds.length,
			limit: PAGE_SIZE,
		});
	}, [guilds.length, hasMore]);
	useEffect(() => {
		const items = virtualizer.getVirtualItems();
		const lastItem = items[items.length - 1];
		if (lastItem && lastItem.index >= rowCount - OVERSCAN_ROWS) {
			loadMore();
		}
	}, [virtualizer.getVirtualItems(), rowCount, loadMore]);
	useEffect(() => {
		const node = containerRef.current;
		if (!node) return;
		const observer = new ResizeObserver(([entry]) => {
			if (entry) {
				setContainerWidth(entry.contentRect.width);
			}
		});
		observer.observe(node);
		return () => observer.disconnect();
	}, []);
	const handleSearchChange = useCallback((value: string) => {
		if (searchTimerRef.current) {
			clearTimeout(searchTimerRef.current);
		}
		searchTimerRef.current = setTimeout(() => {
			void Discovery.search({query: value, offset: 0, limit: PAGE_SIZE});
		}, SEARCH_DEBOUNCE_MS);
	}, []);
	const handleCategoryClick = useCallback((categoryId: number | null) => {
		void Discovery.search({category: categoryId, offset: 0, limit: PAGE_SIZE});
	}, []);
	const categoryTabs: Array<DiscoveryCategoryTab> = [
		{key: ALL_CATEGORY_TAB_KEY, categoryId: null, label: i18n._(ALL_DESCRIPTOR)},
		...Discovery.categories.map((cat) => ({
			key: getCategoryTabKey(cat.id),
			categoryId: cat.id,
			label: cat.name,
		})),
	];
	const activeCategoryTabKey =
		Discovery.category === null || Discovery.categories.some((category) => category.id === Discovery.category)
			? getCategoryTabKey(Discovery.category)
			: null;
	const selectedCategoryExists = activeCategoryTabKey !== null;
	const focusCategoryTab = useCallback((tabKey: DiscoveryCategoryTabKey) => {
		const nextButton = categoryTabRefs.current.get(tabKey);
		if (!nextButton) {
			return;
		}
		nextButton.focus({preventScroll: true});
		nextButton.scrollIntoView({block: 'nearest', inline: 'nearest'});
	}, []);
	const handleCategoryTabKeyDown = useCallback(
		(event: KeyboardEvent<HTMLButtonElement>, tabKey: DiscoveryCategoryTabKey) => {
			if (event.defaultPrevented || event.altKey || event.ctrlKey || event.metaKey) {
				return;
			}
			const direction = getTabNavigationDirection(event.key, 'horizontal');
			if (!direction) {
				return;
			}
			const currentIndex = categoryTabs.findIndex((tab) => tab.key === tabKey);
			const nextIndex = getNextTabIndex(currentIndex, categoryTabs.length, direction);
			const nextTab = nextIndex == null ? null : categoryTabs[nextIndex];
			if (!nextTab) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();
			focusCategoryTab(nextTab.key);
			if (nextTab.key !== tabKey) {
				handleCategoryClick(nextTab.categoryId);
			}
		},
		[categoryTabs, focusCategoryTab, handleCategoryClick],
	);
	const languageOptions = useMemo<ReadonlyArray<ComboboxOption<string>>>(
		() => [
			{value: '', label: i18n._(ALL_LANGUAGES_DESCRIPTOR)},
			...getSortedDiscoveryLanguages().map((language) => ({
				value: language.code,
				label: language.label,
			})),
		],
		[i18n.locale],
	);
	const handleLanguageChange = useCallback((value: string) => {
		void Discovery.search({language: value || null, offset: 0, limit: PAGE_SIZE});
	}, []);
	useEffect(() => {
		return () => {
			if (searchTimerRef.current) {
				clearTimeout(searchTimerRef.current);
			}
		};
	}, []);
	useEffect(() => {
		if (activeCategoryTabKey) {
			categoryTabRefs.current.get(activeCategoryTabKey)?.scrollIntoView({block: 'nearest', inline: 'nearest'});
		}
	}, [activeCategoryTabKey]);
	const virtualRows = virtualizer.getVirtualItems();
	const gridColumnsStyle =
		columns > 0 ? `repeat(${columns}, 1fr)` : `repeat(auto-fill, minmax(${GRID_MIN_CARD_WIDTH_PX}px, 1fr))`;
	const resultsStatus =
		Discovery.loading && guilds.length === 0
			? i18n._(LOADING_COMMUNITIES_DESCRIPTOR)
			: i18n._(DISCOVERY_RESULTS_COUNT_DESCRIPTOR, {count: Discovery.total});
	return (
		<div className={styles.container} data-flx="discovery.discovery.discovery-page.container">
			<Scroller ref={scrollerRef} data-flx="discovery.discovery.discovery-page.scroller">
				<div className={styles.hero} data-flx="discovery.discovery.discovery-page.hero">
					<div className={styles.heroContent} data-flx="discovery.discovery.discovery-page.hero-content">
						<h1 className={styles.heroHeading} data-flx="discovery.discovery.discovery-page.hero-heading">
							{i18n._(EXPLORE_PUBLIC_COMMUNITIES_DESCRIPTOR)}
						</h1>
						<p className={styles.heroSubheading} data-flx="discovery.discovery.discovery-page.hero-subheading">
							{i18n._(DISCOVERY_LISTING_SUBHEADING_DESCRIPTOR)}
						</p>
						<div
							className={styles.heroFilters}
							role="search"
							aria-label={i18n._(DISCOVERY_SEARCH_AND_FILTERS_DESCRIPTOR)}
							data-flx="discovery.discovery.discovery-page.hero-filters"
						>
							<div className={styles.heroControls} data-flx="discovery.discovery.discovery-page.hero-controls">
								<Input
									className={styles.searchInput}
									type="search"
									autoFocus
									aria-label={i18n._(SEARCH_COMMUNITIES_DESCRIPTOR)}
									aria-controls="discovery-results"
									placeholder={i18n._(SEARCH_COMMUNITIES_DESCRIPTOR)}
									defaultValue={Discovery.query}
									onChange={(e) => handleSearchChange(e.target.value)}
									leftIcon={
										<MagnifyingGlassIcon
											size={remFromPx(16)}
											weight="bold"
											aria-hidden
											data-flx="discovery.discovery.discovery-page.magnifying-glass-icon"
										/>
									}
									data-flx="discovery.discovery.discovery-page.search-input.search-change"
								/>
							</div>
							<div
								className={styles.languageFilterRow}
								data-flx="discovery.discovery.discovery-page.language-filter-row"
							>
								<Combobox<string>
									className={styles.languageFilter}
									options={languageOptions}
									value={Discovery.language ?? ''}
									onChange={handleLanguageChange}
									isSearchable
									density="compact"
									placeholder={i18n._(FILTER_BY_LANGUAGE_DESCRIPTOR)}
									aria-label={i18n._(FILTER_DISCOVERY_BY_LANGUAGE_DESCRIPTOR)}
									data-flx="discovery.discovery.discovery-page.language-filter.language-change"
								/>
							</div>
						</div>
					</div>
				</div>
				<div
					className={styles.categoryTabs}
					role="tablist"
					aria-orientation="horizontal"
					aria-label={i18n._(DISCOVERY_CATEGORIES_DESCRIPTOR)}
					data-flx="discovery.discovery.discovery-page.category-tabs"
				>
					{categoryTabs.map((tab) => {
						const isActive = activeCategoryTabKey === tab.key;
						return (
							<FocusRing
								key={tab.key}
								offset={-2}
								data-flx="discovery.discovery.discovery-page.category-tab.focus-ring"
							>
								<button
									ref={(el) => {
										if (el) {
											categoryTabRefs.current.set(tab.key, el);
										} else {
											categoryTabRefs.current.delete(tab.key);
										}
									}}
									type="button"
									role="tab"
									className={isActive ? styles.categoryTabActive : styles.categoryTab}
									onClick={() => handleCategoryClick(tab.categoryId)}
									onKeyDown={(event) => handleCategoryTabKeyDown(event, tab.key)}
									aria-selected={isActive}
									aria-controls="discovery-results"
									tabIndex={isActive || (!selectedCategoryExists && tab.key === ALL_CATEGORY_TAB_KEY) ? 0 : -1}
									data-flx="discovery.discovery.discovery-page.category-tab"
								>
									{tab.label}
								</button>
							</FocusRing>
						);
					})}
				</div>
				<div
					id="discovery-results"
					ref={containerRef}
					className={styles.content}
					role="region"
					aria-label={i18n._(DISCOVERY_RESULTS_DESCRIPTOR)}
					aria-busy={Discovery.loading}
					data-flx="discovery.discovery.discovery-page.content"
				>
					<div
						className={styles.srOnly}
						role="status"
						aria-live="polite"
						aria-atomic="true"
						data-flx="discovery.discovery.discovery-page.results-status"
					>
						{resultsStatus}
					</div>
					{Discovery.loading && guilds.length === 0 ? (
						<div className={styles.loadingState} data-flx="discovery.discovery.discovery-page.loading-state">
							<Spinner data-flx="discovery.discovery.discovery-page.spinner" />
						</div>
					) : guilds.length > 0 && columns > 0 ? (
						<div
							className={styles.virtualGrid}
							style={{height: virtualizer.getTotalSize()}}
							data-flx="discovery.discovery.discovery-page.virtual-grid"
						>
							{virtualRows.map((virtualRow) => {
								const startIndex = virtualRow.index * columns;
								const rowGuilds = guilds.slice(startIndex, startIndex + columns);
								return (
									<div
										key={virtualRow.key}
										ref={virtualizer.measureElement}
										data-index={virtualRow.index}
										className={styles.gridRow}
										style={{
											transform: `translateY(${virtualRow.start}px)`,
											gridTemplateColumns: gridColumnsStyle,
										}}
										data-flx="discovery.discovery.discovery-page.grid-row"
									>
										{rowGuilds.map((guild) => (
											<DiscoveryGuildCard
												key={guild.id}
												guild={guild}
												data-flx="discovery.discovery.discovery-page.discovery-guild-card"
											/>
										))}
									</div>
								);
							})}
						</div>
					) : !Discovery.loading ? (
						<div className={styles.emptyState} data-flx="discovery.discovery.discovery-page.empty-state">
							<span className={styles.emptyStateText} data-flx="discovery.discovery.discovery-page.empty-state-text">
								{i18n._(NO_COMMUNITIES_FOUND_DESCRIPTOR)}
							</span>
						</div>
					) : null}
					{hasMore && Discovery.loading && (
						<div className={styles.loadingMore} data-flx="discovery.discovery.discovery-page.loading-more">
							<Spinner data-flx="discovery.discovery.discovery-page.spinner--2" />
						</div>
					)}
				</div>
			</Scroller>
		</div>
	);
});
