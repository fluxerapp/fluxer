// SPDX-License-Identifier: AGPL-3.0-or-later

import {reportSkeletonDiscoveryCategoryTabs} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {measureSkeletonWidthPx, useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import searchBarStyles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import {DiscoveryLanguageButton} from '@app/features/discovery/discovery/DiscoveryLanguageButton';
import styles from '@app/features/discovery/discovery/DiscoveryNavbar.module.css';
import {DiscoveryMotionKind, DiscoveryTransition} from '@app/features/discovery/discovery/DiscoveryTransition';
import Discovery from '@app/features/discovery/state/Discovery';
import {CLEAR_SEARCH_DESCRIPTOR, GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type FormEvent, type KeyboardEvent, useCallback, useEffect, useRef, useState} from 'react';

const ALL_DESCRIPTOR = msg({
	message: 'All',
	comment: 'Label for the all-categories tab in the Discovery page.',
});
const DISCOVERY_CATEGORIES_DESCRIPTOR = msg({
	message: 'Discovery categories',
	comment: 'Accessible label for the category tabs in the Discovery page.',
});
const SEARCH_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Search communities',
	comment: 'Accessible label and placeholder for the community search field in the Discovery page.',
});
const DISCOVERY_SEARCH_DESCRIPTOR = msg({
	message: 'Discovery search',
	comment: 'Accessible label for the search form in the Discovery navigation bar.',
});
const SEARCH_RESULTS_FOR_DESCRIPTOR = msg({
	message: 'Search results for “{query}”',
	comment:
		'Heading in the Discovery navigation bar when a search is active. {query} is the text the user searched for.',
});

function resolveLeftSectionTransitionKey(searchActive: boolean): string {
	if (searchActive) {
		return 'search';
	}
	return 'browse';
}

const ALL_CATEGORY_TAB_KEY = 'all';
const CATEGORY_TAB_PREFIX = 'category:';

type DiscoveryCategoryTabKey = typeof ALL_CATEGORY_TAB_KEY | `${typeof CATEGORY_TAB_PREFIX}${number}`;

interface DiscoveryCategoryTab {
	readonly key: DiscoveryCategoryTabKey;
	readonly categoryId: number | null;
	readonly label: string;
}

function getDiscoveryCategoryTabKey(categoryId: number | null): DiscoveryCategoryTabKey {
	return categoryId === null ? ALL_CATEGORY_TAB_KEY : `${CATEGORY_TAB_PREFIX}${categoryId}`;
}

interface DiscoveryNavbarProps {
	readonly resultsRegionId: string;
	readonly onCategorySelect: (categoryId: number | null) => void;
	readonly onSearchSubmit: (query: string) => void;
	readonly onSearchClear: () => void;
	readonly onBack: (() => void) | null;
}

export const DiscoveryNavbar = observer(function DiscoveryNavbar({
	resultsRegionId,
	onCategorySelect,
	onSearchSubmit,
	onSearchClear,
	onBack,
}: DiscoveryNavbarProps) {
	const {i18n} = useLingui();
	const categoryTabRefs = useRef<Map<DiscoveryCategoryTabKey, HTMLButtonElement>>(new Map());
	const searchInputRef = useRef<HTMLInputElement>(null);
	const restoreTabFocusRef = useRef(false);
	const [focusedCategoryTabKey, setFocusedCategoryTabKey] = useState<DiscoveryCategoryTabKey | null>(null);
	const submittedQuery = Discovery.query;
	const [draftQuery, setDraftQuery] = useState(submittedQuery);
	useEffect(() => {
		setDraftQuery(submittedQuery);
	}, [submittedQuery]);
	const searchActive = onBack !== null;
	const categoryTabs: Array<DiscoveryCategoryTab> = [
		{key: ALL_CATEGORY_TAB_KEY, categoryId: null, label: i18n._(ALL_DESCRIPTOR)},
		...Discovery.categories.map((category) => ({
			key: getDiscoveryCategoryTabKey(category.id),
			categoryId: category.id,
			label: category.name,
		})),
	];
	const activeCategoryTabKey =
		Discovery.category === null || Discovery.categories.some((category) => category.id === Discovery.category)
			? getDiscoveryCategoryTabKey(Discovery.category)
			: null;
	const selectedCategoryExists = activeCategoryTabKey !== null;
	const focusCategoryTab = useCallback((tabKey: DiscoveryCategoryTabKey) => {
		const nextButton = categoryTabRefs.current.get(tabKey);
		if (!nextButton) {
			return;
		}
		setFocusedCategoryTabKey(tabKey);
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
		},
		[categoryTabs, focusCategoryTab],
	);
	useEffect(() => {
		if (!searchActive && activeCategoryTabKey) {
			categoryTabRefs.current.get(activeCategoryTabKey)?.scrollIntoView({block: 'nearest', inline: 'nearest'});
		}
	}, [activeCategoryTabKey, searchActive]);
	useSkeletonLayoutReport(
		() => {
			if (searchActive) {
				return;
			}
			reportSkeletonDiscoveryCategoryTabs(
				categoryTabs.map((tab) => measureSkeletonWidthPx(categoryTabRefs.current.get(tab.key) ?? null)),
			);
		},
		`${searchActive}:${categoryTabs.map((tab) => tab.label).join('\u0000')}`,
	);
	const handleCategoryTabClick = useCallback(
		(tab: DiscoveryCategoryTab) => {
			setFocusedCategoryTabKey(tab.key);
			onCategorySelect(tab.categoryId);
		},
		[onCategorySelect],
	);
	const handleSubmit = useCallback(
		(event: FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const trimmedQuery = draftQuery.trim();
			setDraftQuery(trimmedQuery);
			onSearchSubmit(trimmedQuery);
		},
		[draftQuery, onSearchSubmit],
	);
	const resolveCategoryTabIndex = (tabKey: DiscoveryCategoryTabKey, isActive: boolean): number => {
		if (focusedCategoryTabKey !== null) {
			return focusedCategoryTabKey === tabKey ? 0 : -1;
		}
		if (isActive) {
			return 0;
		}
		return !selectedCategoryExists && tabKey === ALL_CATEGORY_TAB_KEY ? 0 : -1;
	};
	const handleClear = useCallback(() => {
		setDraftQuery('');
		onSearchClear();
		searchInputRef.current?.focus();
	}, [onSearchClear]);
	const handleBack = useCallback(() => {
		if (!onBack) {
			return;
		}
		restoreTabFocusRef.current = true;
		onBack();
	}, [onBack]);
	useEffect(() => {
		if (searchActive || !restoreTabFocusRef.current) {
			return;
		}
		restoreTabFocusRef.current = false;
		focusCategoryTab(activeCategoryTabKey ?? ALL_CATEGORY_TAB_KEY);
	}, [searchActive, activeCategoryTabKey, focusCategoryTab]);
	return (
		<div className={styles.navbar} data-flx="discovery.discovery.discovery-navbar.navbar">
			<div className={styles.leftSection} data-flx="discovery.discovery.discovery-navbar.left-section">
				<DiscoveryTransition
					transitionKey={resolveLeftSectionTransitionKey(searchActive)}
					motionKind={DiscoveryMotionKind.NAVBAR}
					className={styles.leftSurface}
					data-flx="discovery.discovery.discovery-navbar.left-surface"
				>
					{onBack ? (
						<>
							<ChannelHeaderIcon
								icon={ArrowLeftIcon}
								iconWeight="bold"
								label={i18n._(GO_BACK_DESCRIPTOR)}
								className={styles.backButton}
								onClick={handleBack}
								data-flx="discovery.discovery.discovery-navbar.back-button"
							/>
							<h2 className={styles.searchTitle} data-flx="discovery.discovery.discovery-navbar.search-title">
								{i18n._(SEARCH_RESULTS_FOR_DESCRIPTOR, {query: submittedQuery})}
							</h2>
						</>
					) : (
						<div
							className={styles.categoryTabs}
							role="tablist"
							aria-orientation="horizontal"
							aria-label={i18n._(DISCOVERY_CATEGORIES_DESCRIPTOR)}
							data-flx="discovery.discovery.discovery-navbar.category-tabs"
						>
							{categoryTabs.map((tab) => {
								const isActive = activeCategoryTabKey === tab.key;
								return (
									<FocusRing key={tab.key} offset={-2} data-flx="discovery.discovery.discovery-navbar.focus-ring">
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
											onClick={() => handleCategoryTabClick(tab)}
											onKeyDown={(event) => handleCategoryTabKeyDown(event, tab.key)}
											aria-selected={isActive}
											tabIndex={resolveCategoryTabIndex(tab.key, isActive)}
											data-flx="discovery.discovery.discovery-navbar.category-tab.category-select.button"
										>
											{tab.label}
										</button>
									</FocusRing>
								);
							})}
						</div>
					)}
				</DiscoveryTransition>
			</div>
			<div className={styles.searchArea} data-flx="discovery.discovery.discovery-navbar.search-area">
				<form
					className={styles.searchForm}
					role="search"
					aria-label={i18n._(DISCOVERY_SEARCH_DESCRIPTOR)}
					onSubmit={handleSubmit}
					data-flx="discovery.discovery.discovery-navbar.search-form.submit"
				>
					<div className={searchBarStyles.inputContainer} data-flx="discovery.discovery.discovery-navbar.div">
						<MagnifyingGlassIcon
							className={clsx(searchBarStyles.searchIcon, styles.searchIcon)}
							weight="bold"
							aria-hidden
							data-flx="discovery.discovery.discovery-navbar.search-icon"
						/>
						<input
							ref={searchInputRef}
							className={styles.searchInput}
							type="search"
							aria-label={i18n._(SEARCH_COMMUNITIES_DESCRIPTOR)}
							aria-controls={resultsRegionId}
							placeholder={i18n._(SEARCH_COMMUNITIES_DESCRIPTOR)}
							value={draftQuery}
							onChange={(event) => setDraftQuery(event.target.value)}
							data-flx="discovery.discovery.discovery-navbar.search-input.set-draft-query"
						/>
						{draftQuery.length > 0 && (
							<FocusRing offset={-2} data-flx="discovery.discovery.discovery-navbar.focus-ring">
								<button
									type="button"
									className={searchBarStyles.clearButton}
									aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
									onClick={handleClear}
									data-flx="discovery.discovery.discovery-navbar.button.clear"
								>
									<XIcon
										weight="bold"
										className={searchBarStyles.optionMetaIcon}
										aria-hidden
										data-flx="discovery.discovery.discovery-navbar.x-icon"
									/>
								</button>
							</FocusRing>
						)}
					</div>
				</form>
				<DiscoveryLanguageButton data-flx="discovery.discovery.discovery-navbar.discovery-language-button" />
			</div>
		</div>
	);
});
