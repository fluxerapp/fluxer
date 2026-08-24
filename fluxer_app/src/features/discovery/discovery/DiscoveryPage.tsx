// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {DiscoveryPageSkeleton, DiscoveryResultsSkeleton} from '@app/features/app/components/skeleton/DiscoverySkeleton';
import {
	reportSkeletonDiscoveryGrid,
	SKELETON_DISCOVERY_ESTIMATED_ROW_HEIGHT_PX,
	SKELETON_DISCOVERY_GRID_GAP_PX,
	SKELETON_DISCOVERY_MAX_COLUMNS,
	SKELETON_DISCOVERY_MIN_CARD_WIDTH_PX,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {measureSkeletonHeightPx, useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {DiscoveryEmptyState} from '@app/features/discovery/discovery/DiscoveryEmptyState';
import {DISCOVERY_GRID_METRICS_STYLE} from '@app/features/discovery/discovery/DiscoveryGridMetrics';
import {DiscoveryGuildCard} from '@app/features/discovery/discovery/DiscoveryGuildCard';
import {DiscoveryNavbar} from '@app/features/discovery/discovery/DiscoveryNavbar';
import styles from '@app/features/discovery/discovery/DiscoveryPage.module.css';
import {DiscoverySearchFilters} from '@app/features/discovery/discovery/DiscoverySearchFilters';
import {DiscoveryMotionKind, DiscoveryTransition} from '@app/features/discovery/discovery/DiscoveryTransition';
import Discovery from '@app/features/discovery/state/Discovery';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import {getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useVirtualizer} from '@tanstack/react-virtual';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

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

function resolveDiscoveryResultsSurfaceKey(searchActive: boolean): string {
	if (searchActive) {
		return 'search';
	}
	return 'browse';
}

const PAGE_SIZE = 36;
const OVERSCAN_ROWS = 3;
const DISCOVERY_RESULTS_REGION_ID = 'discovery-results';

function useCategoriesSettledLatch(): boolean {
	const settled = Discovery.categoriesLoaded || Discovery.categoriesError;
	const latchedRef = useRef(settled);
	if (settled) {
		latchedRef.current = true;
	}
	return latchedRef.current;
}

function useFirstResultsSettledLatch(): boolean {
	const startedRef = useRef(false);
	const settledRef = useRef(false);
	if (Discovery.loading) {
		startedRef.current = true;
	}
	if (startedRef.current && !Discovery.loading) {
		settledRef.current = true;
	}
	return settledRef.current;
}

function resolveScrollOffsetTop(element: HTMLElement, scrollerNode: HTMLElement | null): number {
	if (scrollerNode == null) {
		return 0;
	}
	const offsetTop = element.getBoundingClientRect().top - scrollerNode.getBoundingClientRect().top;
	return Math.max(0, Math.round(offsetTop + scrollerNode.scrollTop));
}

export const DiscoveryPage = observer(function DiscoveryPage() {
	const {i18n} = useLingui();
	const scrollerRef = useRef<ScrollerHandle>(null);
	const [resultsColumnNode, setResultsColumnNode] = useState<HTMLDivElement | null>(null);
	const [containerWidth, setContainerWidth] = useState(0);
	const [gridOffsetTop, setGridOffsetTop] = useState(0);
	const zoomLevel = Accessibility.zoomLevel;
	const columns = useMemo(() => {
		if (containerWidth <= 0) {
			return 0;
		}
		const logicalWidth = containerWidth / getAppRemScale();
		const columnsThatFit = Math.floor(
			(logicalWidth + SKELETON_DISCOVERY_GRID_GAP_PX) /
				(SKELETON_DISCOVERY_MIN_CARD_WIDTH_PX + SKELETON_DISCOVERY_GRID_GAP_PX),
		);
		return Math.min(SKELETON_DISCOVERY_MAX_COLUMNS, Math.max(1, columnsThatFit));
	}, [containerWidth, zoomLevel]);
	const guilds = Discovery.guilds;
	const searchActive = Discovery.query.length > 0;
	const rowCount = columns > 0 ? Math.ceil(guilds.length / columns) : 0;
	const hasMore = guilds.length < Discovery.total;
	const virtualizer = useVirtualizer({
		count: rowCount,
		getScrollElement: () => scrollerRef.current?.getViewportElement() ?? null,
		estimateSize: () => SKELETON_DISCOVERY_ESTIMATED_ROW_HEIGHT_PX,
		overscan: OVERSCAN_ROWS,
		scrollMargin: gridOffsetTop,
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
	const attachResultsColumn = useCallback((node: HTMLDivElement | null) => {
		setResultsColumnNode(node);
		if (!node) {
			return;
		}
		setContainerWidth(node.getBoundingClientRect().width);
		setGridOffsetTop(resolveScrollOffsetTop(node, scrollerRef.current?.getViewportElement() ?? null));
	}, []);
	useEffect(() => {
		if (!resultsColumnNode) return;
		const observer = new ResizeObserver(([entry]) => {
			if (entry) {
				setContainerWidth(entry.contentRect.width);
			}
		});
		observer.observe(resultsColumnNode);
		return () => observer.disconnect();
	}, [resultsColumnNode]);
	useLayoutEffect(() => {
		if (!resultsColumnNode) return;
		setContainerWidth(resultsColumnNode.getBoundingClientRect().width);
	}, [resultsColumnNode, searchActive]);
	useEffect(() => {
		if (!resultsColumnNode) return;
		setGridOffsetTop(resolveScrollOffsetTop(resultsColumnNode, scrollerRef.current?.getViewportElement() ?? null));
	}, [resultsColumnNode, searchActive, containerWidth]);
	const scrollResultsToTop = useCallback(() => {
		scrollerRef.current?.scrollTo({to: 0});
	}, []);
	const handleCategorySelect = useCallback(
		(categoryId: number | null) => {
			void Discovery.search({category: categoryId, offset: 0, limit: PAGE_SIZE});
			scrollResultsToTop();
		},
		[scrollResultsToTop],
	);
	const handleSearchSubmit = useCallback(
		(query: string) => {
			if (query === Discovery.query && !Discovery.error) {
				return;
			}
			void Discovery.search({query, offset: 0, limit: PAGE_SIZE});
			scrollResultsToTop();
		},
		[scrollResultsToTop],
	);
	const handleSearchClear = useCallback(() => {
		if (Discovery.query.length === 0) {
			return;
		}
		void Discovery.search({query: '', language: null, offset: 0, limit: PAGE_SIZE});
		scrollResultsToTop();
	}, [scrollResultsToTop]);
	const handleRetry = useCallback(() => {
		void Discovery.search({offset: 0, limit: PAGE_SIZE});
	}, []);
	useSkeletonLayoutReport(() => {
		if (searchActive || columns <= 0) {
			return;
		}
		const viewportHeightPx = measureSkeletonHeightPx(scrollerRef.current?.getViewportElement() ?? null);
		reportSkeletonDiscoveryGrid(columns, Math.ceil(viewportHeightPx / SKELETON_DISCOVERY_ESTIMATED_ROW_HEIGHT_PX));
	}, `${searchActive}:${columns}:${containerWidth}`);
	const virtualRows = virtualizer.getVirtualItems();
	const gridColumnsStyle =
		columns > 0
			? `repeat(${columns}, minmax(0, var(--discovery-card-max-width)))`
			: `repeat(auto-fill, minmax(${SKELETON_DISCOVERY_MIN_CARD_WIDTH_PX}px, var(--discovery-card-max-width)))`;
	const resultsTransitionKey = resolveDiscoveryResultsSurfaceKey(searchActive);
	const categoriesSettled = useCategoriesSettledLatch();
	const resultsSettled = useFirstResultsSettledLatch();
	const showPageSkeleton = !categoriesSettled || !resultsSettled;
	const resultsStatus =
		Discovery.loading && guilds.length === 0
			? i18n._(LOADING_COMMUNITIES_DESCRIPTOR)
			: i18n._(DISCOVERY_RESULTS_COUNT_DESCRIPTOR, {count: Discovery.total});
	if (showPageSkeleton) {
		return <DiscoveryPageSkeleton data-flx="discovery.discovery.discovery-page.discovery-page-skeleton" />;
	}
	return (
		<div
			className={styles.container}
			style={DISCOVERY_GRID_METRICS_STYLE}
			data-flx="discovery.discovery.discovery-page.container"
		>
			<DiscoveryNavbar
				resultsRegionId={DISCOVERY_RESULTS_REGION_ID}
				onCategorySelect={handleCategorySelect}
				onSearchSubmit={handleSearchSubmit}
				onSearchClear={handleSearchClear}
				onBack={searchActive ? handleSearchClear : null}
				data-flx="discovery.discovery.discovery-page.discovery-navbar"
			/>
			<Scroller ref={scrollerRef} data-flx="discovery.discovery.discovery-page.scroller">
				<div className={styles.scrollBody} data-flx="discovery.discovery.discovery-page.scroll-body">
					<div
						id={DISCOVERY_RESULTS_REGION_ID}
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
							data-flx="discovery.discovery.discovery-page.sr-only"
						>
							{resultsStatus}
						</div>
						<div
							className={searchActive ? styles.searchLayout : styles.browseLayout}
							data-flx="discovery.discovery.discovery-page.search-layout"
						>
							<div
								ref={attachResultsColumn}
								className={styles.resultsColumn}
								data-flx="discovery.discovery.discovery-page.results-column"
							>
								<DiscoveryTransition
									transitionKey={resultsTransitionKey}
									motionKind={DiscoveryMotionKind.RESULTS}
									className={styles.resultsSurface}
									data-flx="discovery.discovery.discovery-page.results-surface"
								>
									{Discovery.loading && guilds.length === 0 ? (
										<DiscoveryResultsSkeleton
											columnCount={columns}
											data-flx="discovery.discovery.discovery-page.discovery-results-skeleton"
										/>
									) : guilds.length > 0 && columns > 0 ? (
										<div
											className={styles.virtualGrid}
											role="list"
											aria-label={i18n._(DISCOVERY_RESULTS_DESCRIPTOR)}
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
														role="presentation"
														style={{
															transform: `translateY(${virtualRow.start - gridOffsetTop}px)`,
															gridTemplateColumns: gridColumnsStyle,
														}}
														data-flx="discovery.discovery.discovery-page.grid-row"
													>
														{rowGuilds.map((guild, columnIndex) => (
															<DiscoveryGuildCard
																key={guild.id}
																guild={guild}
																positionInSet={startIndex + columnIndex + 1}
																setSize={Discovery.total}
																data-flx="discovery.discovery.discovery-page.discovery-guild-card"
															/>
														))}
													</div>
												);
											})}
										</div>
									) : guilds.length === 0 && !Discovery.loading ? (
										<DiscoveryEmptyState
											onRetry={handleRetry}
											data-flx="discovery.discovery.discovery-page.discovery-empty-state"
										/>
									) : null}
								</DiscoveryTransition>
								{guilds.length > 0 && hasMore && Discovery.loading && (
									<div className={styles.loadingMore} data-flx="discovery.discovery.discovery-page.loading-more">
										<Spinner data-flx="discovery.discovery.discovery-page.spinner" />
									</div>
								)}
							</div>
							{searchActive && (
								<DiscoverySearchFilters
									onFilterApplied={scrollResultsToTop}
									data-flx="discovery.discovery.discovery-page.discovery-search-filters"
								/>
							)}
						</div>
					</div>
				</div>
			</Scroller>
		</div>
	);
});
