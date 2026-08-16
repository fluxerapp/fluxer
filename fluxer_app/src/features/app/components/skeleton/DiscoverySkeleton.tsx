// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/DiscoverySkeleton.module.css';
import {SidebarShellSkeleton} from '@app/features/app/components/skeleton/SidebarShellSkeleton';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {
	getRememberedSkeletonDiscoveryLayout,
	resolveDefaultSkeletonDiscoveryLayout,
	SKELETON_UNMEASURED_WIDTH_PX,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {DISCOVERY_GRID_METRICS_STYLE} from '@app/features/discovery/discovery/DiscoveryGridMetrics';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {flxElementClassName} from '@app/lib/react';
import {useMemo, useState} from 'react';

interface DiscoveryNavItemSpec {
	labelWidth: string;
	comingSoon: boolean;
}

const NAV_ITEMS: ReadonlyArray<DiscoveryNavItemSpec> = [
	{labelWidth: '6.5rem', comingSoon: false},
	{labelWidth: '3.5rem', comingSoon: true},
	{labelWidth: '4.5rem', comingSoon: true},
];
const NAV_ICON_SIZE = '1.25rem';
const NAV_LABEL_HEIGHT = '0.875rem';
const NAV_BADGE_WIDTH = '5.5rem';
const NAV_BADGE_HEIGHT = '1.25rem';
const CATEGORY_TAB_FALLBACK_WIDTH = '3rem';
const CATEGORY_TAB_HEIGHT = '2rem';
const SEARCH_WIDTH = '100%';
const SEARCH_HEIGHT = '2.25rem';
const CARD_BANNER_HEIGHT = '7.5rem';
const CARD_ICON_SIZE = '3.5rem';
const CARD_NAME_HEIGHT = '0.875rem';
const CARD_DESCRIPTION_HEIGHT = '0.75rem';
const CARD_DESCRIPTION_LINE_COUNT = 4;
const CARD_STAT_HEIGHT = '0.625rem';
const CARD_STAT_DOT_SIZE = '0.5rem';
const CARD_NAME_WIDTH_MIN = 40;
const CARD_NAME_WIDTH_RANGE = 45;
const CARD_DESCRIPTION_WIDTH_MIN = 62;
const CARD_DESCRIPTION_WIDTH_RANGE = 38;
const CARD_LAST_LINE_WIDTH_MIN = 30;
const CARD_LAST_LINE_WIDTH_RANGE = 35;
const CARD_STAT_WIDTH_MIN = 3.5;
const CARD_STAT_WIDTH_RANGE = 2;

interface DiscoveryCardSpec {
	nameWidth: string;
	descriptionWidths: ReadonlyArray<string>;
	statWidths: ReadonlyArray<string>;
}

class DiscoverySkeletonSpecOwner {
	private readonly random = createSkeletonRandomFromKey('discovery-skeleton-cards');

	public createCards(cardCount: number): ReadonlyArray<DiscoveryCardSpec> {
		return Array.from({length: cardCount}, () => this.createCard());
	}

	private createCard(): DiscoveryCardSpec {
		return {
			nameWidth: `${CARD_NAME_WIDTH_MIN + this.random() * CARD_NAME_WIDTH_RANGE}%`,
			descriptionWidths: Array.from(Array(CARD_DESCRIPTION_LINE_COUNT).keys(), (lineIndex) =>
				this.createDescriptionWidth(lineIndex),
			),
			statWidths: [
				`${CARD_STAT_WIDTH_MIN + this.random() * CARD_STAT_WIDTH_RANGE}rem`,
				`${CARD_STAT_WIDTH_MIN + this.random() * CARD_STAT_WIDTH_RANGE}rem`,
			],
		};
	}

	private createDescriptionWidth(lineIndex: number): string {
		if (lineIndex === CARD_DESCRIPTION_LINE_COUNT - 1) {
			return `${CARD_LAST_LINE_WIDTH_MIN + this.random() * CARD_LAST_LINE_WIDTH_RANGE}%`;
		}
		return `${CARD_DESCRIPTION_WIDTH_MIN + this.random() * CARD_DESCRIPTION_WIDTH_RANGE}%`;
	}
}

function resolveCategoryTabWidths(widthsPx: ReadonlyArray<number>): ReadonlyArray<string> {
	return widthsPx.map((widthPx) => {
		if (widthPx === SKELETON_UNMEASURED_WIDTH_PX) {
			return CATEGORY_TAB_FALLBACK_WIDTH;
		}
		return remFromPx(widthPx);
	});
}

interface DiscoveryNavSkeletonItemProps {
	readonly item: DiscoveryNavItemSpec;
	readonly active: boolean;
}

function DiscoveryNavSkeletonItem({item, active}: DiscoveryNavSkeletonItemProps) {
	return (
		<div
			className={flxElementClassName(styles.navItem, active && styles.navItemActive)}
			data-flx="app.skeleton.discovery-skeleton.discovery-nav-skeleton-item.nav-item"
		>
			<SkeletonBlock
				width={NAV_ICON_SIZE}
				height={NAV_ICON_SIZE}
				radius={SkeletonRadius.SMALL}
				data-flx="app.skeleton.discovery-skeleton.discovery-nav-skeleton-item.skeleton-block"
			/>
			<div
				className={flxElementClassName(styles.navItemLabel)}
				data-flx="app.skeleton.discovery-skeleton.discovery-nav-skeleton-item.nav-item-label"
			>
				<SkeletonLine
					width={item.labelWidth}
					height={NAV_LABEL_HEIGHT}
					data-flx="app.skeleton.discovery-skeleton.discovery-nav-skeleton-item.skeleton-line"
				/>
			</div>
			{item.comingSoon && (
				<SkeletonBlock
					width={NAV_BADGE_WIDTH}
					height={NAV_BADGE_HEIGHT}
					radius={SkeletonRadius.SMALL}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.discovery-skeleton.discovery-nav-skeleton-item.skeleton-block--2"
				/>
			)}
		</div>
	);
}

interface DiscoveryCardStatSkeletonProps {
	readonly cardIndex: number;
	readonly statIndex: number;
	readonly width: string;
}

function DiscoveryCardStatSkeleton({cardIndex, statIndex, width}: DiscoveryCardStatSkeletonProps) {
	return (
		<div
			key={`discovery-card-${cardIndex}-stat-${statIndex}`}
			className={flxElementClassName(styles.cardStat)}
			data-flx="app.skeleton.discovery-skeleton.discovery-card-stat-skeleton.card-stat"
		>
			<SkeletonCircle
				size={CARD_STAT_DOT_SIZE}
				emphasis={SkeletonEmphasis.MUTED}
				data-flx="app.skeleton.discovery-skeleton.discovery-card-stat-skeleton.skeleton-circle"
			/>
			<SkeletonLine
				width={width}
				height={CARD_STAT_HEIGHT}
				emphasis={SkeletonEmphasis.MUTED}
				data-flx="app.skeleton.discovery-skeleton.discovery-card-stat-skeleton.skeleton-line"
			/>
		</div>
	);
}

interface DiscoveryCardSkeletonProps {
	readonly card: DiscoveryCardSpec;
	readonly cardIndex: number;
}

function DiscoveryCardSkeleton({card, cardIndex}: DiscoveryCardSkeletonProps) {
	return (
		<div
			className={flxElementClassName(styles.card)}
			data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card"
		>
			<div
				className={flxElementClassName(styles.cardBanner)}
				data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card-banner"
			>
				<SkeletonBlock
					height={CARD_BANNER_HEIGHT}
					radius={SkeletonRadius.SHARP}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.skeleton-block"
				/>
			</div>
			<div
				className={flxElementClassName(styles.cardIcon)}
				data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card-icon"
			>
				<SkeletonCircle
					size={CARD_ICON_SIZE}
					emphasis={SkeletonEmphasis.STRONG}
					data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.skeleton-circle"
				/>
			</div>
			<div
				className={flxElementClassName(styles.cardBody)}
				data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card-body"
			>
				<div
					className={flxElementClassName(styles.cardTitle)}
					data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card-title"
				>
					<SkeletonLine
						width={card.nameWidth}
						height={CARD_NAME_HEIGHT}
						emphasis={SkeletonEmphasis.STRONG}
						data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.skeleton-line"
					/>
				</div>
				<div
					className={flxElementClassName(styles.cardDescription)}
					data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card-description"
				>
					{card.descriptionWidths.map((lineWidth, lineIndex) => (
						<SkeletonLine
							key={`discovery-card-${cardIndex}-line-${lineIndex}`}
							width={lineWidth}
							height={CARD_DESCRIPTION_HEIGHT}
							data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.skeleton-line--2"
						/>
					))}
				</div>
			</div>
			<div
				className={flxElementClassName(styles.cardFooter)}
				data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card-footer"
			>
				<div
					className={flxElementClassName(styles.cardStats)}
					data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.card-stats"
				>
					{card.statWidths.map((statWidth, statIndex) => (
						<DiscoveryCardStatSkeleton
							key={statIndex}
							cardIndex={cardIndex}
							statIndex={statIndex}
							width={statWidth}
							data-flx="app.skeleton.discovery-skeleton.discovery-card-skeleton.discovery-card-stat-skeleton"
						/>
					))}
				</div>
			</div>
		</div>
	);
}

export const DiscoverySidebarSkeleton = () => (
	<SidebarShellSkeleton data-flx="app.skeleton.discovery-skeleton.discovery-sidebar-skeleton.sidebar-shell-skeleton">
		<div
			className={flxElementClassName(styles.navList)}
			data-flx="app.skeleton.discovery-skeleton.discovery-sidebar-skeleton.nav-list"
		>
			{NAV_ITEMS.map((item, itemIndex) => (
				<DiscoveryNavSkeletonItem
					key={item.labelWidth}
					item={item}
					active={itemIndex === 0}
					data-flx="app.skeleton.discovery-skeleton.discovery-sidebar-skeleton.discovery-nav-skeleton-item"
				/>
			))}
		</div>
	</SidebarShellSkeleton>
);

interface DiscoveryResultsSkeletonProps {
	readonly columnCount?: number | null;
}

export const DiscoveryResultsSkeleton = ({columnCount = null}: DiscoveryResultsSkeletonProps = {}) => {
	const [layout] = useState(() => getRememberedSkeletonDiscoveryLayout() ?? resolveDefaultSkeletonDiscoveryLayout());
	let resolvedColumnCount: number;
	if (columnCount != null && columnCount > 0) {
		resolvedColumnCount = columnCount;
	} else {
		resolvedColumnCount = layout.columnCount;
	}
	const cardCount = resolvedColumnCount * layout.visibleRowCount;
	const cards = useMemo<ReadonlyArray<DiscoveryCardSpec>>(
		() => new DiscoverySkeletonSpecOwner().createCards(cardCount),
		[cardCount],
	);
	return (
		<div
			className={flxElementClassName(styles.grid)}
			style={{gridTemplateColumns: `repeat(${resolvedColumnCount}, minmax(0, var(--discovery-card-max-width)))`}}
			aria-hidden
			data-flx="app.skeleton.discovery-skeleton.discovery-results-skeleton.grid"
		>
			{cards.map((card, cardIndex) => (
				<DiscoveryCardSkeleton
					key={cardIndex}
					card={card}
					cardIndex={cardIndex}
					data-flx="app.skeleton.discovery-skeleton.discovery-results-skeleton.discovery-card-skeleton"
				/>
			))}
		</div>
	);
};

export const DiscoveryPageSkeleton = () => {
	const [layout] = useState(() => getRememberedSkeletonDiscoveryLayout() ?? resolveDefaultSkeletonDiscoveryLayout());
	const categoryTabWidths = useMemo(
		() => resolveCategoryTabWidths(layout.categoryTabWidthsPx),
		[layout.categoryTabWidthsPx],
	);
	return (
		<div
			className={flxElementClassName(styles.container)}
			style={DISCOVERY_GRID_METRICS_STYLE}
			aria-hidden
			data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.container"
		>
			<div
				className={flxElementClassName(styles.navbar)}
				data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.navbar"
			>
				<div
					className={flxElementClassName(styles.categoryTabs)}
					data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.category-tabs"
				>
					{categoryTabWidths.map((tabWidth, tabIndex) => (
						<SkeletonBlock
							key={tabIndex}
							width={tabWidth}
							height={CATEGORY_TAB_HEIGHT}
							radius={SkeletonRadius.MEDIUM}
							data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.skeleton-block"
						/>
					))}
				</div>
				<div
					className={flxElementClassName(styles.navbarSearch)}
					data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.navbar-search"
				>
					<SkeletonBlock
						width={SEARCH_WIDTH}
						height={SEARCH_HEIGHT}
						radius={SkeletonRadius.EXTRA_LARGE}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.skeleton-block--2"
					/>
				</div>
			</div>
			<div
				className={flxElementClassName(styles.scrollBody)}
				data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.scroll-body"
			>
				<div
					className={flxElementClassName(styles.content)}
					data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.content"
				>
					<DiscoveryResultsSkeleton data-flx="app.skeleton.discovery-skeleton.discovery-page-skeleton.discovery-results-skeleton" />
				</div>
			</div>
		</div>
	);
};
