// SPDX-License-Identifier: AGPL-3.0-or-later

import guildStyles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {resolveGuildListIndicatorBarTarget} from '@app/features/app/components/layout/sidebar_nav/GuildListIndicator';
import styles from '@app/features/app/components/skeleton/GuildRailSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {
	getRememberedSkeletonGuildRailLayout,
	type RememberedSkeletonGuildRailItem,
	SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
	SkeletonGuildRailItemIndicator,
	SkeletonGuildRailItemKind,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {skeletonSurfaceVar} from '@app/features/app/components/skeleton/SkeletonSurfaceContract';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Platform} from '@app/features/platform/types/Platform';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import Dimension from '@app/features/ui/state/Dimension';
import {flxElementClassName} from '@app/lib/react';
import {
	ChatCircleIcon,
	CompassIcon,
	DownloadSimpleIcon,
	type Icon,
	type IconWeight,
	PlusIcon,
	QuestionMarkIcon,
	StarIcon,
} from '@phosphor-icons/react';
import type React from 'react';
import {useState} from 'react';

const GUILD_ICON_SIZE = skeletonSurfaceVar('--guild-icon-size');
const FOLDER_ICON_SIZE = skeletonSurfaceVar('--guild-list-item-box-size');
const MENTION_BADGE_SIZE = 'var(--guild-rail-skeleton-badge-size)';
const GUILD_PLACEHOLDER_COUNT = 6;
const SHOWS_DOWNLOAD_ACTION = !Platform.isElectron && !Platform.isPWA;
const FALLBACK_ORGANIZED_ITEMS: ReadonlyArray<RememberedSkeletonGuildRailItem> = Object.freeze(
	Array.from({length: GUILD_PLACEHOLDER_COUNT}, () =>
		Object.freeze({kind: SkeletonGuildRailItemKind.GUILD, indicator: SkeletonGuildRailItemIndicator.NONE} as const),
	),
);
const RAIL_ROW_PITCH_REM = 3.375;
const RAIL_DIVIDER_SLOT_REM = 0.8125;
const EMPTY_INLINE_DM_UNREAD_FLAGS: ReadonlyArray<boolean> = Object.freeze([]);

function countRailVisualRows(items: ReadonlyArray<RememberedSkeletonGuildRailItem>): number {
	let visualRows = 0;
	for (const item of items) {
		if (item.kind === SkeletonGuildRailItemKind.EXPANDED_FOLDER) {
			visualRows += item.childCount + 1;
			continue;
		}
		visualRows += 1;
	}
	return visualRows;
}

interface DrawableRailScrollTopRequest {
	readonly bottomButtonCount: number;
	readonly organizedItems: ReadonlyArray<RememberedSkeletonGuildRailItem>;
	readonly outageVisible: boolean;
	readonly remScale: number;
	readonly scrollTopPx: number;
	readonly topRowCount: number;
	readonly viewportPx: number;
}

function resolveDrawableRailScrollTopPx({
	bottomButtonCount,
	organizedItems,
	outageVisible,
	remScale,
	scrollTopPx,
	topRowCount,
	viewportPx,
}: DrawableRailScrollTopRequest): number {
	if (scrollTopPx <= 0) {
		return 0;
	}
	const outageRows = outageVisible ? 1 : 0;
	const visualRows = topRowCount + outageRows + countRailVisualRows(organizedItems) + bottomButtonCount;
	const drawableContentPx = (visualRows * RAIL_ROW_PITCH_REM + RAIL_DIVIDER_SLOT_REM) * remScale;
	return Math.min(scrollTopPx, Math.max(0, drawableContentPx - viewportPx));
}

const GuildRailSkeletonIndicator = ({selected}: {readonly selected: boolean}) => (
	<flx-app-guild-rail-skeleton-indicator
		className={flxElementClassName(guildStyles.guildIndicator)}
		data-flx="app.skeleton.guild-rail-skeleton.guild-rail-skeleton-indicator.flx-app-guild-rail-skeleton-indicator"
	>
		<span
			className={guildStyles.guildIndicatorBar}
			style={resolveGuildListIndicatorBarTarget({isSelected: selected, showHoverState: false})}
			data-flx="app.skeleton.guild-rail-skeleton.guild-rail-skeleton-indicator.span"
		/>
	</flx-app-guild-rail-skeleton-indicator>
);

const InlineDMPlaceholder = ({selected, unread}: {readonly selected: boolean; readonly unread: boolean}) => (
	<flx-app-guild-rail-skeleton-item
		className={flxElementClassName(styles.item)}
		data-selected={selectedAttribute(selected)}
		data-flx="app.skeleton.guild-rail-skeleton.inline-dm-placeholder.item"
	>
		{(selected || unread) && (
			<GuildRailSkeletonIndicator
				selected={selected}
				data-flx="app.skeleton.guild-rail-skeleton.inline-dm-placeholder.guild-rail-skeleton-indicator"
			/>
		)}
		<SkeletonCircle
			size={GUILD_ICON_SIZE}
			emphasis={SkeletonEmphasis.DEFAULT}
			data-flx="app.skeleton.guild-rail-skeleton.inline-dm-placeholder.skeleton-circle"
		/>
	</flx-app-guild-rail-skeleton-item>
);

const RailActionButton = ({
	icon: Icon,
	selected = false,
	weight = 'bold',
}: {
	readonly icon: Icon;
	readonly selected?: boolean;
	readonly weight?: IconWeight;
}) => (
	<flx-app-guild-rail-skeleton-item
		className={flxElementClassName(styles.item)}
		data-selected={selectedAttribute(selected)}
		data-flx="app.skeleton.guild-rail-skeleton.rail-action-button.item"
	>
		<flx-app-guild-rail-skeleton-action-icon
			className={flxElementClassName(styles.actionIcon)}
			data-flx="app.skeleton.guild-rail-skeleton.rail-action-button.action-icon"
		>
			<Icon
				weight={weight}
				className={styles.actionIconGlyph}
				data-flx="app.skeleton.guild-rail-skeleton.rail-action-button.icon"
			/>
		</flx-app-guild-rail-skeleton-action-icon>
	</flx-app-guild-rail-skeleton-item>
);

const FavoritesButtonPlaceholder = ({selected}: {readonly selected: boolean}) => (
	<flx-app-guild-rail-skeleton-item
		className={flxElementClassName(styles.item)}
		data-selected={selectedAttribute(selected)}
		data-flx="app.skeleton.guild-rail-skeleton.favorites-button-placeholder.item"
	>
		{selected && (
			<GuildRailSkeletonIndicator
				selected={true}
				data-flx="app.skeleton.guild-rail-skeleton.favorites-button-placeholder.guild-rail-skeleton-indicator"
			/>
		)}
		<flx-app-guild-rail-skeleton-fluxer-icon
			className={flxElementClassName(styles.fluxerIcon)}
			data-flx="app.skeleton.guild-rail-skeleton.favorites-button-placeholder.icon-surface"
		>
			<StarIcon
				weight="fill"
				className={styles.favoritesIconGlyph}
				data-flx="app.skeleton.guild-rail-skeleton.favorites-button-placeholder.icon"
			/>
		</flx-app-guild-rail-skeleton-fluxer-icon>
	</flx-app-guild-rail-skeleton-item>
);

function selectedAttribute(selected: boolean): string | undefined {
	if (selected) {
		return 'true';
	}
	return undefined;
}

interface SkeletonIndicatorVisibilityRequest {
	readonly indicator: SkeletonGuildRailItemIndicator;
	readonly selected: boolean;
}

function showsSkeletonIndicator({indicator, selected}: SkeletonIndicatorVisibilityRequest): boolean {
	if (selected) {
		return true;
	}
	return indicator !== SkeletonGuildRailItemIndicator.NONE;
}

const MentionBadgePlaceholder = () => (
	<SkeletonBlock
		width={MENTION_BADGE_SIZE}
		height={MENTION_BADGE_SIZE}
		radius={SkeletonRadius.MEDIUM}
		emphasis={SkeletonEmphasis.MUTED}
		className={styles.mentionBadge}
		data-flx="app.skeleton.guild-rail-skeleton.mention-badge-placeholder.mention-badge"
	/>
);

const GuildPlaceholder = ({
	indicator,
	selected,
}: {
	readonly indicator: SkeletonGuildRailItemIndicator;
	readonly selected: boolean;
}) => (
	<flx-app-guild-rail-skeleton-item-slot
		className={flxElementClassName(styles.itemSlot)}
		data-selected={selectedAttribute(selected)}
		data-flx="app.skeleton.guild-rail-skeleton.guild-placeholder.item-slot"
	>
		{showsSkeletonIndicator({indicator, selected}) && (
			<GuildRailSkeletonIndicator
				selected={selected}
				data-flx="app.skeleton.guild-rail-skeleton.guild-placeholder.guild-rail-skeleton-indicator"
			/>
		)}
		<SkeletonCircle
			size={GUILD_ICON_SIZE}
			data-flx="app.skeleton.guild-rail-skeleton.guild-placeholder.skeleton-circle"
		/>
		{indicator === SkeletonGuildRailItemIndicator.MENTION && (
			<MentionBadgePlaceholder data-flx="app.skeleton.guild-rail-skeleton.guild-placeholder.mention-badge-placeholder" />
		)}
	</flx-app-guild-rail-skeleton-item-slot>
);

const FluxerButtonPlaceholder = ({selected}: {readonly selected: boolean}) => (
	<flx-app-guild-rail-skeleton-item
		className={flxElementClassName(styles.item)}
		data-selected={selectedAttribute(selected)}
		data-flx="app.skeleton.guild-rail-skeleton.fluxer-button-placeholder.item"
	>
		{selected && (
			<GuildRailSkeletonIndicator
				selected={true}
				data-flx="app.skeleton.guild-rail-skeleton.fluxer-button-placeholder.guild-rail-skeleton-indicator"
			/>
		)}
		<flx-app-guild-rail-skeleton-fluxer-icon
			className={flxElementClassName(styles.fluxerIcon)}
			data-flx="app.skeleton.guild-rail-skeleton.fluxer-button-placeholder.fluxer-icon"
		>
			<ChatCircleIcon
				weight="fill"
				className={styles.fluxerIconGlyph}
				data-flx="app.skeleton.guild-rail-skeleton.fluxer-button-placeholder.fluxer-icon-glyph"
			/>
		</flx-app-guild-rail-skeleton-fluxer-icon>
	</flx-app-guild-rail-skeleton-item>
);

const OutagePlaceholder = () => (
	<flx-app-guild-rail-skeleton-item-slot
		className={flxElementClassName(styles.itemSlot, styles.outageSlot)}
		data-flx="app.skeleton.guild-rail-skeleton.outage-placeholder.item-slot"
	>
		<SkeletonCircle
			size={GUILD_ICON_SIZE}
			emphasis={SkeletonEmphasis.MUTED}
			data-flx="app.skeleton.guild-rail-skeleton.outage-placeholder.skeleton-circle"
		/>
	</flx-app-guild-rail-skeleton-item-slot>
);

const CollapsedFolderPlaceholder = ({
	childCount,
	showIconWhenCollapsed,
	indicator,
	selected,
}: {
	readonly childCount: number;
	readonly showIconWhenCollapsed: boolean;
	readonly indicator: SkeletonGuildRailItemIndicator;
	readonly selected: boolean;
}) => (
	<flx-app-guild-rail-skeleton-item-slot
		className={flxElementClassName(styles.itemSlot, styles.folderSlot)}
		data-selected={selectedAttribute(selected)}
		data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.item-slot"
	>
		{showsSkeletonIndicator({indicator, selected}) && (
			<GuildRailSkeletonIndicator
				selected={selected}
				data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.guild-rail-skeleton-indicator"
			/>
		)}
		<flx-app-guild-rail-skeleton-collapsed-folder
			className={flxElementClassName(styles.collapsedFolder)}
			data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.collapsed-folder"
		>
			<SkeletonBlock
				width={FOLDER_ICON_SIZE}
				height={FOLDER_ICON_SIZE}
				radius={SkeletonRadius.SHARP}
				emphasis={SkeletonEmphasis.MUTED}
				className={styles.collapsedFolderBackground}
				data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.collapsed-folder-background"
			/>
			{showIconWhenCollapsed ? (
				<SkeletonBlock
					width="1.5rem"
					height="1.5rem"
					radius={SkeletonRadius.MEDIUM}
					data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.skeleton-block"
				/>
			) : (
				<flx-app-guild-rail-skeleton-collapsed-folder-grid
					className={flxElementClassName(styles.collapsedFolderGrid)}
					data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.collapsed-folder-grid"
				>
					{Array.from({length: childCount}, (_, index) => (
						<SkeletonBlock
							key={index}
							width="100%"
							height="100%"
							radius={SkeletonRadius.SHARP}
							className={styles.collapsedFolderMini}
							data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.collapsed-folder-mini"
						/>
					))}
				</flx-app-guild-rail-skeleton-collapsed-folder-grid>
			)}
		</flx-app-guild-rail-skeleton-collapsed-folder>
		{indicator === SkeletonGuildRailItemIndicator.MENTION && (
			<MentionBadgePlaceholder data-flx="app.skeleton.guild-rail-skeleton.collapsed-folder-placeholder.mention-badge-placeholder" />
		)}
	</flx-app-guild-rail-skeleton-item-slot>
);

const ExpandedFolderGuildPlaceholder = ({
	indicator,
	selected,
}: {
	readonly indicator: SkeletonGuildRailItemIndicator;
	readonly selected: boolean;
}) => (
	<flx-app-guild-rail-skeleton-expanded-folder-guild
		className={flxElementClassName(styles.expandedFolderGuild)}
		data-selected={selectedAttribute(selected)}
		data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-guild-placeholder.expanded-folder-guild"
	>
		{showsSkeletonIndicator({indicator, selected}) && (
			<GuildRailSkeletonIndicator
				selected={selected}
				data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-guild-placeholder.guild-rail-skeleton-indicator"
			/>
		)}
		<SkeletonCircle
			size={GUILD_ICON_SIZE}
			data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-guild-placeholder.skeleton-circle"
		/>
		{indicator === SkeletonGuildRailItemIndicator.MENTION && (
			<MentionBadgePlaceholder data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-guild-placeholder.mention-badge-placeholder" />
		)}
	</flx-app-guild-rail-skeleton-expanded-folder-guild>
);

const ExpandedFolderPlaceholder = ({
	childCount,
	childIndicators,
	selectedChildIndex,
}: {
	readonly childCount: number;
	readonly childIndicators: ReadonlyArray<SkeletonGuildRailItemIndicator>;
	readonly selectedChildIndex: number;
}) => (
	<flx-app-guild-rail-skeleton-item-slot
		className={flxElementClassName(styles.expandedFolderSlot)}
		data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-placeholder.expanded-folder-slot"
	>
		<flx-app-guild-rail-skeleton-expanded-folder
			className={flxElementClassName(styles.expandedFolder)}
			data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-placeholder.expanded-folder"
		>
			<SkeletonBlock
				width={`calc(${skeletonSurfaceVar('--guild-folder-expanded-surface-size')} + 0.25rem)`}
				radius={SkeletonRadius.SHARP}
				emphasis={SkeletonEmphasis.MUTED}
				className={styles.expandedFolderBackground}
				data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-placeholder.expanded-folder-background"
			/>
			<flx-app-guild-rail-skeleton-expanded-folder-header
				className={flxElementClassName(styles.expandedFolderHeader)}
				data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-placeholder.expanded-folder-header"
			>
				<SkeletonBlock
					width={FOLDER_ICON_SIZE}
					height={FOLDER_ICON_SIZE}
					radius={SkeletonRadius.SHARP}
					className={styles.expandedFolderHeaderSurface}
					data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-placeholder.expanded-folder-header-surface"
				/>
			</flx-app-guild-rail-skeleton-expanded-folder-header>
			<flx-app-guild-rail-skeleton-expanded-folder-guilds
				className={flxElementClassName(styles.expandedFolderGuilds)}
				data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-placeholder.expanded-folder-guilds"
			>
				{Array.from({length: childCount}, (_, index) => (
					<ExpandedFolderGuildPlaceholder
						key={index}
						indicator={childIndicators[index] ?? SkeletonGuildRailItemIndicator.NONE}
						selected={index === selectedChildIndex}
						data-flx="app.skeleton.guild-rail-skeleton.expanded-folder-placeholder.expanded-folder-guild-placeholder"
					/>
				))}
			</flx-app-guild-rail-skeleton-expanded-folder-guilds>
		</flx-app-guild-rail-skeleton-expanded-folder>
	</flx-app-guild-rail-skeleton-item-slot>
);

function renderOrganizedItem(
	item: RememberedSkeletonGuildRailItem,
	index: number,
	selectedItemIndex: number,
): React.ReactNode {
	const selected = index === selectedItemIndex;
	switch (item.kind) {
		case SkeletonGuildRailItemKind.GUILD:
			return (
				<GuildPlaceholder
					key={index}
					indicator={item.indicator}
					selected={selected}
					data-flx="app.skeleton.guild-rail-skeleton.render-organized-item.guild-placeholder"
				/>
			);
		case SkeletonGuildRailItemKind.COLLAPSED_FOLDER:
			return (
				<CollapsedFolderPlaceholder
					key={index}
					childCount={item.childCount}
					showIconWhenCollapsed={item.showIconWhenCollapsed}
					indicator={item.indicator}
					selected={selected}
					data-flx="app.skeleton.guild-rail-skeleton.render-organized-item.collapsed-folder-placeholder"
				/>
			);
		case SkeletonGuildRailItemKind.EXPANDED_FOLDER:
			return (
				<ExpandedFolderPlaceholder
					key={index}
					childCount={item.childCount}
					childIndicators={item.childIndicators}
					selectedChildIndex={item.selectedChildIndex}
					data-flx="app.skeleton.guild-rail-skeleton.render-organized-item.expanded-folder-placeholder"
				/>
			);
	}
}

interface GuildRailSkeletonProps {
	readonly isFluxerSelected: boolean;
	readonly isFavoritesSelected: boolean;
	readonly isDiscoverySelected: boolean;
}

export const GuildRailSkeleton: React.FC<GuildRailSkeletonProps> = ({
	isFluxerSelected,
	isFavoritesSelected,
	isDiscoverySelected,
}) => {
	const [mountState] = useState(() => {
		const rememberedLayout = getRememberedSkeletonGuildRailLayout();
		const remScale = getRemScaleForDocument(document);
		const liveScrollTop = Math.max(0, Dimension.getGuildListDimensions().scrollTop);
		let scrollTop = liveScrollTop;
		if (liveScrollTop === 0 && rememberedLayout != null) {
			scrollTop = rememberedLayout.scrollTopPx * remScale;
		}
		return Object.freeze({
			rememberedLayout,
			remScale,
			scrollTop,
			viewportPx: document.documentElement.clientHeight,
		});
	});
	const {rememberedLayout} = mountState;
	const communityActionsAvailable = !RuntimeConfig.singleCommunityEnabled;
	const inlineDmRowCount = rememberedLayout?.inlineDmRowCount ?? 0;
	const inlineDmUnreadFlags = rememberedLayout?.inlineDmUnreadFlags ?? EMPTY_INLINE_DM_UNREAD_FLAGS;
	const selectedInlineDmRowIndex = rememberedLayout?.selectedInlineDmRowIndex ?? SKELETON_NO_SELECTED_RAIL_ITEM_INDEX;
	const inlineDmPlaceholders = Array.from({length: inlineDmRowCount}, (_, index) => index);
	const outageVisible = rememberedLayout?.outageVisible ?? false;
	const organizedItems = rememberedLayout?.organizedItems ?? FALLBACK_ORGANIZED_ITEMS;
	const selectedItemIndex = rememberedLayout?.selectedItemIndex ?? SKELETON_NO_SELECTED_RAIL_ITEM_INDEX;
	const fluxerVisible = rememberedLayout?.fluxerVisible ?? !RuntimeConfig.directMessagesDisabled;
	const favoritesVisible = rememberedLayout?.favoritesVisible ?? true;
	const discoveryVisible = rememberedLayout?.discoveryVisible ?? communityActionsAvailable;
	const addGuildVisible = rememberedLayout?.addGuildVisible ?? communityActionsAvailable;
	const downloadVisible = rememberedLayout?.downloadVisible ?? SHOWS_DOWNLOAD_ACTION;
	const helpVisible = rememberedLayout?.helpVisible ?? true;
	const hasGuildItems = outageVisible || organizedItems.length > 0;
	const fluxerIsLastTopRow = fluxerVisible && !favoritesVisible && inlineDmRowCount === 0;
	const hasBottomRailButtons = discoveryVisible || addGuildVisible || downloadVisible || helpVisible;
	const itemsEndWithoutGap = outageVisible && organizedItems.length === 0 && hasBottomRailButtons;
	let guildsSectionTrailingGap: boolean;
	if (helpVisible) {
		guildsSectionTrailingGap = false;
	} else if (discoveryVisible || addGuildVisible || downloadVisible) {
		guildsSectionTrailingGap = true;
	} else {
		guildsSectionTrailingGap = organizedItems.length > 0;
	}
	const topRowCount = (fluxerVisible ? 1 : 0) + (favoritesVisible ? 1 : 0) + inlineDmRowCount;
	const bottomButtonCount =
		(discoveryVisible ? 1 : 0) + (addGuildVisible ? 1 : 0) + (downloadVisible ? 1 : 0) + (helpVisible ? 1 : 0);
	const scrollTop = resolveDrawableRailScrollTopPx({
		bottomButtonCount,
		organizedItems,
		outageVisible,
		remScale: mountState.remScale,
		scrollTopPx: mountState.scrollTop,
		topRowCount,
		viewportPx: mountState.viewportPx,
	});
	let contentStyle: React.CSSProperties | undefined;
	if (scrollTop > 0) {
		contentStyle = {marginTop: `${-scrollTop}px`};
	}
	return (
		<flx-app-guild-rail-skeleton
			className={flxElementClassName(styles.rail)}
			aria-hidden
			data-flx="app.skeleton.guild-rail-skeleton.rail"
		>
			<flx-app-guild-rail-skeleton-scroller
				className={flxElementClassName(styles.scroller)}
				data-flx="app.skeleton.guild-rail-skeleton.scroller"
			>
				<flx-app-guild-rail-skeleton-content
					className={flxElementClassName(styles.content)}
					style={contentStyle}
					data-flx="app.skeleton.guild-rail-skeleton.content"
				>
					<flx-app-guild-rail-skeleton-top-section
						className={flxElementClassName(styles.section, fluxerIsLastTopRow && styles.sectionTrailingGap)}
						data-flx="app.skeleton.guild-rail-skeleton.section"
					>
						{fluxerVisible && (
							<FluxerButtonPlaceholder
								selected={isFluxerSelected}
								data-flx="app.skeleton.guild-rail-skeleton.fluxer-button-placeholder"
							/>
						)}
						{favoritesVisible && (
							<FavoritesButtonPlaceholder
								selected={isFavoritesSelected}
								data-flx="app.skeleton.guild-rail-skeleton.favorites-button-placeholder"
							/>
						)}
						{inlineDmPlaceholders.map((index) => (
							<InlineDMPlaceholder
								key={index}
								selected={index === selectedInlineDmRowIndex}
								unread={inlineDmUnreadFlags[index] === true}
								data-flx="app.skeleton.guild-rail-skeleton.inline-dm-placeholder"
							/>
						))}
					</flx-app-guild-rail-skeleton-top-section>
					<flx-app-guild-rail-skeleton-divider-slot
						className={flxElementClassName(styles.dividerSlot, hasGuildItems && styles.dividerSlotExtended)}
						data-flx="app.skeleton.guild-rail-skeleton.divider-slot"
					>
						<flx-app-guild-rail-skeleton-divider
							className={flxElementClassName(styles.divider)}
							data-flx="app.skeleton.guild-rail-skeleton.divider"
						/>
					</flx-app-guild-rail-skeleton-divider-slot>
					<flx-app-guild-rail-skeleton-guilds-section
						className={flxElementClassName(styles.section, guildsSectionTrailingGap && styles.sectionTrailingGap)}
						data-flx="app.skeleton.guild-rail-skeleton.section--2"
					>
						{hasGuildItems && (
							<flx-app-guild-rail-skeleton-items
								className={flxElementClassName(styles.items, itemsEndWithoutGap && styles.itemsTrailingGapCancel)}
								data-flx="app.skeleton.guild-rail-skeleton.items"
							>
								{outageVisible && <OutagePlaceholder data-flx="app.skeleton.guild-rail-skeleton.outage-placeholder" />}
								{organizedItems.map((item, index) => renderOrganizedItem(item, index, selectedItemIndex))}
							</flx-app-guild-rail-skeleton-items>
						)}
						{discoveryVisible && (
							<RailActionButton
								icon={CompassIcon}
								weight="fill"
								selected={isDiscoverySelected}
								data-flx="app.skeleton.guild-rail-skeleton.rail-action-button"
							/>
						)}
						{addGuildVisible && (
							<RailActionButton icon={PlusIcon} data-flx="app.skeleton.guild-rail-skeleton.rail-action-button--2" />
						)}
						{downloadVisible && (
							<RailActionButton
								icon={DownloadSimpleIcon}
								data-flx="app.skeleton.guild-rail-skeleton.rail-action-button--3"
							/>
						)}
						{helpVisible && (
							<RailActionButton
								icon={QuestionMarkIcon}
								data-flx="app.skeleton.guild-rail-skeleton.rail-action-button--4"
							/>
						)}
					</flx-app-guild-rail-skeleton-guilds-section>
				</flx-app-guild-rail-skeleton-content>
			</flx-app-guild-rail-skeleton-scroller>
		</flx-app-guild-rail-skeleton>
	);
};
