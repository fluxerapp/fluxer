// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import styles from '@app/features/channel/components/EmojiPicker.module.css';
import gifStyles from '@app/features/channel/components/GifPicker.module.css';
import {PremiumUpsellBanner} from '@app/features/channel/components/PremiumUpsellBanner';
import premiumStyles from '@app/features/channel/components/PremiumUpsellBanner.module.css';
import {useScrollerViewport} from '@app/features/channel/components/pickers/shared/useScrollerViewport';
import {PickerEmptyState} from '@app/features/channel/components/shared/PickerEmptyState';
import {useStickerCategories} from '@app/features/channel/components/sticker_picker/hooks/useStickerCategories';
import {useVirtualRows} from '@app/features/channel/components/sticker_picker/hooks/useVirtualRows';
import {StickerPickerCategoryList} from '@app/features/channel/components/sticker_picker/StickerPickerCategoryList';
import {
	buildStickerRowOffsets,
	getStickerRowHeight,
	getStickerRowWindow,
	STICKER_SECTION_GAP,
	STICKERS_PER_ROW,
	type StickerRowKind,
} from '@app/features/channel/components/sticker_picker/StickerPickerConstants';
import {StickerPickerInspector} from '@app/features/channel/components/sticker_picker/StickerPickerInspector';
import {StickerPickerSearchBar} from '@app/features/channel/components/sticker_picker/StickerPickerSearchBar';
import {VirtualRowWrapper} from '@app/features/channel/components/sticker_picker/VirtualRow';
import Channels from '@app/features/channel/state/Channels';
import * as StickerPickerCommands from '@app/features/emoji/commands/StickerPickerCommands';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import {ExpressionPickerHeaderPortal} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {
	checkStickerAvailability,
	shouldShowStickerPremiumUpsell,
} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {usePremiumUpsellData} from '@app/features/premium/hooks/usePremiumUpsellData';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {SmileySadIcon, StickerIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore} from 'react';

const NO_STICKERS_AVAILABLE_DESCRIPTOR = msg({
	message: 'No stickers yet',
	comment: 'Empty-state text in the channel and chat stickers picker.',
});
const JOIN_A_COMMUNITY_WITH_STICKERS_TO_GET_STARTED_DESCRIPTOR = msg({
	message: 'Join a community to unlock stickers.',
	comment: 'Empty-state hint in the channel and chat stickers picker.',
});
const NO_STICKERS_MATCH_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No stickers match that search',
	comment: 'Empty-state text in the channel and chat stickers picker.',
});
export const StickersPicker = observer(
	({
		channelId,
		handleSelect,
	}: {
		channelId?: string;
		handleSelect: (sticker: GuildSticker, shiftKey?: boolean) => void;
	}) => {
		const {i18n} = useLingui();
		const [searchTerm, setSearchTerm] = useState('');
		const [hoveredSticker, setHoveredSticker] = useState<GuildSticker | null>(null);
		const [selectedRow, setSelectedRow] = useState(-1);
		const [selectedColumn, setSelectedColumn] = useState(-1);
		const [shouldScrollOnSelection, setShouldScrollOnSelection] = useState(false);
		const scrollerRef = useRef<ScrollerHandle>(null);
		const {scrollTop, handleScroll, handleResize} = useScrollerViewport(scrollerRef);
		const searchInputRef = useRef<HTMLInputElement>(null);
		const stickerRefs = useRef<Map<string, HTMLButtonElement>>(new Map());
		const channel = channelId ? (Channels.getChannel(channelId) ?? null) : null;
		const rowListRef = useRef<HTMLDivElement>(null);
		const [listMetrics, setListMetrics] = useState({origin: 0, viewportHeight: 0, gridWidth: 0});
		const [stickerDataVersion, setStickerDataVersion] = useState(0);
		const permissionVersion = useSyncExternalStore(Permission.subscribe.bind(Permission), () => Permission.version);
		const {shouldAnimate: shouldAnimateStickerPreview} = useStickerAnimation();
		const getStickerAvailability = useCallback(
			(sticker: GuildSticker) => checkStickerAvailability(i18n, sticker, channel),
			[channel, i18n, permissionVersion],
		);
		const getStickerGuildId = useCallback((sticker: GuildSticker) => sticker.guildId, []);
		const renderStickerPreviewItem = useCallback(
			(sticker: GuildSticker) => (
				<div
					className={premiumStyles.previewItem}
					key={`${sticker.guildId ?? 'guild'}-${sticker.id}`}
					data-flx="channel.stickers-picker.render-sticker-preview-item.div"
				>
					<img
						src={AvatarUtils.getStickerURL({
							id: sticker.id,
							animated: shouldAnimateStickerPreview,
							isAnimatable: sticker.animated,
							size: 320,
						})}
						alt={sticker.name}
						data-flx="channel.stickers-picker.render-sticker-preview-item.img"
					/>
				</div>
			),
			[shouldAnimateStickerPreview],
		);
		useEffect(() => {
			const handleStickerDataUpdated = () => {
				setStickerDataVersion((version) => version + 1);
			};
			return ComponentBus.subscribe('STICKER_PICKER_RERENDER', handleStickerDataUpdated);
		}, []);
		useSearchInputAutofocus(searchInputRef);
		const searchItems = useMemo(
			() => Sticker.searchWithChannel(channel, searchTerm),
			[channel, searchTerm, stickerDataVersion],
		);
		const searchUpsell = usePremiumUpsellData({
			items: searchItems,
			getAvailability: getStickerAvailability,
			getGuildId: getStickerGuildId,
		});
		const renderedStickers = searchUpsell.accessibleItems;
		const allItems = Sticker.getAllStickers();
		const allUpsell = usePremiumUpsellData({
			items: allItems,
			getAvailability: getStickerAvailability,
			getGuildId: getStickerGuildId,
			renderPreviewItem: renderStickerPreviewItem,
			previewLimit: 4,
		});
		const {favoriteStickers, frequentlyUsedStickers, stickersByGuildId} = useStickerCategories(
			allUpsell.accessibleItems,
			renderedStickers,
		);
		const pickerRows = useVirtualRows(
			searchTerm,
			renderedStickers,
			favoriteStickers,
			frequentlyUsedStickers,
			stickersByGuildId,
			STICKERS_PER_ROW,
		);
		const hasNoStickersAtAll = allItems.length === 0;
		const isSearching = searchTerm.trim().length > 0;
		const lockedStickerCount = allUpsell.summary.lockedItems.length;
		const previewContent = allUpsell.previewContent;
		const stickerCommunityCount = allUpsell.summary.communityCount;
		const stickerUpsellMessage = (
			<Trans>
				Unlock{' '}
				<Plural
					value={lockedStickerCount}
					one="# sticker"
					other="# stickers"
					data-flx="channel.stickers-picker.plural"
				/>{' '}
				from{' '}
				<Plural
					value={stickerCommunityCount}
					one="# community"
					other="# communities"
					data-flx="channel.stickers-picker.plural--2"
				/>{' '}
				with {PREMIUM_PRODUCT_NAME}.
			</Trans>
		);
		const showPremiumUpsell =
			shouldShowPremiumFeatures() && shouldShowStickerPremiumUpsell(channel) && !isSearching && lockedStickerCount > 0;
		const sections = useMemo(() => {
			const result: Array<number> = [];
			for (const row of pickerRows) {
				if (row.type === 'sticker-row') {
					result.push(row.stickers.length);
				}
			}
			return result;
		}, [pickerRows]);
		const {stickerRowIndexes, stickerRowStarts, categoryRowIndexes} = useMemo(() => {
			const indexes = new Array<number>(pickerRows.length);
			const starts: Array<number> = [];
			const categories = new Map<string, number>();
			let stickerRowIndex = 0;
			for (let rowIndex = 0; rowIndex < pickerRows.length; rowIndex += 1) {
				const row = pickerRows[rowIndex];
				indexes[rowIndex] = stickerRowIndex;
				if (row.type === 'sticker-row') {
					starts.push(rowIndex);
					stickerRowIndex += 1;
				} else {
					categories.set(row.category, rowIndex);
				}
			}
			return {stickerRowIndexes: indexes, stickerRowStarts: starts, categoryRowIndexes: categories};
		}, [pickerRows]);
		const zoomLevel = Accessibility.zoomLevel;
		const remScale = getAppRemScale();
		const stickerRowHeight = useMemo(
			() => getStickerRowHeight(listMetrics.gridWidth, STICKERS_PER_ROW, remScale),
			[listMetrics.gridWidth, remScale, zoomLevel],
		);
		const rowOffsets = useMemo(() => {
			const rowKinds = pickerRows.map((row): StickerRowKind => row.type);
			return buildStickerRowOffsets(rowKinds, {remScale, stickerRowHeight, sectionGap: STICKER_SECTION_GAP});
		}, [pickerRows, remScale, stickerRowHeight, zoomLevel]);
		const contentHeight = rowOffsets[rowOffsets.length - 1]!;
		useLayoutEffect(() => {
			const rowListNode = rowListRef.current;
			const scrollerNode = scrollerRef.current?.getViewportElement();
			if (!rowListNode || !scrollerNode) {
				return;
			}
			const origin = Math.round(
				rowListNode.getBoundingClientRect().top - scrollerNode.getBoundingClientRect().top + scrollerNode.scrollTop,
			);
			const viewportHeight = scrollerNode.clientHeight;
			const gridWidth = rowListNode.clientWidth;
			setListMetrics((current) =>
				current.origin === origin && current.viewportHeight === viewportHeight && current.gridWidth === gridWidth
					? current
					: {origin, viewportHeight, gridWidth},
			);
		});
		const rowWindow = useMemo(
			() => getStickerRowWindow(rowOffsets, scrollTop - listMetrics.origin, listMetrics.viewportHeight),
			[rowOffsets, scrollTop, listMetrics],
		);
		const handleCategoryClick = (category: string) => {
			const rowIndex = categoryRowIndexes.get(category);
			if (rowIndex == null) {
				return;
			}
			scrollerRef.current?.scrollTo({to: listMetrics.origin + rowOffsets[rowIndex]!});
		};
		const handleStickerSelect = useCallback(
			(sticker: GuildSticker, shiftKey?: boolean) => {
				const availability = checkStickerAvailability(i18n, sticker, channel);
				if (!availability.canUse) {
					return;
				}
				StickerPickerCommands.trackStickerUsage(sticker);
				handleSelect(sticker, shiftKey);
			},
			[channel, handleSelect, i18n],
		);
		const handleSelectionChange = useCallback(
			(row: number, column: number, shouldScroll = false) => {
				if (row < 0 || column < 0) {
					return;
				}
				setSelectedRow(row);
				setSelectedColumn(column);
				setShouldScrollOnSelection(shouldScroll);
				let currentRow = 0;
				for (const pickerRow of pickerRows) {
					if (pickerRow.type === 'sticker-row') {
						if (currentRow === row && column < pickerRow.stickers.length) {
							const sticker = pickerRow.stickers[column];
							setHoveredSticker(sticker);
							break;
						}
						currentRow++;
					}
				}
			},
			[pickerRows],
		);
		const handleHover = useCallback(
			(sticker: GuildSticker | null, row?: number, column?: number) => {
				setHoveredSticker(sticker);
				if (sticker && row !== undefined && column !== undefined) {
					handleSelectionChange(row, column, false);
				}
			},
			[handleSelectionChange],
		);
		useEffect(() => {
			if (renderedStickers.length > 0 && selectedRow === 0 && selectedColumn === 0 && !hoveredSticker) {
				handleSelectionChange(0, 0, false);
			}
		}, [renderedStickers, selectedRow, selectedColumn, hoveredSticker, handleSelectionChange]);
		useEffect(() => {
			if (!shouldScrollOnSelection || selectedRow < 0) {
				return;
			}
			const rowIndex = stickerRowStarts[selectedRow];
			if (rowIndex == null) {
				return;
			}
			const start = listMetrics.origin + rowOffsets[rowIndex]!;
			scrollerRef.current?.revealRange({start, end: start + stickerRowHeight});
		}, [shouldScrollOnSelection, selectedRow, stickerRowStarts, rowOffsets, listMetrics.origin, stickerRowHeight]);
		const handleSelectSticker = useCallback(
			(row: number | null, column: number | null, event?: React.KeyboardEvent) => {
				if (row === null || column === null) {
					return;
				}
				let currentRow = 0;
				for (const pickerRow of pickerRows) {
					if (pickerRow.type === 'sticker-row') {
						if (currentRow === row && column < pickerRow.stickers.length) {
							const sticker = pickerRow.stickers[column];
							handleStickerSelect(sticker, event?.shiftKey);
							return;
						}
						currentRow++;
					}
				}
			},
			[pickerRows, handleStickerSelect],
		);
		if (hasNoStickersAtAll) {
			return (
				<div className={gifStyles.gifPickerContainer} data-flx="channel.stickers-picker.div">
					<div className={gifStyles.gifPickerMain} data-flx="channel.stickers-picker.div--2">
						<PickerEmptyState
							icon={StickerIcon}
							title={i18n._(NO_STICKERS_AVAILABLE_DESCRIPTOR)}
							description={i18n._(JOIN_A_COMMUNITY_WITH_STICKERS_TO_GET_STARTED_DESCRIPTOR)}
							data-flx="channel.stickers-picker.picker-empty-state"
						/>
					</div>
				</div>
			);
		}
		const renderSearchBar = () => (
			<StickerPickerSearchBar
				searchTerm={searchTerm}
				setSearchTerm={setSearchTerm}
				hoveredSticker={hoveredSticker}
				inputRef={searchInputRef}
				selectedRow={selectedRow}
				selectedColumn={selectedColumn}
				sections={sections}
				onSelect={handleSelectSticker}
				onSelectionChange={handleSelectionChange}
				data-flx="channel.stickers-picker.render-search-bar.sticker-picker-search-bar.select-sticker"
			/>
		);
		return (
			<div className={styles.container} data-flx="channel.stickers-picker.container">
				<ExpressionPickerHeaderPortal data-flx="channel.stickers-picker.expression-picker-header-portal">
					{renderSearchBar()}
				</ExpressionPickerHeaderPortal>
				<div className={styles.emojiPicker} data-flx="channel.stickers-picker.emoji-picker">
					<div className={styles.bodyWrapper} data-flx="channel.stickers-picker.body-wrapper">
						<div
							className={styles.emojiPickerListWrapper}
							role="presentation"
							data-flx="channel.stickers-picker.emoji-picker-list-wrapper"
						>
							<Scroller
								ref={scrollerRef}
								className={`${styles.list} ${styles.listWrapper}`}
								fade={false}
								key="stickers-picker-scroller"
								onScroll={handleScroll}
								onResize={handleResize}
								data-flx="channel.stickers-picker.list"
							>
								{showPremiumUpsell && (
									<PremiumUpsellBanner
										message={stickerUpsellMessage}
										communityIds={allUpsell.summary.lockedCommunityIds}
										communityCount={stickerCommunityCount}
										previewContent={previewContent}
										data-flx="channel.stickers-picker.premium-upsell-banner"
									/>
								)}
								<div
									ref={rowListRef}
									style={{position: 'relative', flexShrink: 0, height: `${contentHeight}px`}}
									data-flx="channel.stickers-picker.row-list"
								>
									{pickerRows.slice(rowWindow.firstRow, rowWindow.lastRow + 1).map((row, windowIndex) => {
										const rowIndex = rowWindow.firstRow + windowIndex;
										return (
											<div
												key={`${row.type}-${row.index}`}
												style={{
													position: 'absolute',
													left: 0,
													right: 0,
													transform: `translateY(${rowOffsets[rowIndex]!}px)`,
												}}
												data-flx="channel.stickers-picker.div--3"
											>
												<VirtualRowWrapper
													row={row}
													handleHover={handleHover}
													handleSelect={handleStickerSelect}
													gridColumns={STICKERS_PER_ROW}
													selectedRow={selectedRow}
													selectedColumn={selectedColumn}
													stickerRowIndex={stickerRowIndexes[rowIndex]!}
													shouldScrollOnSelection={shouldScrollOnSelection}
													stickerRefs={stickerRefs}
													channel={channel}
													data-flx="channel.stickers-picker.virtual-row-wrapper"
												/>
											</div>
										);
									})}
								</div>
							</Scroller>
							{renderedStickers.length === 0 && (
								<div className={styles.emptyState} data-flx="channel.stickers-picker.empty-state">
									<div className={styles.emptyStateInner} data-flx="channel.stickers-picker.empty-state-inner">
										<div className={styles.emptyIcon} data-flx="channel.stickers-picker.empty-icon">
											<SmileySadIcon weight="duotone" data-flx="channel.stickers-picker.smiley-sad-icon" />
										</div>
										<div className={styles.emptyLabel} data-flx="channel.stickers-picker.empty-label">
											{i18n._(NO_STICKERS_MATCH_YOUR_SEARCH_DESCRIPTOR)}
										</div>
									</div>
								</div>
							)}
						</div>
					</div>
					<StickerPickerInspector
						hoveredSticker={hoveredSticker}
						data-flx="channel.stickers-picker.sticker-picker-inspector"
					/>
				</div>
				<StickerPickerCategoryList
					stickersByGuildId={stickersByGuildId}
					handleCategoryClick={handleCategoryClick}
					data-flx="channel.stickers-picker.sticker-picker-category-list"
				/>
			</div>
		);
	},
);
