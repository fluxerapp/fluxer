// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {EmojiPickerCategoryList} from '@app/features/channel/components/emoji_picker/EmojiPickerCategoryList';
import {
	EMOJI_GRID_TRACK_WIDTH,
	EMOJI_ROW_HEIGHT,
	EMOJI_SECTION_GAP,
	type EmojiRowKind,
	getEmojiGridColumns,
} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import {EmojiPickerInspector} from '@app/features/channel/components/emoji_picker/EmojiPickerInspector';
import {EmojiPickerSearchBar} from '@app/features/channel/components/emoji_picker/EmojiPickerSearchBar';
import {useEmojiCategories} from '@app/features/channel/components/emoji_picker/hooks/useEmojiCategories';
import {getPickerRowItems, useVirtualRows} from '@app/features/channel/components/emoji_picker/hooks/useVirtualRows';
import {renderEmojiPickerPreviewItem} from '@app/features/channel/components/emoji_picker/PickerPreviewItem';
import {VirtualizedRow} from '@app/features/channel/components/emoji_picker/VirtualRow';
import {PremiumUpsellBanner} from '@app/features/channel/components/PremiumUpsellBanner';
import {
	buildRowOffsets,
	PICKER_CATEGORY_HEADER_GAP,
	PICKER_CATEGORY_HEADER_HEIGHT,
} from '@app/features/channel/components/pickers/shared/PickerRowLayout';
import {useScrollerViewport} from '@app/features/channel/components/pickers/shared/useScrollerViewport';
import {
	usePickerListMetrics,
	useVirtualPickerGrid,
} from '@app/features/channel/components/pickers/shared/useVirtualPickerGrid';
import Channels from '@app/features/channel/state/Channels';
import Emoji, {normalizeEmojiSearchQuery} from '@app/features/emoji/state/Emoji';
import EmojiPickerState from '@app/features/emoji/state/EmojiPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	ExpressionPickerHeaderContext,
	ExpressionPickerHeaderPortal,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {
	checkEmojiAvailability,
	shouldShowEmojiPremiumUpsell,
} from '@app/features/expressions/utils/ExpressionPermissionUtils';

import Permission from '@app/features/permissions/state/Permission';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {usePremiumUpsellData} from '@app/features/premium/hooks/usePremiumUpsellData';
import {getPremiumProductName, shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {SmileySadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useContext, useEffect, useMemo, useRef, useState, useSyncExternalStore} from 'react';

const NO_EMOJIS_MATCH_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No emojis match that search',
	comment: 'Empty-state text in the channel and chat emoji picker.',
});
export const EmojiPicker = observer(
	({channelId, handleSelect}: {channelId?: string; handleSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void}) => {
		const PREMIUM_PRODUCT_NAME = getPremiumProductName();
		const headerContext = useContext(ExpressionPickerHeaderContext);
		if (!headerContext) {
			throw new Error(
				'EmojiPicker must be rendered inside ExpressionPickerPopout so that the header portal is available.',
			);
		}
		const [searchTerm, setSearchTerm] = useState('');

		const scrollerRef = useRef<ScrollerHandle>(null);
		const {viewportSize, scrollTop, handleScroll, handleResize} = useScrollerViewport(scrollerRef);
		const gridColumns = useMemo(() => getEmojiGridColumns(viewportSize.width), [viewportSize.width]);
		const searchInputRef = useRef<HTMLInputElement>(null);
		const emojiRefs = useRef<Map<string, HTMLButtonElement>>(new Map());
		const normalizedSearchTerm = useMemo(() => normalizeEmojiSearchQuery(searchTerm), [searchTerm]);
		const {i18n} = useLingui();
		const channel = channelId ? (Channels.getChannel(channelId) ?? null) : null;
		const rowListRef = useRef<HTMLDivElement>(null);
		const listMetrics = usePickerListMetrics(scrollerRef, rowListRef);
		const [emojiDataVersion, setEmojiDataVersion] = useState(0);
		const permissionVersion = useSyncExternalStore(Permission.subscribe.bind(Permission), () => Permission.version);
		const skinTone = Emoji.skinTone;
		const shouldAnimateEmoji = useShouldAnimate({kind: 'emoji'});
		const getEmojiAvailability = useCallback(
			(emoji: FlatEmoji) => checkEmojiAvailability(i18n, emoji, channel),
			[channel, i18n, permissionVersion],
		);
		const getEmojiGuildId = useCallback((emoji: FlatEmoji) => emoji.guildId, []);
		const renderEmojiPreviewItem = useCallback(
			(emoji: FlatEmoji) => renderEmojiPickerPreviewItem(emoji, skinTone, shouldAnimateEmoji),
			[skinTone, shouldAnimateEmoji],
		);
		useEffect(() => {
			const handleEmojiDataUpdated = () => {
				setEmojiDataVersion((version) => version + 1);
			};
			return ComponentBus.subscribe('EMOJI_PICKER_RERENDER', handleEmojiDataUpdated);
		}, []);
		useSearchInputAutofocus(searchInputRef);
		const searchItems = useMemo(
			() => Emoji.search(channel, normalizedSearchTerm).slice(),
			[channel, normalizedSearchTerm, emojiDataVersion],
		);
		const searchUpsell = usePremiumUpsellData({
			items: searchItems,
			getAvailability: getEmojiAvailability,
			getGuildId: getEmojiGuildId,
		});
		const renderedEmojis = searchUpsell.accessibleItems;
		const allItems = useMemo(() => Emoji.getAllEmojis(channel).slice(), [channel, emojiDataVersion]);
		const allUpsell = usePremiumUpsellData({
			items: allItems,
			getAvailability: getEmojiAvailability,
			getGuildId: getEmojiGuildId,
			renderPreviewItem: renderEmojiPreviewItem,
			previewLimit: 4,
		});
		const {favoriteEmojis, frequentlyUsedEmojis, customEmojisByGuildId, unicodeEmojisByCategory} = useEmojiCategories(
			allUpsell.accessibleItems,
			renderedEmojis,
		);
		const showFrequentlyUsedButton = frequentlyUsedEmojis.length > 0 && !normalizedSearchTerm;
		const pickerRows = useVirtualRows(
			normalizedSearchTerm,
			renderedEmojis,
			favoriteEmojis,
			frequentlyUsedEmojis,
			customEmojisByGuildId,
			unicodeEmojisByCategory,
			gridColumns,
		);
		const lockedEmojiCount = allUpsell.summary.lockedItems.length;
		const communityCount = allUpsell.summary.communityCount;
		const previewContent = allUpsell.previewContent;
		const emojiUpsellMessage = (
			<Trans>
				Unlock{' '}
				<Plural
					value={lockedEmojiCount}
					one="# custom emoji"
					other="# custom emojis"
					data-flx="channel.emoji-picker.plural"
				/>{' '}
				from{' '}
				<Plural
					value={communityCount}
					one="# community"
					other="# communities"
					data-flx="channel.emoji-picker.plural--2"
				/>{' '}
				with {PREMIUM_PRODUCT_NAME}.
			</Trans>
		);
		const showPremiumUpsell =
			shouldShowPremiumFeatures() &&
			shouldShowEmojiPremiumUpsell(channel) &&
			!normalizedSearchTerm &&
			lockedEmojiCount > 0;

		const zoomLevel = Accessibility.zoomLevel;
		const remScale = getAppRemScale();
		const rowOffsets = useMemo(() => {
			const rowKinds = pickerRows.map((row): EmojiRowKind => row.type);
			return buildRowOffsets(rowKinds, {
				headerHeightPx: (PICKER_CATEGORY_HEADER_HEIGHT + PICKER_CATEGORY_HEADER_GAP) * remScale,
				rowHeightPx: EMOJI_ROW_HEIGHT * remScale,
				sectionGap: EMOJI_SECTION_GAP,
			});
		}, [pickerRows, remScale, zoomLevel]);
		const {
			sections,
			rowIndexes: emojiRowIndexes,
			rowWindow,
			contentHeight,
			selectedRow,
			selectedColumn,
			hovered: hoveredEmoji,
			shouldScrollOnSelection,
			handleSelectionChange,
			handleHover,
			handleCategoryClick,
			selectAt,
		} = useVirtualPickerGrid({
			rows: pickerRows,
			getItems: getPickerRowItems,
			rowOffsets,
			scrollerRef,
			scrollTop,
			rowHeightPx: EMOJI_ROW_HEIGHT * remScale,
			listMetrics,
			leadRows: 0,
			zoomLevel,
			renderedCount: renderedEmojis.length,
		});

		const handleEmojiSelect = useCallback(
			(emoji: FlatEmoji, shiftKey?: boolean) => {
				const availability = checkEmojiAvailability(i18n, emoji, channel);
				if (!availability.canUse) {
					return;
				}
				EmojiPickerState.trackEmoji(emoji);
				handleSelect(emoji, shiftKey);
			},
			[channel, handleSelect, i18n],
		);

		const handleSelectEmoji = useCallback(
			(row: number | null, column: number | null, event?: React.KeyboardEvent) => {
				const item = selectAt(row, column);
				if (item !== undefined) handleEmojiSelect(item, event?.shiftKey);
			},
			[selectAt, handleEmojiSelect],
		);
		return (
			<div className={styles.container} data-flx="channel.emoji-picker.container">
				<ExpressionPickerHeaderPortal data-flx="channel.emoji-picker.expression-picker-header-portal">
					<EmojiPickerSearchBar
						searchTerm={searchTerm}
						setSearchTerm={setSearchTerm}
						hoveredEmoji={hoveredEmoji}
						inputRef={searchInputRef}
						selectedRow={selectedRow}
						selectedColumn={selectedColumn}
						sections={sections}
						onSelect={handleSelectEmoji}
						onSelectionChange={handleSelectionChange}
						data-flx="channel.emoji-picker.emoji-picker-search-bar.select-emoji"
					/>
				</ExpressionPickerHeaderPortal>
				<div className={styles.emojiPicker} data-flx="channel.emoji-picker.emoji-picker">
					<div className={styles.bodyWrapper} data-flx="channel.emoji-picker.body-wrapper">
						<div
							className={styles.emojiPickerListWrapper}
							role="presentation"
							data-flx="channel.emoji-picker.emoji-picker-list-wrapper"
						>
							<Scroller
								ref={scrollerRef}
								className={`${styles.list} ${styles.listWrapper}`}
								fade={false}
								key="emoji_picker-scroller"
								onScroll={handleScroll}
								onResize={handleResize}
								data-flx="channel.emoji-picker.list"
							>
								{showPremiumUpsell && (
									<PremiumUpsellBanner
										message={emojiUpsellMessage}
										communityIds={allUpsell.summary.lockedCommunityIds}
										communityCount={communityCount}
										previewContent={previewContent}
										data-flx="channel.emoji-picker.premium-upsell-banner"
									/>
								)}
								<div
									ref={rowListRef}
									style={{position: 'relative', flexShrink: 0, height: `${contentHeight}px`}}
									data-flx="channel.emoji-picker.row-list"
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
												data-flx="channel.emoji-picker.div"
											>
												<VirtualizedRow
													row={row}
													handleHover={handleHover}
													handleSelect={handleEmojiSelect}
													skinTone={skinTone}
													channel={channel}
													allowAnimation={shouldAnimateEmoji}
													gridColumns={gridColumns}
													cellTrackWidth={EMOJI_GRID_TRACK_WIDTH}
													selectedRow={selectedRow}
													selectedColumn={selectedColumn}
													emojiRowIndex={emojiRowIndexes[rowIndex]!}
													shouldScrollOnSelection={shouldScrollOnSelection}
													emojiRefs={emojiRefs}
													data-flx="channel.emoji-picker.virtualized-row"
												/>
											</div>
										);
									})}
								</div>
							</Scroller>
							{renderedEmojis.length === 0 && (
								<div className={styles.emptyState} data-flx="channel.emoji-picker.empty-state">
									<div className={styles.emptyStateInner} data-flx="channel.emoji-picker.empty-state-inner">
										<div className={styles.emptyIcon} data-flx="channel.emoji-picker.empty-icon">
											<SmileySadIcon weight="duotone" data-flx="channel.emoji-picker.smiley-sad-icon" />
										</div>
										<div className={styles.emptyLabel} data-flx="channel.emoji-picker.empty-label">
											{i18n._(NO_EMOJIS_MATCH_YOUR_SEARCH_DESCRIPTOR)}
										</div>
									</div>
								</div>
							)}
						</div>
					</div>
					<EmojiPickerInspector hoveredEmoji={hoveredEmoji} data-flx="channel.emoji-picker.emoji-picker-inspector" />
				</div>
				<EmojiPickerCategoryList
					customEmojisByGuildId={customEmojisByGuildId}
					unicodeEmojisByCategory={unicodeEmojisByCategory}
					handleCategoryClick={handleCategoryClick}
					showFrequentlyUsedButton={showFrequentlyUsedButton}
					data-flx="channel.emoji-picker.emoji-picker-category-list"
				/>
			</div>
		);
	},
);
