// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {EmojiPickerCategoryList} from '@app/features/channel/components/emoji_picker/EmojiPickerCategoryList';
import {
	buildEmojiRowOffsets,
	type EmojiRowKind,
	getEmojiRowWindow,
} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import {EmojiPickerSearchBar} from '@app/features/channel/components/emoji_picker/EmojiPickerSearchBar';
import {useEmojiCategories} from '@app/features/channel/components/emoji_picker/hooks/useEmojiCategories';
import {useVirtualRows} from '@app/features/channel/components/emoji_picker/hooks/useVirtualRows';
import {VirtualizedRow} from '@app/features/channel/components/emoji_picker/VirtualRow';
import mobileStyles from '@app/features/channel/components/MobileEmojiPicker.module.css';
import {PremiumUpsellBanner} from '@app/features/channel/components/PremiumUpsellBanner';
import premiumStyles from '@app/features/channel/components/PremiumUpsellBanner.module.css';
import {getMobileEmojiGridColumns} from '@app/features/channel/components/pickers/shared/MobilePickerGridLayout';
import {useScrollerViewport} from '@app/features/channel/components/pickers/shared/useScrollerViewport';
import Channels from '@app/features/channel/state/Channels';
import Emoji, {normalizeEmojiSearchQuery} from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {
	checkEmojiAvailability,
	shouldShowEmojiPremiumUpsell,
} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import {getEmojiDisplayDataWithSkinTone} from '@app/features/expressions/utils/SkinToneUtils';
import {getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {usePremiumUpsellData} from '@app/features/premium/hooks/usePremiumUpsellData';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore} from 'react';

function getVisibleViewportWidth(): number {
	if (typeof window === 'undefined') {
		return 0;
	}
	return window.visualViewport?.width ?? window.innerWidth ?? 0;
}

export const MobileEmojiPicker = observer(
	({
		channelId,
		handleSelect,
		externalSearchTerm,
		externalSetSearchTerm,
		hideSearchBar = false,
	}: {
		channelId?: string;
		handleSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
		externalSearchTerm?: string;
		externalSetSearchTerm?: (term: string) => void;
		hideSearchBar?: boolean;
	}) => {
		const headerPortalContext = useExpressionPickerHeaderPortal();
		const hasPortal = Boolean(headerPortalContext?.headerPortalElement);
		const {i18n} = useLingui();
		const [internalSearchTerm, setInternalSearchTerm] = useState('');
		const [hoveredEmoji, setHoveredEmoji] = useState<FlatEmoji | null>(null);
		const [visibleViewportWidth, setVisibleViewportWidth] = useState(() => getVisibleViewportWidth());
		const scrollerRef = useRef<ScrollerHandle>(null);
		const searchInputRef = useRef<HTMLInputElement>(null);
		const emojiRefs = useRef<Map<string, HTMLButtonElement>>(new Map());
		const {viewportSize, scrollTop, handleScroll, handleResize} = useScrollerViewport(scrollerRef);
		const channel = channelId ? (Channels.getChannel(channelId) ?? null) : null;
		const rowListRef = useRef<HTMLDivElement>(null);
		const [listMetrics, setListMetrics] = useState({origin: 0, viewportHeight: 0});
		const [emojiDataVersion, setEmojiDataVersion] = useState(0);
		const permissionVersion = useSyncExternalStore(Permission.subscribe.bind(Permission), () => Permission.version);
		const getEmojiAvailability = useCallback(
			(emoji: FlatEmoji) => checkEmojiAvailability(i18n, emoji, channel),
			[channel, i18n, permissionVersion],
		);
		const getEmojiGuildId = useCallback((emoji: FlatEmoji) => emoji.guildId, []);
		const skinTone = Emoji.skinTone;
		const shouldAnimateEmoji = useShouldAnimate({kind: 'emoji'});
		const searchTerm = externalSearchTerm ?? internalSearchTerm;
		const setSearchTerm = externalSetSearchTerm ?? setInternalSearchTerm;
		const normalizedSearchTerm = useMemo(() => normalizeEmojiSearchQuery(searchTerm), [searchTerm]);
		useEffect(() => {
			const handleEmojiDataUpdated = () => {
				setEmojiDataVersion((version) => version + 1);
			};
			return ComponentBus.subscribe('EMOJI_PICKER_RERENDER', handleEmojiDataUpdated);
		}, []);
		useEffect(() => {
			const updateVisibleViewportWidth = () => {
				setVisibleViewportWidth((previousWidth) => {
					const nextWidth = getVisibleViewportWidth();
					return previousWidth === nextWidth ? previousWidth : nextWidth;
				});
			};
			updateVisibleViewportWidth();
			window.addEventListener('resize', updateVisibleViewportWidth);
			window.visualViewport?.addEventListener('resize', updateVisibleViewportWidth);
			return () => {
				window.removeEventListener('resize', updateVisibleViewportWidth);
				window.visualViewport?.removeEventListener('resize', updateVisibleViewportWidth);
			};
		}, []);
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
		const renderEmojiPreviewItem = useCallback(
			(emoji: FlatEmoji) => {
				const key = emoji.id ?? emoji.uniqueName ?? emoji.name ?? `${emoji.guildId ?? 'unicode'}-mobile`;
				const {url: fallbackDisplayUrl} = getEmojiDisplayDataWithSkinTone(emoji, skinTone);
				const displayUrl = emoji.id
					? getEmojiRenderUrl({
							id: emoji.id,
							surrogateUrl: null,
							isAnimatable: Boolean(emoji.animated),
							animated: shouldAnimateEmoji,
							jumbo: false,
						})
					: fallbackDisplayUrl;
				let content: React.ReactNode;
				if (displayUrl) {
					content = (
						<img
							src={displayUrl}
							alt={emoji.name}
							data-flx="channel.mobile-emoji-picker.render-emoji-preview-item.img"
						/>
					);
				} else {
					content = (
						<span
							className={premiumStyles.previewEmojiText}
							data-flx="channel.mobile-emoji-picker.render-emoji-preview-item.span"
						>
							{emoji.name ?? emoji.uniqueName}
						</span>
					);
				}
				return (
					<div
						className={premiumStyles.previewItem}
						key={key}
						data-flx="channel.mobile-emoji-picker.render-emoji-preview-item.div"
					>
						{content}
					</div>
				);
			},
			[shouldAnimateEmoji, skinTone],
		);
		const allUpsell = usePremiumUpsellData({
			items: allItems,
			getAvailability: getEmojiAvailability,
			getGuildId: getEmojiGuildId,
			renderPreviewItem: renderEmojiPreviewItem,
			previewLimit: 4,
		});
		useSearchInputAutofocus(searchInputRef);
		const {customEmojisByGuildId, unicodeEmojisByCategory, favoriteEmojis, frequentlyUsedEmojis} = useEmojiCategories(
			allUpsell.accessibleItems,
			renderedEmojis,
		);
		const showFrequentlyUsedButton = frequentlyUsedEmojis.length > 0 && !normalizedSearchTerm;
		const zoomLevel = Accessibility.zoomLevel;
		const effectiveViewportWidth = useMemo(() => {
			const candidateWidths = [viewportSize.width, visibleViewportWidth].filter((width) => width > 0);
			if (candidateWidths.length === 0) {
				return 0;
			}
			return Math.min(...candidateWidths);
		}, [viewportSize.width, visibleViewportWidth]);
		const gridColumns = useMemo(
			() => getMobileEmojiGridColumns(effectiveViewportWidth),
			[effectiveViewportWidth, zoomLevel],
		);
		const gridWidth = useMemo(() => {
			if (effectiveViewportWidth <= 0) {
				return undefined;
			}
			return Math.max(0, effectiveViewportWidth - 24);
		}, [effectiveViewportWidth]);
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
					data-flx="channel.mobile-emoji-picker.plural"
				/>{' '}
				from{' '}
				<Plural
					value={communityCount}
					one="# community"
					other="# communities"
					data-flx="channel.mobile-emoji-picker.plural--2"
				/>{' '}
				with {PREMIUM_PRODUCT_NAME}.
			</Trans>
		);
		const showPremiumUpsell =
			shouldShowPremiumFeatures() &&
			shouldShowEmojiPremiumUpsell(channel) &&
			!normalizedSearchTerm &&
			lockedEmojiCount > 0;
		const categoryRowIndexes = useMemo(() => {
			const categories = new Map<string, number>();
			for (let rowIndex = 0; rowIndex < pickerRows.length; rowIndex += 1) {
				const row = pickerRows[rowIndex];
				if (row.type === 'header') {
					categories.set(row.category, rowIndex);
				}
			}
			return categories;
		}, [pickerRows]);
		const remScale = getAppRemScale();
		const rowOffsets = useMemo(() => {
			const rowKinds = pickerRows.map((row): EmojiRowKind => row.type);
			return buildEmojiRowOffsets(rowKinds, {remScale, sectionGap: 0});
		}, [pickerRows, remScale, zoomLevel]);
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
			setListMetrics((current) =>
				current.origin === origin && current.viewportHeight === viewportHeight ? current : {origin, viewportHeight},
			);
		});
		const rowWindow = useMemo(
			() => getEmojiRowWindow(rowOffsets, scrollTop - listMetrics.origin, listMetrics.viewportHeight),
			[rowOffsets, scrollTop, listMetrics],
		);
		const handleCategoryClick = (category: string) => {
			const rowIndex = categoryRowIndexes.get(category);
			if (rowIndex == null) {
				return;
			}
			scrollerRef.current?.scrollTo({to: listMetrics.origin + rowOffsets[rowIndex]!});
		};
		const handleHover = useCallback((emoji: FlatEmoji | null) => {
			setHoveredEmoji(emoji);
		}, []);
		const searchBar = !hideSearchBar ? (
			<EmojiPickerSearchBar
				searchTerm={searchTerm}
				setSearchTerm={setSearchTerm}
				hoveredEmoji={hoveredEmoji}
				data-flx="channel.mobile-emoji-picker.emoji-picker-search-bar"
			/>
		) : null;
		return (
			<div className={mobileStyles.container} data-flx="channel.mobile-emoji-picker.div">
				{hasPortal && searchBar ? (
					<ExpressionPickerHeaderPortal data-flx="channel.mobile-emoji-picker.expression-picker-header-portal">
						{searchBar}
					</ExpressionPickerHeaderPortal>
				) : null}
				<div className={mobileStyles.mobileEmojiPicker} data-flx="channel.mobile-emoji-picker.div--2">
					{!hasPortal && searchBar}
					<div className={mobileStyles.bodyWrapper} data-flx="channel.mobile-emoji-picker.div--3">
						<div
							className={mobileStyles.emojiPickerListWrapper}
							role="presentation"
							data-flx="channel.mobile-emoji-picker.presentation"
						>
							<Scroller
								ref={scrollerRef}
								className={`${mobileStyles.list} ${mobileStyles.listWrapper}`}
								key="mobile-emoji_picker-scroller"
								onScroll={handleScroll}
								onResize={handleResize}
								data-flx="channel.mobile-emoji-picker.scroller"
							>
								{showPremiumUpsell && (
									<PremiumUpsellBanner
										message={emojiUpsellMessage}
										communityIds={allUpsell.summary.lockedCommunityIds}
										communityCount={communityCount}
										previewContent={previewContent}
										data-flx="channel.mobile-emoji-picker.premium-upsell-banner"
									/>
								)}
								<div
									ref={rowListRef}
									style={{position: 'relative', flexShrink: 0, height: `${contentHeight}px`}}
									data-flx="channel.mobile-emoji-picker.row-list"
								>
									{pickerRows.slice(rowWindow.firstRow, rowWindow.lastRow + 1).map((row, windowIndex) => (
										<div
											key={`${row.type}-${row.index}`}
											style={{
												position: 'absolute',
												left: 0,
												right: 0,
												transform: `translateY(${rowOffsets[rowWindow.firstRow + windowIndex]!}px)`,
											}}
											data-flx="channel.mobile-emoji-picker.div--4"
										>
											<VirtualizedRow
												row={row}
												handleHover={handleHover}
												handleSelect={handleSelect}
												skinTone={skinTone}
												channel={channel}
												allowAnimation={shouldAnimateEmoji}
												gridColumns={gridColumns}
												gridWidth={gridWidth}
												selectedRow={-1}
												selectedColumn={-1}
												emojiRowIndex={0}
												emojiRefs={emojiRefs}
												data-flx="channel.mobile-emoji-picker.virtualized-row"
											/>
										</div>
									))}
								</div>
							</Scroller>
						</div>
					</div>
					<div className={mobileStyles.categoryListBottom} data-flx="channel.mobile-emoji-picker.div--5">
						<Scroller
							className={mobileStyles.categoryListBottomScroller}
							orientation="horizontal"
							overflow="auto"
							fade={false}
							showTrack={false}
							key="mobile-emoji-picker-category-scroller"
							data-flx="channel.mobile-emoji-picker.scroller--2"
						>
							<div className={mobileStyles.categoryListBottomContent} data-flx="channel.mobile-emoji-picker.div--6">
								<EmojiPickerCategoryList
									customEmojisByGuildId={customEmojisByGuildId}
									unicodeEmojisByCategory={unicodeEmojisByCategory}
									handleCategoryClick={handleCategoryClick}
									horizontal={true}
									showFrequentlyUsedButton={showFrequentlyUsedButton}
									data-flx="channel.mobile-emoji-picker.emoji-picker-category-list"
								/>
							</div>
						</Scroller>
					</div>
				</div>
			</div>
		);
	},
);
