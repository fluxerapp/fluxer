// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import mobileStyles from '@app/features/channel/components/MobileEmojiPicker.module.css';
import {PremiumUpsellBanner} from '@app/features/channel/components/PremiumUpsellBanner';
import premiumStyles from '@app/features/channel/components/PremiumUpsellBanner.module.css';
import {getMobileStickerGridColumns} from '@app/features/channel/components/pickers/shared/MobilePickerGridLayout';
import {useScrollerViewport} from '@app/features/channel/components/pickers/shared/useScrollerViewport';
import stickerStyles from '@app/features/channel/components/StickersPicker.module.css';
import {PickerEmptyState} from '@app/features/channel/components/shared/PickerEmptyState';
import {useStickerCategories} from '@app/features/channel/components/sticker_picker/hooks/useStickerCategories';
import {useVirtualRows} from '@app/features/channel/components/sticker_picker/hooks/useVirtualRows';
import {StickerPickerCategoryList} from '@app/features/channel/components/sticker_picker/StickerPickerCategoryList';
import {StickerPickerInspector} from '@app/features/channel/components/sticker_picker/StickerPickerInspector';
import {StickerPickerSearchBar} from '@app/features/channel/components/sticker_picker/StickerPickerSearchBar';
import {VirtualRowRenderer} from '@app/features/channel/components/sticker_picker/VirtualRow';
import Channels from '@app/features/channel/state/Channels';
import * as StickerPickerCommands from '@app/features/emoji/commands/StickerPickerCommands';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {
	checkStickerAvailability,
	shouldShowStickerPremiumUpsell,
} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {usePremiumUpsellData} from '@app/features/premium/hooks/usePremiumUpsellData';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {SmileySadIcon, StickerIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState, useSyncExternalStore} from 'react';

const NO_STICKERS_AVAILABLE_DESCRIPTOR = msg({
	message: 'No stickers yet',
	comment: 'Empty-state text in the channel and chat mobile stickers picker.',
});
const JOIN_A_COMMUNITY_WITH_STICKERS_TO_GET_STARTED_DESCRIPTOR = msg({
	message: 'Join a community to unlock stickers.',
	comment: 'Empty-state hint in the channel and chat mobile stickers picker.',
});
const NO_STICKERS_FOUND_DESCRIPTOR = msg({
	message: 'No stickers match',
	comment: 'Empty-state text in the channel and chat mobile stickers picker.',
});
const TRY_A_DIFFERENT_SEARCH_TERM_DESCRIPTOR = msg({
	message: 'Try a different search.',
	comment: 'Label in the channel and chat mobile stickers picker.',
});
export const MobileStickersPicker = observer(
	({
		channelId,
		handleSelect,
	}: {
		channelId?: string;
		handleSelect: (sticker: GuildSticker, shiftKey?: boolean) => void;
	}) => {
		const {i18n} = useLingui();
		const headerPortalContext = useExpressionPickerHeaderPortal();
		const hasPortal = Boolean(headerPortalContext?.headerPortalElement);
		const [searchTerm, setSearchTerm] = useState('');
		const [hoveredSticker, setHoveredSticker] = useState<GuildSticker | null>(null);
		const scrollerRef = useRef<ScrollerHandle>(null);
		const searchInputRef = useRef<HTMLInputElement>(null);
		const stickerRefs = useRef<Map<string, HTMLButtonElement>>(new Map());
		const {viewportSize, handleResize} = useScrollerViewport(scrollerRef);
		const channel = channelId ? (Channels.getChannel(channelId) ?? null) : null;
		const categoryRefs = useRef<Map<string, HTMLDivElement>>(new Map());
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
					data-flx="channel.mobile-stickers-picker.render-sticker-preview-item.div"
				>
					<img
						src={AvatarUtils.getStickerURL({
							id: sticker.id,
							animated: shouldAnimateStickerPreview,
							size: 320,
						})}
						alt={sticker.name}
						loading="lazy"
						data-flx="channel.mobile-stickers-picker.render-sticker-preview-item.img"
					/>
				</div>
			),
			[shouldAnimateStickerPreview],
		);
		useEffect(() => {
			const handleStickerDataUpdated = () => {
				setStickerDataVersion((version) => version + 1);
			};
			return ComponentDispatch.subscribe('STICKER_PICKER_RERENDER', handleStickerDataUpdated);
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
		const zoomLevel = Accessibility.zoomLevel;
		const gridColumns = useMemo(() => getMobileStickerGridColumns(viewportSize.width), [viewportSize.width, zoomLevel]);
		const virtualRows = useVirtualRows(
			searchTerm,
			renderedStickers,
			favoriteStickers,
			frequentlyUsedStickers,
			stickersByGuildId,
			gridColumns,
		);
		const hasNoStickersAtAll = allItems.length === 0;
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
					data-flx="channel.mobile-stickers-picker.plural"
				/>{' '}
				from{' '}
				<Plural
					value={stickerCommunityCount}
					one="# community"
					other="# communities"
					data-flx="channel.mobile-stickers-picker.plural--2"
				/>{' '}
				with {PREMIUM_PRODUCT_NAME}.
			</Trans>
		);
		const isSearching = searchTerm.trim().length > 0;
		const showPremiumUpsell =
			shouldShowPremiumFeatures() && shouldShowStickerPremiumUpsell(channel) && !isSearching && lockedStickerCount > 0;
		const handleCategoryClick = (category: string) => {
			const element = categoryRefs.current.get(category);
			if (element) {
				scrollerRef.current?.scrollIntoViewNode({node: element, shouldScrollToStart: true});
			}
		};
		const handleHover = (sticker: GuildSticker | null) => {
			setHoveredSticker(sticker);
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
		if (hasNoStickersAtAll) {
			return (
				<PickerEmptyState
					icon={StickerIcon}
					title={i18n._(NO_STICKERS_AVAILABLE_DESCRIPTOR)}
					description={i18n._(JOIN_A_COMMUNITY_WITH_STICKERS_TO_GET_STARTED_DESCRIPTOR)}
					data-flx="channel.mobile-stickers-picker.picker-empty-state"
				/>
			);
		}
		const searchBar = (
			<StickerPickerSearchBar
				searchTerm={searchTerm}
				setSearchTerm={setSearchTerm}
				hoveredSticker={hoveredSticker}
				inputRef={searchInputRef}
				selectedRow={-1}
				selectedColumn={-1}
				sections={[]}
				onSelect={() => {}}
				onSelectionChange={() => {}}
				data-flx="channel.mobile-stickers-picker.sticker-picker-search-bar"
			/>
		);
		if (renderedStickers.length === 0 && searchTerm) {
			return (
				<div className={stickerStyles.searchResultsContainer} data-flx="channel.mobile-stickers-picker.div">
					{hasPortal ? (
						<ExpressionPickerHeaderPortal data-flx="channel.mobile-stickers-picker.expression-picker-header-portal">
							{searchBar}
						</ExpressionPickerHeaderPortal>
					) : (
						searchBar
					)}
					<PickerEmptyState
						icon={SmileySadIcon}
						title={i18n._(NO_STICKERS_FOUND_DESCRIPTOR)}
						description={i18n._(TRY_A_DIFFERENT_SEARCH_TERM_DESCRIPTOR)}
						data-flx="channel.mobile-stickers-picker.picker-empty-state--2"
					/>
				</div>
			);
		}
		return (
			<div className={mobileStyles.container} data-flx="channel.mobile-stickers-picker.div--2">
				{hasPortal ? (
					<ExpressionPickerHeaderPortal data-flx="channel.mobile-stickers-picker.expression-picker-header-portal--2">
						{searchBar}
					</ExpressionPickerHeaderPortal>
				) : null}
				<div className={mobileStyles.mobileEmojiPicker} data-flx="channel.mobile-stickers-picker.div--3">
					{!hasPortal && searchBar}
					<div className={mobileStyles.bodyWrapper} data-flx="channel.mobile-stickers-picker.div--4">
						<div
							className={mobileStyles.emojiPickerListWrapper}
							role="presentation"
							data-flx="channel.mobile-stickers-picker.presentation"
						>
							<Scroller
								ref={scrollerRef}
								className={`${mobileStyles.list} ${mobileStyles.listWrapper}`}
								key="mobile-stickers-picker-scroller"
								onResize={handleResize}
								data-flx="channel.mobile-stickers-picker.scroller"
							>
								{showPremiumUpsell && (
									<PremiumUpsellBanner
										message={stickerUpsellMessage}
										communityIds={allUpsell.summary.lockedCommunityIds}
										communityCount={stickerCommunityCount}
										previewContent={previewContent}
										data-flx="channel.mobile-stickers-picker.premium-upsell-banner"
									/>
								)}
								{virtualRows.map((row, index) => {
									const stickerRowIndex = virtualRows.slice(0, index).filter((r) => r.type === 'sticker-row').length;
									return (
										<div
											key={`${row.type}-${row.index}`}
											ref={
												row.type === 'header'
													? (el) => {
															if (el && 'category' in row) {
																categoryRefs.current.set(row.category, el);
															}
														}
													: undefined
											}
											data-flx="channel.mobile-stickers-picker.div--5"
										>
											<VirtualRowRenderer
												row={row}
												handleHover={handleHover}
												handleSelect={handleStickerSelect}
												gridColumns={gridColumns}
												hoveredSticker={hoveredSticker}
												selectedRow={-1}
												selectedColumn={-1}
												stickerRowIndex={stickerRowIndex}
												shouldScrollOnSelection={false}
												stickerRefs={stickerRefs}
												channel={channel}
												data-flx="channel.mobile-stickers-picker.virtual-row-renderer"
											/>
										</div>
									);
								})}
							</Scroller>
						</div>
					</div>
					<div className={mobileStyles.categoryListBottom} data-flx="channel.mobile-stickers-picker.div--6">
						<StickerPickerCategoryList
							stickersByGuildId={stickersByGuildId}
							handleCategoryClick={handleCategoryClick}
							horizontal={true}
							data-flx="channel.mobile-stickers-picker.sticker-picker-category-list"
						/>
					</div>
					<StickerPickerInspector
						hoveredSticker={hoveredSticker}
						style={{gridColumn: '1 / -1', gridRow: '4 / 5'}}
						data-flx="channel.mobile-stickers-picker.sticker-picker-inspector"
					/>
				</div>
			</div>
		);
	},
);
