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

import {useLingui} from '@lingui/react/macro';
import {SmileySadIcon, StickerIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as StickerPickerActionCreators from '~/actions/StickerPickerActionCreators';
import mobileStyles from '~/components/channel/MobileEmojiPicker.module.css';
import {PremiumUpsellBanner} from '~/components/channel/PremiumUpsellBanner';
import stickerStyles from '~/components/channel/StickersPicker.module.css';
import {PickerEmptyState} from '~/components/channel/shared/PickerEmptyState';
import {useStickerCategories} from '~/components/channel/sticker-picker/hooks/useStickerCategories';
import {useVirtualRows} from '~/components/channel/sticker-picker/hooks/useVirtualRows';
import {StickerPickerCategoryList} from '~/components/channel/sticker-picker/StickerPickerCategoryList';
import {STICKERS_PER_ROW_MOBILE} from '~/components/channel/sticker-picker/StickerPickerConstants';
import {StickerPickerInspector} from '~/components/channel/sticker-picker/StickerPickerInspector';
import {StickerPickerSearchBar} from '~/components/channel/sticker-picker/StickerPickerSearchBar';
import {VirtualRowRenderer} from '~/components/channel/sticker-picker/VirtualRow';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '~/components/popouts/ExpressionPickerPopout';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {useForceUpdate} from '~/hooks/useForceUpdate';
import {useSearchInputAutofocus} from '~/hooks/useSearchInputAutofocus';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import ChannelStore from '~/stores/ChannelStore';
import StickerStore from '~/stores/StickerStore';
import {shouldShowStickerPremiumUpsell} from '~/utils/ExpressionPermissionUtils';
import {shouldShowPremiumFeatures} from '~/utils/PremiumUtils';
export const MobileStickersPicker = observer(
	({
		channelId,
		handleSelect,
	}: {
		channelId?: string;
		handleSelect: (sticker: GuildStickerRecord, shiftKey?: boolean) => void;
	}) => {
		const {t} = useLingui();
		const headerPortalContext = useExpressionPickerHeaderPortal();
		const hasPortal = Boolean(headerPortalContext?.headerPortalElement);

		const [searchTerm, setSearchTerm] = React.useState('');
		const [hoveredSticker, setHoveredSticker] = React.useState<GuildStickerRecord | null>(null);
		const [renderedStickers, setRenderedStickers] = React.useState<ReadonlyArray<GuildStickerRecord>>([]);
		const [allStickersForCategories, setAllStickersForCategories] = React.useState<ReadonlyArray<GuildStickerRecord>>(
			[],
		);
		const scrollerRef = React.useRef<ScrollerHandle>(null);
		const searchInputRef = React.useRef<HTMLInputElement>(null);
		const stickerRefs = React.useRef<Map<string, HTMLButtonElement>>(new Map());

		const channel = channelId ? (ChannelStore.getChannel(channelId) ?? null) : null;
		const categoryRefs = React.useRef<Map<string, HTMLDivElement>>(new Map());
		const forceUpdate = useForceUpdate();

		React.useEffect(() => {
			const stickers = StickerStore.searchWithChannel(channel, searchTerm);
			setRenderedStickers(stickers);
		}, [channel, searchTerm]);

		React.useEffect(() => {
			setAllStickersForCategories(StickerStore.getAllStickers());
		}, []);

		React.useEffect(() => {
			return ComponentDispatch.subscribe('STICKER_PICKER_RERENDER', forceUpdate);
		});

		useSearchInputAutofocus(searchInputRef);

		const {favoriteStickers, frequentlyUsedStickers, stickersByGuildId} = useStickerCategories(
			allStickersForCategories,
			renderedStickers,
		);
		const virtualRows = useVirtualRows(
			searchTerm,
			renderedStickers,
			favoriteStickers,
			frequentlyUsedStickers,
			stickersByGuildId,
			STICKERS_PER_ROW_MOBILE,
		);

		const allStickers = React.useMemo(() => StickerStore.getAllStickers(), []);
		const hasNoStickersAtAll = allStickers.length === 0;

		const showPremiumUpsell = shouldShowPremiumFeatures() && shouldShowStickerPremiumUpsell(channel);

		const handleCategoryClick = (category: string) => {
			const element = categoryRefs.current.get(category);
			if (element) {
				scrollerRef.current?.scrollIntoViewNode({node: element, shouldScrollToStart: true});
			}
		};

		const handleHover = (sticker: GuildStickerRecord | null) => {
			setHoveredSticker(sticker);
		};

		const handleStickerSelect = React.useCallback(
			(sticker: GuildStickerRecord, shiftKey?: boolean) => {
				StickerPickerActionCreators.trackStickerUsage(sticker);
				handleSelect(sticker, shiftKey);
			},
			[handleSelect],
		);

		if (hasNoStickersAtAll) {
			return (
				<PickerEmptyState
					icon={StickerIcon}
					title={t`No Stickers Available`}
					description={t`Join a community with stickers to get started!`}
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
			/>
		);

		if (renderedStickers.length === 0 && searchTerm) {
			return (
				<div className={stickerStyles.searchResultsContainer}>
					{hasPortal ? <ExpressionPickerHeaderPortal>{searchBar}</ExpressionPickerHeaderPortal> : searchBar}
					<PickerEmptyState
						icon={SmileySadIcon}
						title={t`No Stickers Found`}
						description={t`Try a different search term`}
					/>
				</div>
			);
		}

		return (
			<div className={mobileStyles.container}>
				{hasPortal ? <ExpressionPickerHeaderPortal>{searchBar}</ExpressionPickerHeaderPortal> : null}
				<div className={mobileStyles.mobileEmojiPicker}>
					{!hasPortal && searchBar}
					<div className={mobileStyles.bodyWrapper}>
						<div className={mobileStyles.emojiPickerListWrapper} role="presentation">
							<Scroller
								ref={scrollerRef}
								className={`${mobileStyles.list} ${mobileStyles.listWrapper}`}
								key="mobile-stickers-picker-scroller"
							>
								{showPremiumUpsell && <PremiumUpsellBanner />}
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
										>
											<VirtualRowRenderer
												row={row}
												handleHover={handleHover}
												handleSelect={handleStickerSelect}
												gridColumns={STICKERS_PER_ROW_MOBILE}
												hoveredSticker={hoveredSticker}
												selectedRow={-1}
												selectedColumn={-1}
												stickerRowIndex={stickerRowIndex}
												shouldScrollOnSelection={false}
												stickerRefs={stickerRefs}
												channel={channel}
											/>
										</div>
									);
								})}
							</Scroller>
						</div>
					</div>
					<div className={mobileStyles.categoryListBottom}>
						<StickerPickerCategoryList
							stickersByGuildId={stickersByGuildId}
							handleCategoryClick={handleCategoryClick}
							horizontal={true}
						/>
					</div>
					<StickerPickerInspector hoveredSticker={hoveredSticker} />
				</div>
			</div>
		);
	},
);
