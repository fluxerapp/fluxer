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
import styles from '~/components/channel/EmojiPicker.module.css';
import gifStyles from '~/components/channel/GifPicker.module.css';
import {PremiumUpsellBanner} from '~/components/channel/PremiumUpsellBanner';
import {PickerEmptyState} from '~/components/channel/shared/PickerEmptyState';
import {useStickerCategories} from '~/components/channel/sticker-picker/hooks/useStickerCategories';
import {useVirtualRows} from '~/components/channel/sticker-picker/hooks/useVirtualRows';
import {StickerPickerCategoryList} from '~/components/channel/sticker-picker/StickerPickerCategoryList';
import {STICKERS_PER_ROW} from '~/components/channel/sticker-picker/StickerPickerConstants';
import {StickerPickerInspector} from '~/components/channel/sticker-picker/StickerPickerInspector';
import {StickerPickerSearchBar} from '~/components/channel/sticker-picker/StickerPickerSearchBar';
import {VirtualRowWrapper} from '~/components/channel/sticker-picker/VirtualRow';
import {ExpressionPickerHeaderPortal} from '~/components/popouts/ExpressionPickerPopout';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {useForceUpdate} from '~/hooks/useForceUpdate';
import {useSearchInputAutofocus} from '~/hooks/useSearchInputAutofocus';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import ChannelStore from '~/stores/ChannelStore';
import StickerStore from '~/stores/StickerStore';
import {checkStickerAvailability, shouldShowStickerPremiumUpsell} from '~/utils/ExpressionPermissionUtils';
import {shouldShowPremiumFeatures} from '~/utils/PremiumUtils';
export const StickersPicker = observer(
	({
		channelId,
		handleSelect,
	}: {
		channelId?: string;
		handleSelect: (sticker: GuildStickerRecord, shiftKey?: boolean) => void;
	}) => {
		const {t, i18n} = useLingui();
		const [searchTerm, setSearchTerm] = React.useState('');
		const [hoveredSticker, setHoveredSticker] = React.useState<GuildStickerRecord | null>(null);
		const [renderedStickers, setRenderedStickers] = React.useState<ReadonlyArray<GuildStickerRecord>>([]);
		const [allStickersForCategories, setAllStickersForCategories] = React.useState<ReadonlyArray<GuildStickerRecord>>(
			[],
		);
		const [selectedRow, setSelectedRow] = React.useState(-1);
		const [selectedColumn, setSelectedColumn] = React.useState(-1);
		const [shouldScrollOnSelection, setShouldScrollOnSelection] = React.useState(false);
		const scrollerRef = React.useRef<ScrollerHandle>(null);
		const searchInputRef = React.useRef<HTMLInputElement>(null);
		const stickerRefs = React.useRef<Map<string, HTMLButtonElement>>(new Map());

		const channel = channelId ? (ChannelStore.getChannel(channelId) ?? null) : null;
		const categoryRefs = React.useRef<Map<string, HTMLDivElement>>(new Map());
		const forceUpdate = useForceUpdate();

		React.useEffect(() => {
			const stickers = StickerStore.searchWithChannel(channel, searchTerm);
			setRenderedStickers(stickers);
			if (stickers.length > 0) {
				setSelectedRow(0);
				setSelectedColumn(0);
			} else {
				setSelectedRow(-1);
				setSelectedColumn(-1);
				setHoveredSticker(null);
			}
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
			STICKERS_PER_ROW,
		);

		const allStickers = React.useMemo(() => StickerStore.getAllStickers(), []);
		const hasNoStickersAtAll = allStickers.length === 0;

		const isSearching = searchTerm.trim().length > 0;
		const showPremiumUpsell = shouldShowPremiumFeatures() && shouldShowStickerPremiumUpsell(channel) && !isSearching;

		const sections = React.useMemo(() => {
			const result: Array<number> = [];
			for (const row of virtualRows) {
				if (row.type === 'sticker-row') {
					result.push(row.stickers.length);
				}
			}
			return result;
		}, [virtualRows]);

		const handleCategoryClick = (category: string) => {
			const element = categoryRefs.current.get(category);
			if (element) {
				scrollerRef.current?.scrollIntoViewNode({node: element, shouldScrollToStart: true});
			}
		};

		const handleHover = (sticker: GuildStickerRecord | null, row?: number, column?: number) => {
			setHoveredSticker(sticker);
			if (sticker && row !== undefined && column !== undefined) {
				handleSelectionChange(row, column, false);
			}
		};

		const handleStickerSelect = React.useCallback(
			(sticker: GuildStickerRecord, shiftKey?: boolean) => {
				const availability = checkStickerAvailability(i18n, sticker, channel);
				if (!availability.canUse) {
					return;
				}

				StickerPickerActionCreators.trackStickerUsage(sticker);
				handleSelect(sticker, shiftKey);
			},
			[channel, handleSelect, i18n],
		);

		const handleSelectionChange = React.useCallback(
			(row: number, column: number, shouldScroll = false) => {
				if (row < 0 || column < 0) {
					return;
				}
				setSelectedRow(row);
				setSelectedColumn(column);
				setShouldScrollOnSelection(shouldScroll);

				let currentRow = 0;
				for (const virtualRow of virtualRows) {
					if (virtualRow.type === 'sticker-row') {
						if (currentRow === row && column < virtualRow.stickers.length) {
							const sticker = virtualRow.stickers[column];
							setHoveredSticker(sticker);
							break;
						}
						currentRow++;
					}
				}
			},
			[virtualRows],
		);

		React.useEffect(() => {
			if (renderedStickers.length > 0 && selectedRow === 0 && selectedColumn === 0 && !hoveredSticker) {
				handleSelectionChange(0, 0, false);
			}
		}, [renderedStickers, selectedRow, selectedColumn, hoveredSticker, handleSelectionChange]);

		const handleSelectSticker = React.useCallback(
			(row: number | null, column: number | null, event?: React.KeyboardEvent) => {
				if (row === null || column === null) {
					return;
				}

				let currentRow = 0;
				for (const virtualRow of virtualRows) {
					if (virtualRow.type === 'sticker-row') {
						if (currentRow === row && column < virtualRow.stickers.length) {
							const sticker = virtualRow.stickers[column];
							handleStickerSelect(sticker, event?.shiftKey);
							return;
						}
						currentRow++;
					}
				}
			},
			[virtualRows, handleStickerSelect],
		);

		if (hasNoStickersAtAll) {
			return (
				<div className={gifStyles.gifPickerContainer}>
					<div className={gifStyles.gifPickerMain}>
						<PickerEmptyState
							icon={StickerIcon}
							title={t`No Stickers Available`}
							description={t`Join a community with stickers to get started!`}
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
			/>
		);

		return (
			<div className={styles.container}>
				<ExpressionPickerHeaderPortal>{renderSearchBar()}</ExpressionPickerHeaderPortal>
				<div className={styles.emojiPicker}>
					<div className={styles.bodyWrapper}>
						<div className={styles.emojiPickerListWrapper} role="presentation">
							<Scroller
								ref={scrollerRef}
								className={`${styles.list} ${styles.listWrapper}`}
								fade={false}
								key="stickers-picker-scroller"
								reserveScrollbarTrack={true}
							>
								{showPremiumUpsell && <PremiumUpsellBanner />}
								{virtualRows.map((row, index) => {
									const stickerRowIndex = virtualRows.slice(0, index).filter((r) => r.type === 'sticker-row').length;
									const needsSpacingAfter = row.type === 'sticker-row' && virtualRows[index + 1]?.type === 'header';

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
											style={row.type === 'sticker-row' && needsSpacingAfter ? {marginBottom: '12px'} : undefined}
										>
											<VirtualRowWrapper
												row={row}
												handleHover={handleHover}
												handleSelect={handleStickerSelect}
												gridColumns={STICKERS_PER_ROW}
												hoveredSticker={hoveredSticker}
												selectedRow={selectedRow}
												selectedColumn={selectedColumn}
												stickerRowIndex={stickerRowIndex}
												shouldScrollOnSelection={shouldScrollOnSelection}
												stickerRefs={stickerRefs}
												channel={channel}
											/>
										</div>
									);
								})}
							</Scroller>
							{renderedStickers.length === 0 && (
								<div className={styles.emptyState}>
									<div className={styles.emptyStateInner}>
										<div className={styles.emptyIcon}>
											<SmileySadIcon weight="duotone" />
										</div>
										<div className={styles.emptyLabel}>{t`No stickers match your search`}</div>
									</div>
								</div>
							)}
						</div>
					</div>
					<StickerPickerInspector hoveredSticker={hoveredSticker} />
				</div>
				<StickerPickerCategoryList stickersByGuildId={stickersByGuildId} handleCategoryClick={handleCategoryClick} />
			</div>
		);
	},
);
