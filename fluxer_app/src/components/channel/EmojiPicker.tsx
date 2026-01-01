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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as EmojiPickerActionCreators from '~/actions/EmojiPickerActionCreators';
import styles from '~/components/channel/EmojiPicker.module.css';
import {EmojiPickerCategoryList} from '~/components/channel/emoji-picker/EmojiPickerCategoryList';
import {EMOJI_SPRITE_SIZE} from '~/components/channel/emoji-picker/EmojiPickerConstants';
import {EmojiPickerInspector} from '~/components/channel/emoji-picker/EmojiPickerInspector';
import {EmojiPickerSearchBar} from '~/components/channel/emoji-picker/EmojiPickerSearchBar';
import {useEmojiCategories} from '~/components/channel/emoji-picker/hooks/useEmojiCategories';
import {useVirtualRows} from '~/components/channel/emoji-picker/hooks/useVirtualRows';
import {VirtualizedRow} from '~/components/channel/emoji-picker/VirtualRow';
import {PremiumUpsellBanner} from '~/components/channel/PremiumUpsellBanner';
import {ExpressionPickerHeaderContext, ExpressionPickerHeaderPortal} from '~/components/popouts/ExpressionPickerPopout';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {useForceUpdate} from '~/hooks/useForceUpdate';
import {useSearchInputAutofocus} from '~/hooks/useSearchInputAutofocus';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import UnicodeEmojis, {EMOJI_SPRITES} from '~/lib/UnicodeEmojis';
import ChannelStore from '~/stores/ChannelStore';
import EmojiStore, {type Emoji} from '~/stores/EmojiStore';
import {checkEmojiAvailability, shouldShowEmojiPremiumUpsell} from '~/utils/ExpressionPermissionUtils';
import {shouldShowPremiumFeatures} from '~/utils/PremiumUtils';

export const EmojiPicker = observer(
	({channelId, handleSelect}: {channelId?: string; handleSelect: (emoji: Emoji, shiftKey?: boolean) => void}) => {
		const headerContext = React.useContext(ExpressionPickerHeaderContext);
		if (!headerContext) {
			throw new Error(
				'EmojiPicker must be rendered inside ExpressionPickerPopout so that the header portal is available.',
			);
		}

		const [searchTerm, setSearchTerm] = React.useState('');
		const [hoveredEmoji, setHoveredEmoji] = React.useState<Emoji | null>(null);
		const [renderedEmojis, setRenderedEmojis] = React.useState<Array<Emoji>>([]);
		const [selectedRow, setSelectedRow] = React.useState(-1);
		const [selectedColumn, setSelectedColumn] = React.useState(-1);
		const [shouldScrollOnSelection, setShouldScrollOnSelection] = React.useState(false);
		const scrollerRef = React.useRef<ScrollerHandle>(null);
		const searchInputRef = React.useRef<HTMLInputElement>(null);
		const emojiRefs = React.useRef<Map<string, HTMLButtonElement>>(new Map());

		const {i18n} = useLingui();
		const channel = channelId ? (ChannelStore.getChannel(channelId) ?? null) : null;
		const categoryRefs = React.useRef<Map<string, HTMLDivElement>>(new Map());
		const forceUpdate = useForceUpdate();
		const skinTone = EmojiStore.skinTone;

		const spriteSheetSizes = React.useMemo(() => {
			const nonDiversitySize = [
				`${EMOJI_SPRITE_SIZE * EMOJI_SPRITES.NonDiversityPerRow}px`,
				`${EMOJI_SPRITE_SIZE * Math.ceil(UnicodeEmojis.numNonDiversitySprites / EMOJI_SPRITES.NonDiversityPerRow)}px`,
			].join(' ');

			const diversitySize = [
				`${EMOJI_SPRITE_SIZE * EMOJI_SPRITES.DiversityPerRow}px`,
				`${EMOJI_SPRITE_SIZE * Math.ceil(UnicodeEmojis.numDiversitySprites / EMOJI_SPRITES.DiversityPerRow)}px`,
			].join(' ');

			return {nonDiversitySize, diversitySize};
		}, []);

		React.useEffect(() => {
			const emojis = EmojiStore.search(channel, searchTerm).slice();
			setRenderedEmojis(emojis);
			if (emojis.length > 0) {
				setSelectedRow(0);
				setSelectedColumn(0);
			} else {
				setSelectedRow(-1);
				setSelectedColumn(-1);
				setHoveredEmoji(null);
			}
		}, [channel, searchTerm]);

		React.useEffect(() => {
			return ComponentDispatch.subscribe('EMOJI_PICKER_RERENDER', forceUpdate);
		});

		useSearchInputAutofocus(searchInputRef);

		const {favoriteEmojis, frequentlyUsedEmojis, customEmojisByGuildId, unicodeEmojisByCategory} =
			useEmojiCategories(renderedEmojis);
		const virtualRows = useVirtualRows(
			searchTerm,
			renderedEmojis,
			favoriteEmojis,
			frequentlyUsedEmojis,
			customEmojisByGuildId,
			unicodeEmojisByCategory,
		);

		const showPremiumUpsell = shouldShowPremiumFeatures() && shouldShowEmojiPremiumUpsell(channel);

		const sections = React.useMemo(() => {
			const result: Array<number> = [];
			for (const row of virtualRows) {
				if (row.type === 'emoji-row') {
					result.push(row.emojis.length);
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

		const handleHover = (emoji: Emoji | null, row?: number, column?: number) => {
			setHoveredEmoji(emoji);
			if (emoji && row !== undefined && column !== undefined) {
				handleSelectionChange(row, column, false);
			}
		};

		const handleEmojiSelect = React.useCallback(
			(emoji: Emoji, shiftKey?: boolean) => {
				const availability = checkEmojiAvailability(i18n, emoji, channel);
				if (!availability.canUse) {
					return;
				}

				EmojiPickerActionCreators.trackEmojiUsage(emoji);
				handleSelect(emoji, shiftKey);
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
					if (virtualRow.type === 'emoji-row') {
						if (currentRow === row && column < virtualRow.emojis.length) {
							const emoji = virtualRow.emojis[column];
							setHoveredEmoji(emoji);
							break;
						}
						currentRow++;
					}
				}
			},
			[virtualRows],
		);

		React.useEffect(() => {
			if (renderedEmojis.length > 0 && selectedRow === 0 && selectedColumn === 0 && !hoveredEmoji) {
				handleSelectionChange(0, 0, false);
			}
		}, [renderedEmojis, selectedRow, selectedColumn, hoveredEmoji, handleSelectionChange]);

		const handleSelectEmoji = React.useCallback(
			(row: number | null, column: number | null, event?: React.KeyboardEvent) => {
				if (row === null || column === null) {
					return;
				}

				let currentRow = 0;
				for (const virtualRow of virtualRows) {
					if (virtualRow.type === 'emoji-row') {
						if (currentRow === row && column < virtualRow.emojis.length) {
							const emoji = virtualRow.emojis[column];
							handleEmojiSelect(emoji, event?.shiftKey);
							return;
						}
						currentRow++;
					}
				}
			},
			[virtualRows, handleEmojiSelect],
		);

		return (
			<div className={styles.container}>
				<ExpressionPickerHeaderPortal>
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
					/>
				</ExpressionPickerHeaderPortal>
				<div className={styles.emojiPicker}>
					<div className={styles.bodyWrapper}>
						<div className={styles.emojiPickerListWrapper} role="presentation">
							<Scroller
								ref={scrollerRef}
								className={`${styles.list} ${styles.listWrapper}`}
								fade={false}
								key="emoji-picker-scroller"
								reserveScrollbarTrack={false}
							>
								{showPremiumUpsell && <PremiumUpsellBanner />}
								{virtualRows.map((row, index) => {
									const emojiRowIndex = virtualRows.slice(0, index).filter((r) => r.type === 'emoji-row').length;

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
											<VirtualizedRow
												row={row}
												handleHover={handleHover}
												handleSelect={handleEmojiSelect}
												skinTone={skinTone}
												spriteSheetSizes={spriteSheetSizes}
												channel={channel}
												hoveredEmoji={hoveredEmoji}
												selectedRow={selectedRow}
												selectedColumn={selectedColumn}
												emojiRowIndex={emojiRowIndex}
												shouldScrollOnSelection={shouldScrollOnSelection}
												emojiRefs={emojiRefs}
											/>
										</div>
									);
								})}
							</Scroller>
						</div>
					</div>
					<EmojiPickerInspector hoveredEmoji={hoveredEmoji} />
				</div>
				<EmojiPickerCategoryList
					customEmojisByGuildId={customEmojisByGuildId}
					unicodeEmojisByCategory={unicodeEmojisByCategory}
					handleCategoryClick={handleCategoryClick}
				/>
			</div>
		);
	},
);
