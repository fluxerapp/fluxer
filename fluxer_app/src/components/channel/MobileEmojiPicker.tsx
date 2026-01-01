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

import {observer} from 'mobx-react-lite';
import React from 'react';
import {EmojiPickerCategoryList} from '~/components/channel/emoji-picker/EmojiPickerCategoryList';
import {EMOJI_SPRITE_SIZE} from '~/components/channel/emoji-picker/EmojiPickerConstants';
import {EmojiPickerSearchBar} from '~/components/channel/emoji-picker/EmojiPickerSearchBar';
import {useEmojiCategories} from '~/components/channel/emoji-picker/hooks/useEmojiCategories';
import {useVirtualRows} from '~/components/channel/emoji-picker/hooks/useVirtualRows';
import {VirtualizedRow} from '~/components/channel/emoji-picker/VirtualRow';
import mobileStyles from '~/components/channel/MobileEmojiPicker.module.css';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '~/components/popouts/ExpressionPickerPopout';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {useForceUpdate} from '~/hooks/useForceUpdate';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import UnicodeEmojis, {EMOJI_SPRITES} from '~/lib/UnicodeEmojis';
import ChannelStore from '~/stores/ChannelStore';
import EmojiStore, {type Emoji} from '~/stores/EmojiStore';

export const MobileEmojiPicker = observer(
	({
		channelId,
		handleSelect,
		externalSearchTerm,
		externalSetSearchTerm,
		hideSearchBar = false,
	}: {
		channelId?: string;
		handleSelect: (emoji: Emoji, shiftKey?: boolean) => void;
		externalSearchTerm?: string;
		externalSetSearchTerm?: (term: string) => void;
		hideSearchBar?: boolean;
	}) => {
		const headerPortalContext = useExpressionPickerHeaderPortal();
		const hasPortal = Boolean(headerPortalContext?.headerPortalElement);

		const [internalSearchTerm, setInternalSearchTerm] = React.useState('');
		const [hoveredEmoji, setHoveredEmoji] = React.useState<Emoji | null>(null);
		const [renderedEmojis, setRenderedEmojis] = React.useState<Array<Emoji>>([]);
		const scrollerRef = React.useRef<ScrollerHandle>(null);
		const emojiRefs = React.useRef<Map<string, HTMLButtonElement>>(new Map());

		const channel = channelId ? (ChannelStore.getChannel(channelId) ?? null) : null;
		const categoryRefs = React.useRef<Map<string, HTMLDivElement>>(new Map());
		const forceUpdate = useForceUpdate();
		const skinTone = EmojiStore.skinTone;

		const searchTerm = externalSearchTerm ?? internalSearchTerm;
		const setSearchTerm = externalSetSearchTerm ?? setInternalSearchTerm;

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
			const emojis = EmojiStore.search(null, searchTerm).slice();
			setRenderedEmojis(emojis);
		}, [searchTerm]);

		React.useEffect(() => {
			return ComponentDispatch.subscribe('EMOJI_PICKER_RERENDER', forceUpdate);
		});

		const {customEmojisByGuildId, unicodeEmojisByCategory, favoriteEmojis, frequentlyUsedEmojis} =
			useEmojiCategories(renderedEmojis);
		const virtualRows = useVirtualRows(
			searchTerm,
			renderedEmojis,
			favoriteEmojis,
			frequentlyUsedEmojis,
			customEmojisByGuildId,
			unicodeEmojisByCategory,
			8,
		);

		const handleCategoryClick = (category: string) => {
			const element = categoryRefs.current.get(category);
			if (element) {
				scrollerRef.current?.scrollIntoViewNode({node: element, shouldScrollToStart: true});
			}
		};

		const handleHover = (emoji: Emoji | null) => {
			setHoveredEmoji(emoji);
		};

		const searchBar = !hideSearchBar ? (
			<EmojiPickerSearchBar searchTerm={searchTerm} setSearchTerm={setSearchTerm} hoveredEmoji={hoveredEmoji} />
		) : null;

		return (
			<div className={mobileStyles.container}>
				{hasPortal && searchBar ? <ExpressionPickerHeaderPortal>{searchBar}</ExpressionPickerHeaderPortal> : null}
				<div className={mobileStyles.mobileEmojiPicker}>
					{!hasPortal && searchBar}
					<div className={mobileStyles.bodyWrapper}>
						<div className={mobileStyles.emojiPickerListWrapper} role="presentation">
							<Scroller
								ref={scrollerRef}
								className={`${mobileStyles.list} ${mobileStyles.listWrapper}`}
								key="mobile-emoji-picker-scroller"
							>
								{virtualRows.map((row) => (
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
											handleSelect={handleSelect}
											skinTone={skinTone}
											spriteSheetSizes={spriteSheetSizes}
											channel={channel}
											gridColumns={8}
											hoveredEmoji={hoveredEmoji}
											selectedRow={-1}
											selectedColumn={-1}
											emojiRowIndex={0}
											emojiRefs={emojiRefs}
										/>
									</div>
								))}
							</Scroller>
						</div>
					</div>
					<div className={mobileStyles.categoryListBottom}>
						<EmojiPickerCategoryList
							customEmojisByGuildId={customEmojisByGuildId}
							unicodeEmojisByCategory={unicodeEmojisByCategory}
							handleCategoryClick={handleCategoryClick}
							horizontal={true}
						/>
					</div>
				</div>
			</div>
		);
	},
);
