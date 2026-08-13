// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useSearchInputAutofocus} from '@app/features/app/hooks/useSearchInputAutofocus';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {EmojiPickerCategoryList} from '@app/features/channel/components/emoji_picker/EmojiPickerCategoryList';
import {getEmojiGridColumns} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import {EmojiPickerInspector} from '@app/features/channel/components/emoji_picker/EmojiPickerInspector';
import {EmojiPickerSearchBar} from '@app/features/channel/components/emoji_picker/EmojiPickerSearchBar';
import {useEmojiCategories} from '@app/features/channel/components/emoji_picker/hooks/useEmojiCategories';
import {useVirtualRows} from '@app/features/channel/components/emoji_picker/hooks/useVirtualRows';
import {VirtualizedRow} from '@app/features/channel/components/emoji_picker/VirtualRow';
import {PremiumUpsellBanner} from '@app/features/channel/components/PremiumUpsellBanner';
import premiumStyles from '@app/features/channel/components/PremiumUpsellBanner.module.css';
import {useScrollerViewport} from '@app/features/channel/components/pickers/shared/useScrollerViewport';
import Channels from '@app/features/channel/state/Channels';
import * as EmojiPickerCommands from '@app/features/emoji/commands/EmojiPickerCommands';
import Emoji, {normalizeEmojiSearchQuery} from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	ExpressionPickerHeaderContext,
	ExpressionPickerHeaderPortal,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {
	checkEmojiAvailability,
	shouldShowEmojiPremiumUpsell,
} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import {getEmojiDisplayDataWithSkinTone} from '@app/features/expressions/utils/SkinToneUtils';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {usePremiumUpsellData} from '@app/features/premium/hooks/usePremiumUpsellData';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
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
		const headerContext = useContext(ExpressionPickerHeaderContext);
		if (!headerContext) {
			throw new Error(
				'EmojiPicker must be rendered inside ExpressionPickerPopout so that the header portal is available.',
			);
		}
		const [searchTerm, setSearchTerm] = useState('');
		const [hoveredEmoji, setHoveredEmoji] = useState<FlatEmoji | null>(null);
		const [selectedRow, setSelectedRow] = useState(-1);
		const [selectedColumn, setSelectedColumn] = useState(-1);
		const [shouldScrollOnSelection, setShouldScrollOnSelection] = useState(false);
		const scrollerRef = useRef<ScrollerHandle>(null);
		const {viewportSize, handleResize} = useScrollerViewport(scrollerRef);
		const gridColumns = useMemo(() => getEmojiGridColumns(viewportSize.width), [viewportSize.width]);
		const searchInputRef = useRef<HTMLInputElement>(null);
		const emojiRefs = useRef<Map<string, HTMLButtonElement>>(new Map());
		const normalizedSearchTerm = useMemo(() => normalizeEmojiSearchQuery(searchTerm), [searchTerm]);
		const {i18n} = useLingui();
		const channel = channelId ? (Channels.getChannel(channelId) ?? null) : null;
		const categoryRefs = useRef<Map<string, HTMLDivElement>>(new Map());
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
			(emoji: FlatEmoji) => {
				const key = emoji.id ?? emoji.uniqueName ?? emoji.name ?? `${emoji.guildId ?? 'unicode'}-preview`;
				const {url: fallbackDisplayUrl} = getEmojiDisplayDataWithSkinTone(emoji, skinTone);
				const displayUrl = emoji.id
					? AvatarUtils.getEmojiURL({id: emoji.id, animated: Boolean(emoji.animated) && shouldAnimateEmoji})
					: fallbackDisplayUrl;
				const content = displayUrl ? (
					<img
						src={displayUrl}
						alt={emoji.name}
						loading="lazy"
						data-flx="channel.emoji-picker.render-emoji-preview-item.img"
					/>
				) : (
					<span
						className={premiumStyles.previewEmojiText}
						data-flx="channel.emoji-picker.render-emoji-preview-item.span"
					>
						{emoji.name ?? emoji.uniqueName}
					</span>
				);
				return (
					<div
						className={premiumStyles.previewItem}
						key={key}
						data-flx="channel.emoji-picker.render-emoji-preview-item.div"
					>
						{content}
					</div>
				);
			},
			[shouldAnimateEmoji, skinTone],
		);
		useEffect(() => {
			const handleEmojiDataUpdated = () => {
				setEmojiDataVersion((version) => version + 1);
			};
			return ComponentDispatch.subscribe('EMOJI_PICKER_RERENDER', handleEmojiDataUpdated);
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
		const virtualRows = useVirtualRows(
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
		const sections = useMemo(() => {
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
		const handleHover = (emoji: FlatEmoji | null, row?: number, column?: number) => {
			setHoveredEmoji(emoji);
			if (emoji && row !== undefined && column !== undefined) {
				handleSelectionChange(row, column, false);
			}
		};
		const handleEmojiSelect = useCallback(
			(emoji: FlatEmoji, shiftKey?: boolean) => {
				const availability = checkEmojiAvailability(i18n, emoji, channel);
				if (!availability.canUse) {
					return;
				}
				EmojiPickerCommands.trackEmojiUsage(emoji);
				handleSelect(emoji, shiftKey);
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
		useEffect(() => {
			if (renderedEmojis.length > 0 && selectedRow === 0 && selectedColumn === 0 && !hoveredEmoji) {
				handleSelectionChange(0, 0, false);
			}
		}, [renderedEmojis, selectedRow, selectedColumn, hoveredEmoji, handleSelectionChange]);
		const handleSelectEmoji = useCallback(
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
								onResize={handleResize}
								data-emoji-picker-scroll-root="true"
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
								{virtualRows.map((row, index) => {
									const emojiRowIndex = virtualRows.slice(0, index).filter((r) => r.type === 'emoji-row').length;
									const needsSpacingAfter = row.type === 'emoji-row' && virtualRows[index + 1]?.type === 'header';
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
											style={row.type === 'emoji-row' && needsSpacingAfter ? {marginBottom: '12px'} : undefined}
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
												hoveredEmoji={hoveredEmoji}
												selectedRow={selectedRow}
												selectedColumn={selectedColumn}
												emojiRowIndex={emojiRowIndex}
												shouldScrollOnSelection={shouldScrollOnSelection}
												emojiRefs={emojiRefs}
												data-flx="channel.emoji-picker.virtualized-row"
											/>
										</div>
									);
								})}
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
