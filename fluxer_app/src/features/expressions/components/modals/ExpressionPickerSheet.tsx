// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {MobileEmojiPicker} from '@app/features/channel/components/MobileEmojiPicker';
import {MobileMemesPicker} from '@app/features/channel/components/MobileMemesPicker';
import {MobileStickersPicker} from '@app/features/channel/components/MobileStickersPicker';
import {GifPicker} from '@app/features/channel/components/pickers/gif/GifPicker';
import * as ExpressionPickerCommands from '@app/features/emoji/commands/ExpressionPickerCommands';
import ExpressionPicker from '@app/features/emoji/state/ExpressionPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import styles from '@app/features/expressions/components/modals/ExpressionPickerSheet.module.css';
import {
	ExpressionPickerHeaderContext,
	type ExpressionPickerTabType,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import * as StickerSendUtils from '@app/features/expressions/utils/StickerSendUtils';
import {
	EMOJIS_DESCRIPTOR,
	EXPRESSION_PICKER_CATEGORIES_DESCRIPTOR,
	GIFS_DESCRIPTOR,
	MEDIA_DESCRIPTOR,
	STICKERS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {type SegmentedTab, SegmentedTabs} from '@app/features/ui/segmented_tabs/SegmentedTabs';
import type {MessageDescriptor} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

interface ExpressionPickerCategoryDescriptor {
	type: ExpressionPickerTabType;
	label: MessageDescriptor;
	renderComponent: (props: {
		channelId?: string;
		onSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
		onClose: () => void;
		searchTerm?: string;
		setSearchTerm?: (term: string) => void;
		setHoveredEmoji?: (emoji: FlatEmoji | null) => void;
	}) => React.ReactNode;
}

const EXPRESSION_PICKER_CATEGORY_DESCRIPTORS: Array<ExpressionPickerCategoryDescriptor> = [
	{
		type: 'gifs' as const,
		label: GIFS_DESCRIPTOR,
		renderComponent: ({onClose}) => (
			<div
				className={styles.pickerContent}
				data-flx="expressions.expression-picker-sheet.render-component.picker-content"
			>
				<GifPicker onClose={onClose} data-flx="expressions.expression-picker-sheet.render-component.gif-picker" />
			</div>
		),
	},
	{
		type: 'memes' as const,
		label: MEDIA_DESCRIPTOR,
		renderComponent: ({onClose}) => (
			<div
				className={styles.pickerContent}
				data-flx="expressions.expression-picker-sheet.render-component.picker-content--2"
			>
				<MobileMemesPicker
					onClose={onClose}
					data-flx="expressions.expression-picker-sheet.render-component.mobile-memes-picker"
				/>
			</div>
		),
	},
	{
		type: 'stickers' as const,
		label: STICKERS_DESCRIPTOR,
		renderComponent: ({channelId, onClose}) => {
			const handleStickerSelect = (sticker: GuildSticker, shiftKey?: boolean) => {
				if (channelId) {
					if (StickerSendUtils.shouldSetPendingSticker(channelId)) {
						StickerSendUtils.setPendingSticker(channelId, sticker);
					} else {
						ComponentBus.dispatch('STICKER_SELECT', {sticker});
					}
					if (!shiftKey) {
						onClose?.();
					}
				}
			};
			return (
				<div
					className={styles.pickerContent}
					data-flx="expressions.expression-picker-sheet.render-component.picker-content--3"
				>
					<MobileStickersPicker
						channelId={channelId}
						handleSelect={handleStickerSelect}
						data-flx="expressions.expression-picker-sheet.render-component.mobile-stickers-picker"
					/>
				</div>
			);
		},
	},
	{
		type: 'emojis' as const,
		label: EMOJIS_DESCRIPTOR,
		renderComponent: ({channelId, onSelect, searchTerm, setSearchTerm}) => (
			<div
				className={styles.pickerContent}
				data-flx="expressions.expression-picker-sheet.render-component.picker-content--4"
			>
				<MobileEmojiPicker
					channelId={channelId}
					handleSelect={onSelect}
					externalSearchTerm={searchTerm}
					externalSetSearchTerm={setSearchTerm}
					data-flx="expressions.expression-picker-sheet.render-component.mobile-emoji-picker"
				/>
			</div>
		),
	},
];

interface ExpressionPickerSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId?: string;
	onEmojiSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
	visibleTabs?: Array<ExpressionPickerTabType>;
	selectedTab?: ExpressionPickerTabType;
	onTabChange?: (tab: ExpressionPickerTabType) => void;
	zIndex?: number;
}

export const ExpressionPickerSheet = observer(
	({
		isOpen,
		onClose,
		channelId,
		onEmojiSelect,
		visibleTabs = ['gifs', 'memes', 'stickers', 'emojis'],
		selectedTab: controlledSelectedTab,
		onTabChange,
		zIndex,
	}: ExpressionPickerSheetProps) => {
		const {i18n} = useLingui();
		const categories = useMemo(
			() =>
				EXPRESSION_PICKER_CATEGORY_DESCRIPTORS.filter(
					(category) => visibleTabs.includes(category.type) && (category.type !== 'gifs' || RuntimeConfig.gifEnabled),
				).map((category) => ({
					type: category.type,
					label: i18n._(category.label),
					renderComponent: category.renderComponent,
				})),
			[visibleTabs, i18n.locale, RuntimeConfig.gifEnabled],
		);
		const [internalSelectedTab, setInternalSelectedTab] = useState<ExpressionPickerTabType>(
			() => categories[0]?.type || 'emojis',
		);
		const [emojiSearchTerm, setEmojiSearchTerm] = useState('');
		const [_hoveredEmoji, setHoveredEmoji] = useState<FlatEmoji | null>(null);
		const storeSelectedTab = ExpressionPicker.selectedTab;
		const selectedTab = storeSelectedTab ?? controlledSelectedTab ?? internalSelectedTab;
		const setSelectedTab = useCallback(
			(tab: ExpressionPickerTabType) => {
				if (onTabChange) {
					onTabChange(tab);
					return;
				}
				const pickerChannelId = ExpressionPicker.channelId;
				if (pickerChannelId) {
					ExpressionPickerCommands.setTab(tab);
				} else {
					setInternalSelectedTab(tab);
				}
			},
			[onTabChange],
		);
		const selectedCategory = categories.find((category) => category.type === selectedTab) || categories[0];
		useEffect(() => {
			if (!isOpen) return;
			if (channelId && ExpressionPicker.channelId !== channelId) {
				ExpressionPickerCommands.open(channelId, selectedTab);
			}
		}, [isOpen, channelId, selectedTab]);
		const handleEmojiSelect = useCallback(
			(emoji: FlatEmoji, shiftKey?: boolean) => {
				onEmojiSelect(emoji, shiftKey);
				if (!shiftKey) {
					onClose();
				}
			},
			[onEmojiSelect, onClose],
		);
		const showTabs = categories.length > 1;
		const segmentedTabs: Array<SegmentedTab<ExpressionPickerTabType>> = useMemo(
			() => categories.map((category) => ({id: category.type, label: category.label})),
			[categories],
		);
		const [headerPortalElement, setHeaderPortalElement] = useState<HTMLDivElement | null>(null);
		const headerPortalCallback = useCallback((node: HTMLDivElement | null) => {
			setHeaderPortalElement(node);
		}, []);
		const headerContextValue = useMemo(() => ({headerPortalElement}), [headerPortalElement]);
		const headerContent = (
			<>
				{showTabs ? (
					<SegmentedTabs
						tabs={segmentedTabs}
						selectedTab={selectedTab}
						onTabChange={setSelectedTab}
						ariaLabel={i18n._(EXPRESSION_PICKER_CATEGORIES_DESCRIPTOR)}
						data-flx="expressions.expression-picker-sheet.segmented-tabs"
					/>
				) : null}
				<div
					ref={headerPortalCallback}
					className={styles.headerPortal}
					data-flx="expressions.expression-picker-sheet.header-portal"
				/>
			</>
		);
		return (
			<ExpressionPickerHeaderContext.Provider value={headerContextValue}>
				<BottomSheet
					isOpen={isOpen}
					onClose={onClose}
					snapPoints={[0, 1]}
					initialSnap={1}
					disablePadding={true}
					disableDefaultHeader={true}
					headerSlot={headerContent}
					showCloseButton={false}
					zIndex={zIndex}
					data-flx="expressions.expression-picker-sheet.bottom-sheet"
				>
					<div className={styles.container} data-flx="expressions.expression-picker-sheet.container">
						<div className={styles.contentContainer} data-flx="expressions.expression-picker-sheet.content-container">
							<div className={styles.contentInner} data-flx="expressions.expression-picker-sheet.content-inner">
								{selectedCategory.renderComponent({
									channelId,
									onSelect: handleEmojiSelect,
									onClose,
									searchTerm: selectedTab === 'emojis' ? emojiSearchTerm : undefined,
									setSearchTerm: selectedTab === 'emojis' ? setEmojiSearchTerm : undefined,
									setHoveredEmoji: selectedTab === 'emojis' ? setHoveredEmoji : undefined,
								})}
							</div>
						</div>
					</div>
				</BottomSheet>
			</ExpressionPickerHeaderContext.Provider>
		);
	},
);
