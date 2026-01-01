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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ExpressionPickerActionCreators from '~/actions/ExpressionPickerActionCreators';
import {MobileEmojiPicker} from '~/components/channel/MobileEmojiPicker';
import {MobileMemesPicker} from '~/components/channel/MobileMemesPicker';
import {MobileStickersPicker} from '~/components/channel/MobileStickersPicker';
import {GifPicker} from '~/components/channel/pickers/gif/GifPicker';
import styles from '~/components/modals/ExpressionPickerSheet.module.css';
import {ExpressionPickerHeaderContext, type ExpressionPickerTabType} from '~/components/popouts/ExpressionPickerPopout';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {type SegmentedTab, SegmentedTabs} from '~/components/uikit/SegmentedTabs/SegmentedTabs';
import * as StickerSendUtils from '~/lib/StickerSendUtils';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import type {Emoji} from '~/stores/EmojiStore';
import ExpressionPickerStore from '~/stores/ExpressionPickerStore';

interface ExpressionPickerCategoryDescriptor {
	type: ExpressionPickerTabType;
	label: MessageDescriptor;
	renderComponent: (props: {
		channelId?: string;
		onSelect: (emoji: Emoji, shiftKey?: boolean) => void;
		onClose: () => void;
		searchTerm?: string;
		setSearchTerm?: (term: string) => void;
		setHoveredEmoji?: (emoji: Emoji | null) => void;
	}) => React.ReactNode;
}

const EXPRESSION_PICKER_CATEGORY_DESCRIPTORS: Array<ExpressionPickerCategoryDescriptor> = [
	{
		type: 'gifs' as const,
		label: msg`GIFs`,
		renderComponent: ({onClose}) => (
			<div className={styles.pickerContent}>
				<GifPicker onClose={onClose} />
			</div>
		),
	},
	{
		type: 'memes' as const,
		label: msg`Media`,
		renderComponent: ({onClose}) => (
			<div className={styles.pickerContent}>
				<MobileMemesPicker onClose={onClose} />
			</div>
		),
	},
	{
		type: 'stickers' as const,
		label: msg`Stickers`,
		renderComponent: ({channelId, onClose}) => {
			const handleStickerSelect = (sticker: GuildStickerRecord, shiftKey?: boolean) => {
				if (channelId) {
					StickerSendUtils.handleStickerSelect(channelId, sticker);
					if (!shiftKey) {
						onClose?.();
					}
				}
			};

			return (
				<div className={styles.pickerContent}>
					<MobileStickersPicker channelId={channelId} handleSelect={handleStickerSelect} />
				</div>
			);
		},
	},
	{
		type: 'emojis' as const,
		label: msg`Emojis`,
		renderComponent: ({channelId, onSelect, searchTerm, setSearchTerm}) => (
			<div className={styles.pickerContent}>
				<MobileEmojiPicker
					channelId={channelId}
					handleSelect={onSelect}
					externalSearchTerm={searchTerm}
					externalSetSearchTerm={setSearchTerm}
				/>
			</div>
		),
	},
];

interface ExpressionPickerSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId?: string;
	onEmojiSelect: (emoji: Emoji, shiftKey?: boolean) => void;
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
		const {t} = useLingui();
		const categories = React.useMemo(
			() =>
				EXPRESSION_PICKER_CATEGORY_DESCRIPTORS.filter((category) => visibleTabs.includes(category.type)).map(
					(category) => ({
						type: category.type,
						label: t(category.label),
						renderComponent: category.renderComponent,
					}),
				),
			[visibleTabs, t],
		);

		const [internalSelectedTab, setInternalSelectedTab] = React.useState<ExpressionPickerTabType>(
			() => categories[0]?.type || 'emojis',
		);

		const [emojiSearchTerm, setEmojiSearchTerm] = React.useState('');
		const [_hoveredEmoji, setHoveredEmoji] = React.useState<Emoji | null>(null);

		const storeSelectedTab = ExpressionPickerStore.selectedTab;
		const selectedTab = storeSelectedTab ?? controlledSelectedTab ?? internalSelectedTab;

		const setSelectedTab = React.useCallback(
			(tab: ExpressionPickerTabType) => {
				if (onTabChange) {
					onTabChange(tab);
					return;
				}

				const pickerChannelId = ExpressionPickerStore.channelId;
				if (pickerChannelId) {
					ExpressionPickerActionCreators.setTab(tab);
				} else {
					setInternalSelectedTab(tab);
				}
			},
			[onTabChange],
		);

		const selectedCategory = categories.find((category) => category.type === selectedTab) || categories[0];

		React.useEffect(() => {
			if (!isOpen) return;

			if (channelId && ExpressionPickerStore.channelId !== channelId) {
				ExpressionPickerActionCreators.open(channelId, selectedTab);
			}

			const timer = setTimeout(() => {
				const firstInput = document.querySelector('input[type="text"]') as HTMLInputElement | null;
				if (firstInput) {
					firstInput.focus();
				}
			}, 150);
			return () => clearTimeout(timer);
		}, [isOpen, channelId, selectedTab, t]);

		const handleEmojiSelect = React.useCallback(
			(emoji: Emoji, shiftKey?: boolean) => {
				onEmojiSelect(emoji, shiftKey);
				if (!shiftKey) {
					onClose();
				}
			},
			[onEmojiSelect, onClose],
		);

		const showTabs = categories.length > 1;

		const segmentedTabs: Array<SegmentedTab<ExpressionPickerTabType>> = React.useMemo(
			() => categories.map((category) => ({id: category.type, label: category.label})),
			[categories],
		);

		const [headerPortalElement, setHeaderPortalElement] = React.useState<HTMLDivElement | null>(null);

		const headerPortalCallback = React.useCallback((node: HTMLDivElement | null) => {
			setHeaderPortalElement(node);
		}, []);

		const headerContextValue = React.useMemo(() => ({headerPortalElement}), [headerPortalElement]);

		const headerContent = (
			<>
				{showTabs ? (
					<SegmentedTabs
						tabs={segmentedTabs}
						selectedTab={selectedTab}
						onTabChange={setSelectedTab}
						ariaLabel={t`Expression picker categories`}
					/>
				) : null}
				<div ref={headerPortalCallback} className={styles.headerPortal} />
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
				>
					<div className={styles.container}>
						<div className={styles.contentContainer}>
							<div className={styles.contentInner}>
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
