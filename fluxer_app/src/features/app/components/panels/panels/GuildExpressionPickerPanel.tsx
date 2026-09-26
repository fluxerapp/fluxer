// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmojiPicker} from '@app/features/channel/components/EmojiPicker';
import {GifPicker} from '@app/features/channel/components/pickers/gif/GifPicker';
import {MemesPicker} from '@app/features/channel/components/pickers/memes/MemesPicker';
import {StickersPicker} from '@app/features/channel/components/StickersPicker';
import * as ExpressionPickerCommands from '@app/features/emoji/commands/ExpressionPickerCommands';
import ExpressionPicker from '@app/features/emoji/state/ExpressionPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	ExpressionPickerHeaderContext,
	type ExpressionPickerTabType,
} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {getExpressionPickerHeight} from '@app/features/expressions/utils/ExpressionPickerUtils';
import {
	EMOJIS_DESCRIPTOR,
	EXPRESSION_PICKER_CATEGORIES_DESCRIPTOR,
	GIFS_DESCRIPTOR,
	MEDIA_DESCRIPTOR,
	STICKERS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

interface GuildExpressionPickerPanelProps {
	guildId: string;
	channelId?: string;
	onEmojiSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
	onClose?: () => void;
	visibleTabs?: Array<ExpressionPickerTabType>;
	selectedTab?: ExpressionPickerTabType;
	onTabChange?: (tab: ExpressionPickerTabType) => void;
}

export const GuildExpressionPickerPanel = observer(
	({
		guildId,
		channelId,
		onEmojiSelect,
		onClose,
		visibleTabs = ['gifs', 'memes', 'stickers', 'emojis'],
		selectedTab: controlledSelectedTab,
		onTabChange,
	}: GuildExpressionPickerPanelProps) => {
		const {i18n} = useLingui();
		const containerRef = useRef<HTMLDivElement>(null);
		const [_openTime, setOpenTime] = useState<number>(Date.now());
		const categories = useMemo(() => {
			const allCategories = [
				{
					type: 'gifs' as const,
					label: i18n._(GIFS_DESCRIPTOR),
					renderComponent: ({onClose: close}: {onClose?: () => void}) => (
						<GifPicker onClose={close} data-flx="app.guild-expression-picker-panel.render-component.gif-picker" />
					),
				},
				{
					type: 'memes' as const,
					label: i18n._(MEDIA_DESCRIPTOR),
					renderComponent: ({onClose: close}: {onClose?: () => void}) => (
						<MemesPicker onClose={close} data-flx="app.guild-expression-picker-panel.render-component.memes-picker" />
					),
				},
				{
					type: 'stickers' as const,
					label: i18n._(STICKERS_DESCRIPTOR),
					renderComponent: ({channelId: chanId, onClose: close}: {channelId?: string; onClose?: () => void}) => {
						const handleStickerSelect = (_sticker: unknown, shiftKey?: boolean) => {
							if (chanId) {
								if (close && !shiftKey) {
									close();
								}
							}
						};
						return (
							<StickersPicker
								channelId={chanId}
								handleSelect={handleStickerSelect}
								data-flx="app.guild-expression-picker-panel.render-component.stickers-picker"
							/>
						);
					},
				},
				{
					type: 'emojis' as const,
					label: i18n._(EMOJIS_DESCRIPTOR),
					renderComponent: ({
						channelId: chanId,
						onSelect,
					}: {
						channelId?: string;
						onSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
					}) => (
						<EmojiPicker
							channelId={chanId}
							handleSelect={onSelect}
							data-flx="app.guild-expression-picker-panel.render-component.emoji-picker"
						/>
					),
				},
			];
			return allCategories.filter((category) => visibleTabs.includes(category.type));
		}, [i18n.locale, visibleTabs]);
		const [internalSelectedTab, setInternalSelectedTab] = useState<ExpressionPickerTabType>(
			() => categories[0]?.type || 'emojis',
		);
		const storeSelectedTab = ExpressionPicker.selectedTab;
		const selectedTab = storeSelectedTab ?? controlledSelectedTab ?? internalSelectedTab;
		const setSelectedTab = useCallback(
			(tab: ExpressionPickerTabType) => {
				if (tab === selectedTab) {
					return;
				}
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
			[channelId, onTabChange, selectedTab],
		);
		const selectedCategory = categories.find((category) => category.type === selectedTab) || categories[0];
		const tabRefs = useRef<Map<ExpressionPickerTabType, HTMLButtonElement>>(new Map());
		useEffect(() => {
			setOpenTime(Date.now());
		}, []);
		useEffect(() => {
			if (containerRef.current) {
				const firstInput = containerRef.current.querySelector('input[type="text"]') as HTMLInputElement | null;
				if (firstInput) {
					firstInput.focus();
				}
			}
		}, []);
		const handleEmojiSelect = useCallback(
			(emoji: FlatEmoji, shiftKey?: boolean) => {
				onEmojiSelect(emoji, shiftKey);
				if (onClose && !shiftKey) {
					onClose();
				}
			},
			[channelId, onClose, onEmojiSelect],
		);
		const handleClose = useCallback(() => {
			onClose?.();
		}, [onClose]);
		const handleTabKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLButtonElement>, tab: ExpressionPickerTabType) => {
				const currentIndex = categories.findIndex((category) => category.type === tab);
				if (currentIndex === -1) return;
				const direction = getTabNavigationDirection(event.key, 'horizontal');
				if (!direction) return;
				const nextIndex = getNextTabIndex(currentIndex, categories.length, direction);
				if (nextIndex == null) return;
				event.preventDefault();
				event.stopPropagation();
				const nextTab = categories[nextIndex]?.type;
				if (!nextTab) return;
				setSelectedTab(nextTab);
				requestAnimationFrame(() => tabRefs.current.get(nextTab)?.focus());
			},
			[categories, setSelectedTab],
		);
		const showTabs = categories.length > 1;
		const [headerPortalElement, setHeaderPortalElement] = useState<HTMLDivElement | null>(null);
		const headerPortalCallback = useCallback((node: HTMLDivElement | null) => {
			setHeaderPortalElement(node);
		}, []);
		const headerContextValue = useMemo(() => ({headerPortalElement}), [headerPortalElement]);
		const pickerHeight = useMemo(() => {
			return getExpressionPickerHeight(50, 44, 32);
		}, []);
		return (
			<ExpressionPickerHeaderContext.Provider value={headerContextValue}>
				<div ref={containerRef} style={{height: pickerHeight}} data-flx="app.guild-expression-picker-panel.div">
					<div data-flx="app.guild-expression-picker-panel.div--2">
						{showTabs && (
							<nav data-flx="app.guild-expression-picker-panel.nav">
								<div
									role="tablist"
									aria-label={i18n._(EXPRESSION_PICKER_CATEGORIES_DESCRIPTOR)}
									data-flx="app.guild-expression-picker-panel.tablist"
								>
									{categories.map((category) => {
										const isSelected = selectedCategory.type === category.type;
										return (
											<button
												key={category.type}
												ref={(element) => {
													if (element) {
														tabRefs.current.set(category.type, element);
													} else {
														tabRefs.current.delete(category.type);
													}
												}}
												id={`${guildId}-${category.type}`}
												role="tab"
												type="button"
												aria-selected={isSelected}
												tabIndex={isSelected ? 0 : -1}
												onClick={() => setSelectedTab(category.type)}
												onKeyDown={(event) => handleTabKeyDown(event, category.type)}
												data-flx="app.guild-expression-picker-panel.tab.button"
											>
												{category.label}
											</button>
										);
									})}
								</div>
							</nav>
						)}
						<div ref={headerPortalCallback} data-flx="app.guild-expression-picker-panel.div--3" />
					</div>
					<div data-flx="app.guild-expression-picker-panel.div--4">
						{selectedCategory.renderComponent({channelId, onSelect: handleEmojiSelect, onClose: handleClose})}
					</div>
				</div>
			</ExpressionPickerHeaderContext.Provider>
		);
	},
);
