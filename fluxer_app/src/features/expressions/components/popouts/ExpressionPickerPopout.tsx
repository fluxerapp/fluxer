// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {EmojiPicker} from '@app/features/channel/components/EmojiPicker';
import {GifPicker} from '@app/features/channel/components/pickers/gif/GifPicker';
import {MemesPicker} from '@app/features/channel/components/pickers/memes/MemesPicker';
import {StickersPicker} from '@app/features/channel/components/StickersPicker';
import * as ExpressionPickerCommands from '@app/features/emoji/commands/ExpressionPickerCommands';
import ExpressionPicker from '@app/features/emoji/state/ExpressionPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import styles from '@app/features/expressions/components/popouts/ExpressionPickerPopout.module.css';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import * as StickerSendUtils from '@app/features/expressions/utils/StickerSendUtils';
import {
	EMOJIS_DESCRIPTOR,
	EXPRESSION_PICKER_CATEGORIES_DESCRIPTOR,
	GIFS_DESCRIPTOR,
	MEDIA_DESCRIPTOR,
	STICKERS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {ResizeEdge} from '@app/features/ui/floating_pane/FloatingPaneMath';
import {useResizablePane} from '@app/features/ui/hooks/useResizablePane';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useContext, useEffect, useMemo, useRef, useState} from 'react';
import ReactDOM from 'react-dom';

const RESIZE_EXPRESSION_PICKER_TOP_LEFT_DESCRIPTOR = msg({
	message: 'Resize expression picker from top left',
	comment: 'Accessible label for the top-left resize handle on the expression picker popout.',
});
const RESIZE_EXPRESSION_PICKER_TOP_RIGHT_DESCRIPTOR = msg({
	message: 'Resize expression picker from top right',
	comment: 'Accessible label for the top-right resize handle on the expression picker popout.',
});
const RESIZE_EXPRESSION_PICKER_BOTTOM_LEFT_DESCRIPTOR = msg({
	message: 'Resize expression picker from bottom left',
	comment: 'Accessible label for the bottom-left resize handle on the expression picker popout.',
});
const RESIZE_EXPRESSION_PICKER_BOTTOM_RIGHT_DESCRIPTOR = msg({
	message: 'Resize expression picker from bottom right',
	comment: 'Accessible label for the bottom-right resize handle on the expression picker popout.',
});

const EXPRESSION_PICKER_DEFAULT_SIZE = {width: 578, height: 690};
const EXPRESSION_PICKER_MIN_SIZE = {width: 360, height: 360};
const EXPRESSION_PICKER_VIEWPORT_PADDING = 16;
const EXPRESSION_PICKER_RESIZING_CLASS = 'expression-picker-resizing';
const EXPRESSION_PICKER_RESIZE_CURSOR_PROPERTY = '--expression-picker-resize-cursor';

interface ResizeHandleSpec {
	edge: ResizeEdge;
	className: string;
	label: MessageDescriptor;
}

interface ExpressionPickerHeaderContextType {
	headerPortalElement: HTMLDivElement | null;
}

export const ExpressionPickerHeaderContext = React.createContext<ExpressionPickerHeaderContextType | null>(null);
export const useExpressionPickerHeaderPortal = () => {
	return useContext(ExpressionPickerHeaderContext);
};
export const ExpressionPickerHeaderPortal = ({children}: {children: React.ReactNode}) => {
	const context = useExpressionPickerHeaderPortal();
	if (context == null || context.headerPortalElement == null) return null;
	return ReactDOM.createPortal(children, context.headerPortalElement);
};

export type ExpressionPickerTabType = 'gifs' | 'memes' | 'stickers' | 'emojis';

interface ExpressionPickerCategory {
	type: ExpressionPickerTabType;
	label: string;
	renderComponent: (props: {
		channelId?: string;
		onSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
		onClose?: () => void;
	}) => React.ReactNode;
}

const createAllCategories = (i18n: I18n): Array<ExpressionPickerCategory> => [
	{
		type: 'gifs' as const,
		label: i18n._(GIFS_DESCRIPTOR),
		renderComponent: ({onClose}) => (
			<GifPicker onClose={onClose} data-flx="expressions.expression-picker-popout.render-component.gif-picker" />
		),
	},
	{
		type: 'memes' as const,
		label: i18n._(MEDIA_DESCRIPTOR),
		renderComponent: ({onClose}) => (
			<MemesPicker onClose={onClose} data-flx="expressions.expression-picker-popout.render-component.memes-picker" />
		),
	},
	{
		type: 'stickers' as const,
		label: i18n._(STICKERS_DESCRIPTOR),
		renderComponent: ({channelId, onClose}) => {
			const handleStickerSelect = (sticker: GuildSticker, shiftKey?: boolean) => {
				if (channelId) {
					if (StickerSendUtils.shouldSetPendingSticker(channelId)) {
						StickerSendUtils.setPendingSticker(channelId, sticker);
					} else {
						ComponentDispatch.dispatch('STICKER_SELECT', {sticker});
					}
					if (onClose && !shiftKey) {
						onClose();
					}
				}
			};
			return (
				<StickersPicker
					channelId={channelId}
					handleSelect={handleStickerSelect}
					data-flx="expressions.expression-picker-popout.render-component.stickers-picker"
				/>
			);
		},
	},
	{
		type: 'emojis' as const,
		label: i18n._(EMOJIS_DESCRIPTOR),
		renderComponent: ({channelId, onSelect}) => (
			<EmojiPicker
				channelId={channelId}
				handleSelect={onSelect}
				data-flx="expressions.expression-picker-popout.render-component.emoji-picker"
			/>
		),
	},
];

interface ExpressionPickerPopoutProps {
	channelId?: string;
	onEmojiSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
	onClose?: () => void;
	visibleTabs?: Array<ExpressionPickerTabType>;
	selectedTab?: ExpressionPickerTabType;
	onTabChange?: (tab: ExpressionPickerTabType) => void;
}

const RESIZE_HANDLES: ReadonlyArray<ResizeHandleSpec> = [
	{
		edge: 'top-left',
		className: styles.resizeCornerTopLeft,
		label: RESIZE_EXPRESSION_PICKER_TOP_LEFT_DESCRIPTOR,
	},
	{
		edge: 'top-right',
		className: styles.resizeCornerTopRight,
		label: RESIZE_EXPRESSION_PICKER_TOP_RIGHT_DESCRIPTOR,
	},
	{
		edge: 'bottom-left',
		className: styles.resizeCornerBottomLeft,
		label: RESIZE_EXPRESSION_PICKER_BOTTOM_LEFT_DESCRIPTOR,
	},
	{
		edge: 'bottom-right',
		className: styles.resizeCornerBottomRight,
		label: RESIZE_EXPRESSION_PICKER_BOTTOM_RIGHT_DESCRIPTOR,
	},
];

export const ExpressionPickerPopout = observer(
	({
		channelId,
		onEmojiSelect,
		onClose,
		visibleTabs = ['gifs', 'memes', 'stickers', 'emojis'],
		selectedTab: controlledSelectedTab,
		onTabChange,
	}: ExpressionPickerPopoutProps) => {
		const {i18n} = useLingui();
		const categories = useMemo(() => {
			const all = createAllCategories(i18n);
			return all.filter(
				(category) => visibleTabs.includes(category.type) && (category.type !== 'gifs' || RuntimeConfig.gifEnabled),
			);
		}, [i18n.locale, visibleTabs, RuntimeConfig.gifEnabled]);
		const [internalSelectedTab, setInternalSelectedTab] = useState<ExpressionPickerTabType>(() => {
			const firstCategory = categories[0];
			return firstCategory == null ? 'emojis' : firstCategory.type;
		});
		const storeSelectedTab = ExpressionPicker.selectedTab;
		let selectedTab = internalSelectedTab;
		if (controlledSelectedTab != null) selectedTab = controlledSelectedTab;
		if (storeSelectedTab != null) selectedTab = storeSelectedTab;
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
					ExpressionPickerCommands.toggle(pickerChannelId, tab);
				} else {
					setInternalSelectedTab(tab);
				}
			},
			[onTabChange, selectedTab],
		);
		const selectedCategory = categories.find((category) => category.type === selectedTab) || categories[0];
		const containerRef = useRef<HTMLDivElement>(null);
		const tabRefs = useRef<Map<ExpressionPickerTabType, HTMLButtonElement>>(new Map());
		const {size, getHandleProps} = useResizablePane(containerRef, {
			storageKey: 'fluxer:ui:expression-picker-size',
			defaultSize: EXPRESSION_PICKER_DEFAULT_SIZE,
			minSize: EXPRESSION_PICKER_MIN_SIZE,
			viewportPadding: EXPRESSION_PICKER_VIEWPORT_PADDING,
			resizingClassName: EXPRESSION_PICKER_RESIZING_CLASS,
			cursorProperty: EXPRESSION_PICKER_RESIZE_CURSOR_PROPERTY,
		});
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
			[onEmojiSelect, onClose],
		);
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
				const nextCategory = categories[nextIndex];
				if (nextCategory == null) return;
				const nextTab = nextCategory.type;
				setSelectedTab(nextTab);
				const ownerWindow = event.currentTarget.ownerDocument.defaultView;
				if (ownerWindow == null) return;
				ownerWindow.requestAnimationFrame(() => {
					const nextTabElement = tabRefs.current.get(nextTab);
					if (nextTabElement != null) nextTabElement.focus();
				});
			},
			[categories, setSelectedTab],
		);
		const showTabs = categories.length > 1;
		const [headerPortalElement, setHeaderPortalElement] = useState<HTMLDivElement | null>(null);
		const headerPortalCallback = useCallback((node: HTMLDivElement | null) => {
			setHeaderPortalElement(node);
		}, []);
		const headerContextValue = useMemo(() => ({headerPortalElement}), [headerPortalElement]);
		return (
			<ExpressionPickerHeaderContext.Provider value={headerContextValue}>
				<div
					ref={containerRef}
					className={clsx(styles.container, showTabs ? styles.containerWithTabs : styles.containerNoTabs)}
					style={{width: remFromPx(size.width), height: remFromPx(size.height)}}
					data-flx="expressions.expression-picker-popout.container"
				>
					<div className={styles.header} data-flx="expressions.expression-picker-popout.header">
						{showTabs && (
							<nav className={styles.nav} data-flx="expressions.expression-picker-popout.nav">
								<div
									className={styles.tabList}
									role="tablist"
									aria-label={i18n._(EXPRESSION_PICKER_CATEGORIES_DESCRIPTOR)}
									data-flx="expressions.expression-picker-popout.tab-list"
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
												id={category.type}
												role="tab"
												type="button"
												aria-selected={isSelected}
												tabIndex={isSelected ? 0 : -1}
												className={clsx(styles.tab, isSelected ? styles.tabActive : styles.tabInactive)}
												onClick={() => setSelectedTab(category.type)}
												onKeyDown={(event) => handleTabKeyDown(event, category.type)}
												data-flx="expressions.expression-picker-popout.tab.button"
											>
												{category.label}
											</button>
										);
									})}
								</div>
							</nav>
						)}
						<div
							ref={headerPortalCallback}
							className={styles.headerPortal}
							data-flx="expressions.expression-picker-popout.header-portal"
						/>
					</div>
					<div className={styles.content} data-flx="expressions.expression-picker-popout.content">
						{selectedCategory.renderComponent({channelId, onSelect: handleEmojiSelect, onClose})}
					</div>
					{RESIZE_HANDLES.map(({edge, className, label}) => (
						<button
							key={edge}
							type="button"
							aria-label={i18n._(label)}
							aria-keyshortcuts="ArrowLeft ArrowRight ArrowUp ArrowDown Home End Enter Backspace Delete"
							className={clsx(styles.resizeCorner, className)}
							{...getHandleProps(edge)}
						/>
					))}
				</div>
			</ExpressionPickerHeaderContext.Provider>
		);
	},
);
