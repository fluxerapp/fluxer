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

import {autorun} from 'mobx';
import React from 'react';
import * as ExpressionPickerActionCreators from '~/actions/ExpressionPickerActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import type {ExpressionPickerTabType} from '~/components/popouts/ExpressionPickerPopout';
import {ExpressionPickerPopout} from '~/components/popouts/ExpressionPickerPopout';
import {openPopout} from '~/components/uikit/Popout/Popout';
import type {Emoji} from '~/stores/EmojiStore';
import ExpressionPickerStore from '~/stores/ExpressionPickerStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PopoutStore from '~/stores/PopoutStore';

interface UseTextareaExpressionPickerOptions {
	channelId: string;
	onEmojiSelect: (emoji: Emoji, shiftKey?: boolean) => void;
	expressionPickerTriggerRef: React.RefObject<HTMLButtonElement | null>;
	invisibleExpressionPickerTriggerRef: React.RefObject<HTMLDivElement | null>;
	textareaRef: React.RefObject<HTMLElement | null>;
}

export const useTextareaExpressionPicker = ({
	channelId,
	onEmojiSelect,
	expressionPickerTriggerRef,
	invisibleExpressionPickerTriggerRef,
	textareaRef,
}: UseTextareaExpressionPickerOptions) => {
	const [expressionPickerOpen, setExpressionPickerOpen] = React.useState(false);
	const selectedTab = React.useSyncExternalStore(
		(listener) => {
			const dispose = autorun(listener);
			return () => dispose();
		},
		() => ExpressionPickerStore.selectedTab,
	);
	const mobileLayout = MobileLayoutStore;

	const getExpressionPickerPopoutKey = React.useCallback(() => `expression-picker-${channelId}`, [channelId]);

	const closeExpressionPicker = React.useCallback(() => {
		const popoutKey = getExpressionPickerPopoutKey();
		PopoutActionCreators.close(popoutKey);
		ExpressionPickerActionCreators.close();
		setExpressionPickerOpen(false);
	}, [getExpressionPickerPopoutKey]);

	const openExpressionPicker = React.useCallback(
		(tab: ExpressionPickerTabType) => {
			const triggerElement = expressionPickerTriggerRef.current || invisibleExpressionPickerTriggerRef.current;
			if (!triggerElement) return;

			const popoutKey = getExpressionPickerPopoutKey();
			ExpressionPickerActionCreators.open(channelId, tab);

			openPopout(
				triggerElement,
				{
					render: ({onClose}) => (
						<ExpressionPickerPopout channelId={channelId} onEmojiSelect={onEmojiSelect} onClose={onClose} />
					),
					position: 'top-end',
					animationType: 'none',
					offsetCrossAxis: 16,
					onOpen: () => setExpressionPickerOpen(true),
					onClose: closeExpressionPicker,
					onCloseRequest: (event) => {
						if (!event) return true;

						const target = event.target as HTMLElement;
						const tabElement = target.closest('[data-expression-picker-tab]');
						if (tabElement) {
							const clickedTab = tabElement.getAttribute('data-expression-picker-tab');
							if (clickedTab && clickedTab !== selectedTab) {
								return false;
							}
						}
						return true;
					},
					returnFocusRef: textareaRef,
				},
				popoutKey,
			);
		},
		[
			channelId,
			selectedTab,
			onEmojiSelect,
			getExpressionPickerPopoutKey,
			closeExpressionPicker,
			expressionPickerTriggerRef,
			invisibleExpressionPickerTriggerRef,
			textareaRef,
		],
	);

	const handleExpressionPickerTabToggle = React.useCallback(
		(tab: ExpressionPickerTabType) => {
			if (mobileLayout.enabled) {
				ExpressionPickerActionCreators.open(channelId, tab);
				setExpressionPickerOpen(true);
				return;
			}

			const popoutKey = getExpressionPickerPopoutKey();
			const isOpen = PopoutStore.isOpen(popoutKey);
			const isSameTab = ExpressionPickerStore.selectedTab === tab;

			if (isOpen && isSameTab) {
				closeExpressionPicker();
			} else if (!isOpen) {
				openExpressionPicker(tab);
			} else {
				ExpressionPickerActionCreators.setTab(tab);
			}
		},
		[mobileLayout.enabled, channelId, getExpressionPickerPopoutKey, closeExpressionPicker, openExpressionPicker],
	);

	React.useEffect(() => {
		if (mobileLayout.enabled) return;

		const dispose = autorun(() => {
			const {isOpen, channelId: storeChannelId, selectedTab} = ExpressionPickerStore;

			if (storeChannelId !== channelId) return;

			const popoutKey = getExpressionPickerPopoutKey();
			const isPopoutOpen = PopoutStore.isOpen(popoutKey);

			if (isOpen && !isPopoutOpen) {
				openExpressionPicker(selectedTab);
			} else if (!isOpen && isPopoutOpen) {
				closeExpressionPicker();
			}
		});

		return () => dispose();
	}, [channelId, getExpressionPickerPopoutKey, openExpressionPicker, closeExpressionPicker, mobileLayout.enabled]);

	React.useEffect(() => {
		const handleGlobalKeyDown = (event: KeyboardEvent) => {
			if (!event.ctrlKey && !event.metaKey) return;
			if (event.shiftKey || event.altKey) return;

			const key = event.key.toLowerCase();
			let tab: ExpressionPickerTabType | null = null;

			if (key === 'e') tab = 'emojis';
			else if (key === 'g') tab = 'gifs';
			else if (key === 's') tab = 'stickers';
			else if (key === 'm') tab = 'memes';

			if (tab) {
				event.preventDefault();
				event.stopPropagation();
				handleExpressionPickerTabToggle(tab);
			}
		};

		window.addEventListener('keydown', handleGlobalKeyDown, {capture: true});
		return () => window.removeEventListener('keydown', handleGlobalKeyDown, {capture: true});
	}, [handleExpressionPickerTabToggle]);

	return {
		expressionPickerOpen,
		setExpressionPickerOpen,
		handleExpressionPickerTabToggle,
		selectedTab,
	};
};
