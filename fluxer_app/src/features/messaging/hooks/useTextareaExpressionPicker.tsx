// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ExpressionPickerCommands from '@app/features/emoji/commands/ExpressionPickerCommands';
import ExpressionPicker from '@app/features/emoji/state/ExpressionPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import type {ExpressionPickerTabType} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {ExpressionPickerPopout} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {openPopout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Popout from '@app/features/ui/state/Popout';
import {autorun} from 'mobx';
import type React from 'react';
import {useCallback, useEffect, useState, useSyncExternalStore} from 'react';

interface UseTextareaExpressionPickerOptions {
	channelId: string;
	onEmojiSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
	expressionPickerTriggerRef: React.RefObject<HTMLButtonElement | null>;
	invisibleExpressionPickerTriggerRef: React.RefObject<HTMLDivElement | null>;
	textareaRef: React.RefObject<HTMLElement | null>;
	enabled?: boolean;
}

export const useTextareaExpressionPicker = ({
	channelId,
	onEmojiSelect,
	expressionPickerTriggerRef,
	invisibleExpressionPickerTriggerRef,
	textareaRef,
	enabled = true,
}: UseTextareaExpressionPickerOptions) => {
	const [expressionPickerOpen, setExpressionPickerOpen] = useState(false);
	const selectedTab = useSyncExternalStore(
		(listener) => {
			const dispose = autorun(listener);
			return () => dispose();
		},
		() => ExpressionPicker.selectedTab,
	);
	const mobileLayout = MobileLayout;
	const getExpressionPickerPopoutKey = useCallback(() => `expression-picker-${channelId}`, [channelId]);
	const closeExpressionPicker = useCallback(() => {
		const popoutKey = getExpressionPickerPopoutKey();
		PopoutCommands.close(popoutKey);
		ExpressionPickerCommands.close();
		setExpressionPickerOpen(false);
	}, [getExpressionPickerPopoutKey]);
	const openExpressionPicker = useCallback(
		(tab: ExpressionPickerTabType) => {
			if (!enabled) return;
			const triggerElement = expressionPickerTriggerRef.current || invisibleExpressionPickerTriggerRef.current;
			if (!triggerElement) return;
			const popoutKey = getExpressionPickerPopoutKey();
			ExpressionPickerCommands.open(channelId, tab);
			openPopout(
				triggerElement,
				{
					render: ({onClose}) => (
						<ExpressionPickerPopout
							channelId={channelId}
							onEmojiSelect={onEmojiSelect}
							onClose={onClose}
							data-flx="messaging.use-textarea-expression-picker.open-expression-picker.expression-picker-popout"
						/>
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
					disableBackdrop: true,
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
			enabled,
		],
	);
	const handleExpressionPickerTabToggle = useCallback(
		(tab: ExpressionPickerTabType) => {
			if (!enabled) return;
			if (mobileLayout.enabled) {
				ExpressionPickerCommands.open(channelId, tab);
				setExpressionPickerOpen(true);
				return;
			}
			const popoutKey = getExpressionPickerPopoutKey();
			const isOpen = Popout.isOpen(popoutKey);
			const isSameTab = ExpressionPicker.selectedTab === tab;
			if (isOpen && isSameTab) {
				closeExpressionPicker();
			} else if (!isOpen) {
				openExpressionPicker(tab);
			} else {
				ExpressionPickerCommands.setTab(tab);
			}
		},
		[
			mobileLayout.enabled,
			channelId,
			getExpressionPickerPopoutKey,
			closeExpressionPicker,
			openExpressionPicker,
			enabled,
		],
	);
	useEffect(() => {
		if (!enabled) {
			closeExpressionPicker();
			return;
		}
		if (mobileLayout.enabled) return;
		const dispose = autorun(() => {
			const {isOpen, channelId: storeChannelId, selectedTab} = ExpressionPicker;
			if (storeChannelId !== channelId) return;
			const popoutKey = getExpressionPickerPopoutKey();
			const isPopoutOpen = Popout.isOpen(popoutKey);
			if (isOpen && !isPopoutOpen) {
				openExpressionPicker(selectedTab);
			} else if (!isOpen && isPopoutOpen) {
				closeExpressionPicker();
			}
		});
		return () => dispose();
	}, [
		channelId,
		getExpressionPickerPopoutKey,
		openExpressionPicker,
		closeExpressionPicker,
		mobileLayout.enabled,
		enabled,
	]);
	useEffect(() => {
		const unsubscribe = ComponentBus.subscribe('EXPRESSION_PICKER_TAB_TOGGLE', (payload?: unknown) => {
			const data = payload as {channelId?: string; tab?: ExpressionPickerTabType} | undefined;
			if (!enabled) return;
			if (!data || data.channelId !== channelId || !data.tab) return;
			handleExpressionPickerTabToggle(data.tab);
		});
		return () => unsubscribe();
	}, [channelId, handleExpressionPickerTabToggle, enabled]);
	return {
		expressionPickerOpen,
		setExpressionPickerOpen,
		handleExpressionPickerTabToggle,
		selectedTab,
	};
};
