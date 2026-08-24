// SPDX-License-Identifier: AGPL-3.0-or-later

import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import {insertTextAtCursor} from '@app/features/messaging/utils/TextInputEditUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {canFocusTextarea, safeFocus} from '@app/features/platform/utils/InputFocusManager';
import {isTextInputKeyEvent} from '@app/features/platform/utils/IsTextInputKeyEvent';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import ContextMenu from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {useCallback, useEffect} from 'react';

interface UseTextareaKeyboardOptions {
	channelId: string;
	isFocused: boolean;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	handleTextChange: (newValue: string, previousValue: string) => void;
	previousValueRef: React.MutableRefObject<string>;
	clearSegments: () => void;
	replyingMessage: {
		messageId: string;
		mentioning: boolean;
	} | null;
	editingMessage: Message | null;
	getLastEditableMessage: () => Message | null;
	enabled: boolean;
}

type ArrowUpEditShortcutEvent = Pick<
	React.KeyboardEvent | KeyboardEvent,
	'key' | 'altKey' | 'ctrlKey' | 'metaKey' | 'shiftKey' | 'defaultPrevented'
>;

export const shouldStartLastMessageEditFromArrowUp = (event: ArrowUpEditShortcutEvent, value: string): boolean => {
	if (event.defaultPrevented) return false;
	if (event.key !== 'ArrowUp') return false;
	if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) return false;
	return value.length === 0;
};
export const useTextareaKeyboard = ({
	channelId,
	isFocused,
	textareaRef,
	value,
	setValue,
	handleTextChange,
	previousValueRef,
	clearSegments,
	replyingMessage,
	editingMessage,
	getLastEditableMessage,
	enabled,
}: UseTextareaKeyboardOptions) => {
	const mobileLayout = MobileLayout;
	const editingMessageId = MessageEdit.getEditingMessageId(channelId);
	useEffect(() => {
		if (!enabled) {
			return;
		}
		const handleKeyDown = (event: KeyboardEvent) => {
			const textarea = textareaRef.current;
			if (!canFocusTextarea(textarea || undefined)) {
				return;
			}
			if (isFocused) {
				return;
			}
			if (QuickSwitcher.getIsOpen()) {
				return;
			}
			if (ContextMenu.contextMenu) {
				return;
			}
			if (KeyboardMode.keyboardModeEnabled && MessageFocus.focusedMessageId) {
				return;
			}
			if (!isTextInputKeyEvent(event)) {
				return;
			}
			if (!textarea) {
				return;
			}
			if (event.key === 'Dead') {
				safeFocus(textarea, true);
				return;
			}
			event.preventDefault();
			safeFocus(textarea, true);
			const inserted = insertTextAtCursor(textarea, event.key);
			if (!inserted) {
				setValue((prev) => {
					const newValue = prev + event.key;
					handleTextChange(newValue, previousValueRef.current ?? '');
					return newValue;
				});
			}
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [
		editingMessageId,
		isFocused,
		mobileLayout.enabled,
		handleTextChange,
		previousValueRef,
		textareaRef,
		setValue,
		enabled,
	]);
	useEffect(() => {
		if (!enabled) {
			return;
		}
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				const isEditingInline = MessageEdit.getEditingMessageId(channelId) != null;
				if (isEditingInline) {
					event.preventDefault();
					event.stopPropagation();
					MessageCommands.stopEdit(channelId);
					return;
				}
				if (editingMessage && mobileLayout.enabled) {
					event.preventDefault();
					MessageCommands.stopEditMobile(channelId);
					setValue('');
					clearSegments();
				} else if (replyingMessage) {
					event.preventDefault();
					MessageCommands.stopReply(channelId);
				} else {
					event.preventDefault();
					ComponentBus.dispatch('ESCAPE_PRESSED');
				}
			}
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [channelId, replyingMessage, editingMessage, mobileLayout.enabled, clearSegments, setValue, enabled]);
	const handleArrowUp = useCallback(
		(event: React.KeyboardEvent) => {
			if (!shouldStartLastMessageEditFromArrowUp(event, value)) {
				return;
			}
			if (KeyboardMode.keyboardModeEnabled) {
				event.preventDefault();
				ComponentBus.dispatch('FOCUS_BOTTOMMOST_MESSAGE', {channelId});
				return;
			}
			const message = getLastEditableMessage();
			if (!message) {
				return;
			}
			event.preventDefault();
			MessageCommands.startEdit(channelId, message.id, message.content);
		},
		[channelId, value, getLastEditableMessage],
	);
	return {handleArrowUp};
};
