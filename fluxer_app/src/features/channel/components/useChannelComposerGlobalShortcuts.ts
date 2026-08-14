// SPDX-License-Identifier: AGPL-3.0-or-later

import {requestChannelComposerAffordanceDismissal} from '@app/features/channel/components/ChannelComposerDismissal';
import type {Channel} from '@app/features/channel/models/Channel';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {canFocusTextarea, safeFocus} from '@app/features/platform/utils/InputFocusManager';
import {isTextInputKeyEvent} from '@app/features/platform/utils/IsTextInputKeyEvent';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import ContextMenuState from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import type React from 'react';
import {useEffect} from 'react';

interface UseChannelComposerGlobalShortcutsParams {
	channel: Channel;
	handleRef: React.RefObject<ComposerHandle | null>;
	editableRef: React.RefObject<HTMLDivElement | null>;
	textareaInputDisabled: boolean;
	isFocused: boolean;
	handleArrowUpEmpty: () => void;
}

export function useChannelComposerGlobalShortcuts({
	channel,
	handleRef,
	editableRef,
	textareaInputDisabled,
	isFocused,
	handleArrowUpEmpty,
}: UseChannelComposerGlobalShortcutsParams): void {
	useEffect(() => {
		if (textareaInputDisabled) {
			return;
		}
		const handleKeyDown = (event: KeyboardEvent) => {
			const element = editableRef.current;
			if (!canFocusTextarea(element === null ? undefined : element)) return;
			if (isFocused) return;
			if (QuickSwitcher.getIsOpen()) return;
			if (ContextMenuState.contextMenu) return;
			if (KeyboardMode.keyboardModeEnabled && MessageFocus.focusedMessageId) return;
			if (!isTextInputKeyEvent(event)) return;
			if (!element) return;
			if (event.key === 'Dead') {
				safeFocus(element, true);
				return;
			}
			event.preventDefault();
			safeFocus(element, true);
			const handle = handleRef.current;
			if (handle === null) return;
			handle.focus();
			handle.insertTextAtCursor(event.key);
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [textareaInputDisabled, isFocused]);
	useEffect(() => {
		if (textareaInputDisabled) {
			return;
		}
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key !== 'ArrowUp' || event.shiftKey || event.altKey || event.ctrlKey || event.metaKey) return;
			const element = editableRef.current;
			if (!element) return;
			if (!canFocusTextarea(element)) return;
			if (isFocused) return;
			if (QuickSwitcher.getIsOpen()) return;
			if (ContextMenuState.contextMenu) return;
			if (KeyboardMode.keyboardModeEnabled) return;
			const handle = handleRef.current;
			let displayValue = '';
			if (handle !== null) displayValue = handle.getDisplayValue();
			if (displayValue.trim().length !== 0) return;
			event.preventDefault();
			safeFocus(element, true);
			if (handle !== null) handle.focus();
			handleArrowUpEmpty();
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [textareaInputDisabled, isFocused, handleArrowUpEmpty]);
	useEffect(() => {
		if (textareaInputDisabled) {
			return;
		}
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.defaultPrevented || event.isComposing) return;
			if (event.key !== 'Escape' || event.shiftKey) return;
			if (requestChannelComposerAffordanceDismissal(channel.id)) {
				event.preventDefault();
				event.stopPropagation();
				return;
			}
			ComponentDispatch.dispatch('ESCAPE_PRESSED', {channelId: channel.id});
			event.preventDefault();
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [channel.id, textareaInputDisabled]);
}
