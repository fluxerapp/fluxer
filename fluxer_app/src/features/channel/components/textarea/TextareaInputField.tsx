// SPDX-License-Identifier: AGPL-3.0-or-later

import {isTouchDevice} from '@app/features/app/hooks/usePressable';
import {
	type AutocompleteOption,
	getAutocompleteOptionId,
	isChannel,
} from '@app/features/channel/components/Autocomplete';
import styles from '@app/features/channel/components/textarea/TextareaInput.module.css';
import * as HighlightCommands from '@app/features/messaging/commands/HighlightCommands';
import {useTextareaAutofocus} from '@app/features/messaging/hooks/useTextareaAutofocus';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {
	applyCtrlArrowLeftAcrossLineStart,
	scheduleShiftArrowUpSelectionFallback,
	shouldHandleCtrlArrowLeftAcrossLineStart,
} from '@app/features/messaging/utils/TextareaKeyboardNavigationUtils';
import type {TextareaTextChangeHint} from '@app/features/messaging/utils/TextareaSegmentManager';
import {Platform} from '@app/features/platform/types/Platform';
import {TextareaAutosize} from '@app/features/platform/utils/AutoResizingTextarea';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {clsx} from 'clsx';
import React from 'react';

const isMobileOrTouchDevice = isTouchDevice || Platform.isMobileBrowser;

interface TextareaInputSelectionSnapshot extends TextareaTextChangeHint {
	value: string;
}

interface TextareaInputFieldProps {
	channelId: string;
	disabled: boolean;
	isMobile: boolean;
	value: string;
	placeholder: string;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	scrollerRef?: React.RefObject<ScrollerHandle | null>;
	shouldStickToBottomRef?: React.RefObject<boolean>;
	isFocused?: boolean;
	isAutocompleteAttached: boolean;
	autocompleteListId?: string;
	autocompleteOptions: Array<AutocompleteOption>;
	selectedIndex: number;
	onFocus: () => void;
	onBlur: () => void;
	onChange: (value: string, inputType?: string, hint?: TextareaTextChangeHint) => void;
	onHeightChange: (height: number) => void;
	onCursorMove: () => void;
	onArrowUp: (event: React.KeyboardEvent) => void;
	onEnter: () => void;
	onAutocompleteSelect: (option: AutocompleteOption) => void;
	setSelectedIndex: React.Dispatch<React.SetStateAction<number>>;
	className?: string;
	onKeyDown?: (event: React.KeyboardEvent<HTMLTextAreaElement>) => void;
	onContextMenu?: (event: React.MouseEvent<HTMLTextAreaElement>) => void;
}

export const TextareaInputField = React.forwardRef<HTMLTextAreaElement, TextareaInputFieldProps>(
	(
		{
			disabled,
			isMobile,
			value,
			placeholder,
			textareaRef,
			isAutocompleteAttached,
			autocompleteListId,
			autocompleteOptions,
			selectedIndex,
			onFocus,
			onBlur,
			onChange,
			onHeightChange,
			onCursorMove,
			onArrowUp,
			onEnter,
			onAutocompleteSelect,
			setSelectedIndex,
			className,
			onKeyDown,
			onContextMenu,
		},
		_ref,
	) => {
		useTextareaAutofocus(textareaRef, isMobile, !disabled);
		const beforeInputSelectionRef = React.useRef<TextareaInputSelectionSnapshot | null>(null);
		const keyDownSelectionRef = React.useRef<TextareaInputSelectionSnapshot | null>(null);
		const activeAutocompleteOptionId =
			isAutocompleteAttached && autocompleteListId && autocompleteOptions[selectedIndex]
				? getAutocompleteOptionId(autocompleteListId, selectedIndex)
				: undefined;
		const getSelectionSnapshot = (
			textarea: HTMLTextAreaElement,
			inputType?: string,
		): TextareaInputSelectionSnapshot => ({
			value: textarea.value,
			selectionStart: textarea.selectionStart ?? textarea.value.length,
			selectionEnd: textarea.selectionEnd ?? textarea.value.length,
			inputType,
		});
		const getTextChangeHint = (inputType?: string): TextareaTextChangeHint | undefined => {
			const beforeInputSelection = beforeInputSelectionRef.current;
			beforeInputSelectionRef.current = null;
			if (beforeInputSelection?.value === value) {
				return {
					selectionStart: beforeInputSelection.selectionStart,
					selectionEnd: beforeInputSelection.selectionEnd,
					inputType: beforeInputSelection.inputType ?? inputType,
				};
			}
			const keyDownSelection = keyDownSelectionRef.current;
			keyDownSelectionRef.current = null;
			if (keyDownSelection?.value === value) {
				return {
					selectionStart: keyDownSelection.selectionStart,
					selectionEnd: keyDownSelection.selectionEnd,
					inputType,
				};
			}
			return undefined;
		};
		const shouldRememberKeyDownSelection = (event: React.KeyboardEvent<HTMLTextAreaElement>) => {
			if (event.key === 'Backspace' || event.key === 'Delete' || event.key === 'Enter') {
				return true;
			}
			return event.key.length === 1 && !event.altKey && !event.ctrlKey && !event.metaKey;
		};
		const handleKeyDown = (event: React.KeyboardEvent<HTMLTextAreaElement>) => {
			if (shouldRememberKeyDownSelection(event)) {
				keyDownSelectionRef.current = getSelectionSnapshot(event.currentTarget);
			}
			onCursorMove();
			if (isIMEComposing(event)) {
				if (onKeyDown) {
					onKeyDown(event);
				}
				return;
			}
			if (shouldHandleCtrlArrowLeftAcrossLineStart(event, event.currentTarget)) {
				const moved = applyCtrlArrowLeftAcrossLineStart(event, event.currentTarget);
				if (moved !== null) {
					event.preventDefault();
					onCursorMove();
					return;
				}
			}
			const hasShortcutModifier = event.altKey || event.ctrlKey || event.metaKey || event.shiftKey;
			if (isAutocompleteAttached) {
				if (!hasShortcutModifier && (event.key === 'ArrowUp' || event.key === 'ArrowDown')) {
					event.preventDefault();
					setSelectedIndex((prevIndex) => {
						const newIndex = event.key === 'ArrowUp' ? prevIndex - 1 : prevIndex + 1;
						const clampedIndex = (newIndex + autocompleteOptions.length) % autocompleteOptions.length;
						if (isChannel(autocompleteOptions[clampedIndex])) {
							HighlightCommands.highlightChannel(autocompleteOptions[clampedIndex].channel.id);
						} else {
							HighlightCommands.clearChannelHighlight();
						}
						return clampedIndex;
					});
				} else if (!hasShortcutModifier && event.key === 'Tab') {
					event.preventDefault();
					const selectedOption = autocompleteOptions[selectedIndex];
					if (selectedOption) {
						onAutocompleteSelect(selectedOption);
					}
				} else if (!hasShortcutModifier && event.key === 'Enter') {
					event.preventDefault();
					const selectedOption = autocompleteOptions[selectedIndex];
					if (selectedOption) {
						onAutocompleteSelect(selectedOption);
					}
				}
			} else if (event.key === 'Enter' && !event.shiftKey && !(isMobile && isMobileOrTouchDevice)) {
				event.preventDefault();
				onEnter();
			} else if (event.key === 'ArrowUp' && !hasShortcutModifier) {
				onArrowUp(event);
			}
			if (onKeyDown) {
				onKeyDown(event);
			}
			scheduleShiftArrowUpSelectionFallback(event, event.currentTarget, onCursorMove);
		};
		return (
			<TextareaAutosize
				data-channel-textarea
				spellCheck={true}
				disabled={disabled}
				className={clsx(styles.textarea, disabled && 'pointer-events-none', className)}
				onBlur={onBlur}
				onBeforeInput={(event) => {
					const nativeEvent = event.nativeEvent as InputEvent;
					beforeInputSelectionRef.current = getSelectionSnapshot(
						event.currentTarget,
						typeof nativeEvent.inputType === 'string' ? nativeEvent.inputType : undefined,
					);
				}}
				onChange={(event) => {
					const nativeEvent = event.nativeEvent as InputEvent;
					const inputType = typeof nativeEvent.inputType === 'string' ? nativeEvent.inputType : undefined;
					onChange(event.currentTarget.value, inputType, getTextChangeHint(inputType));
				}}
				onFocus={onFocus}
				onHeightChange={(h) => onHeightChange(h)}
				onKeyDown={handleKeyDown}
				onContextMenu={onContextMenu}
				placeholder={placeholder}
				aria-label={placeholder}
				aria-autocomplete="list"
				aria-controls={isAutocompleteAttached ? autocompleteListId : undefined}
				aria-activedescendant={activeAutocompleteOptionId}
				ref={textareaRef}
				value={value}
				data-flx="channel.textarea.textarea-input-field.textarea.change"
			/>
		);
	},
);

TextareaInputField.displayName = 'TextareaInputField';
