// SPDX-License-Identifier: AGPL-3.0-or-later

import Keybind, {
	type CustomKeybindEntry,
	type KeybindCommand,
	type KeyCombo,
} from '@app/features/input/state/InputKeybind';
import {replaceTextRange, setTextSelectionSoon} from '@app/features/messaging/utils/TextInputEditUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import type React from 'react';
import {type KeyboardEvent, useCallback, useEffect} from 'react';

interface ShortcutKeyEvent {
	key: string;
	ctrlKey: boolean;
	metaKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
}

interface FormattingShortcut {
	combo: Partial<KeyCombo>;
	wrapper: string;
}

interface MarkdownKeybindScopeOptions {
	preserveEditableFocusActions?: boolean;
}

export const MARKDOWN_FORMATTING_SHORTCUTS: ReadonlyArray<FormattingShortcut> = [
	{combo: {key: 'b', ctrlOrMeta: true}, wrapper: '**'},
	{combo: {key: 'i', ctrlOrMeta: true}, wrapper: '*'},
	{combo: {key: 'u', ctrlOrMeta: true}, wrapper: '__'},
	{combo: {key: 's', ctrlOrMeta: true, shift: true}, wrapper: '~~'},
];
const normalizeKeyName = (key?: string, code?: string): string => {
	const candidate = key?.length ? key : code;
	return candidate ? candidate.toLowerCase() : '';
};
const modifiersMatch = (
	source: {
		ctrlOrMeta: boolean;
		ctrl: boolean;
		meta: boolean;
		alt: boolean;
		shift: boolean;
	},
	target: Partial<KeyCombo>,
): boolean => {
	if (target.ctrlOrMeta !== undefined && source.ctrlOrMeta !== target.ctrlOrMeta) {
		return false;
	}
	if (target.ctrl !== undefined) {
		if (source.ctrl !== target.ctrl) return false;
	} else if (target.ctrlOrMeta === undefined && source.ctrl) {
		return false;
	}
	if (target.meta !== undefined) {
		if (source.meta !== target.meta) return false;
	} else if (target.ctrlOrMeta === undefined && source.meta) {
		return false;
	}
	if (target.alt !== undefined) {
		if (source.alt !== target.alt) return false;
	} else if (source.alt) {
		return false;
	}
	if (target.shift !== undefined) {
		if (source.shift !== target.shift) return false;
	} else if (source.shift) {
		return false;
	}
	return true;
};
const doesStoredComboMatchShortcut = (combo: KeyCombo, target: Partial<KeyCombo>): boolean => {
	const comboKey = normalizeKeyName(combo.key, combo.code);
	const targetKey = normalizeKeyName(target.key, target.code);
	if (targetKey && targetKey !== comboKey) {
		return false;
	}
	return modifiersMatch(
		{
			ctrlOrMeta: Boolean(combo.ctrlOrMeta || combo.ctrl || combo.meta),
			ctrl: Boolean(combo.ctrl),
			meta: Boolean(combo.meta),
			alt: Boolean(combo.alt),
			shift: Boolean(combo.shift),
		},
		target,
	);
};
export const doesEventMatchShortcut = (event: ShortcutKeyEvent, target: Partial<KeyCombo>): boolean => {
	const eventKey = event.key ? event.key.toLowerCase() : '';
	const targetKey = normalizeKeyName(target.key, target.code);
	if (!eventKey || (targetKey && targetKey !== eventKey)) {
		return false;
	}
	return modifiersMatch(
		{
			ctrlOrMeta: event.ctrlKey || event.metaKey,
			ctrl: event.ctrlKey,
			meta: event.metaKey,
			alt: event.altKey,
			shift: event.shiftKey,
		},
		target,
	);
};
const shouldPreserveConflictingAction = (action: KeybindCommand, options: MarkdownKeybindScopeOptions): boolean => {
	if (!options.preserveEditableFocusActions) return false;
	const behavior = Keybind.getDefaultByAction(action)?.editableFocusBehavior;
	return behavior === 'allow' || behavior === 'allow_when_empty';
};
const getConflictingKeybindActions = (options: MarkdownKeybindScopeOptions = {}): Set<KeybindCommand> => {
	const actions = new Set<KeybindCommand>();
	for (const {combo, action} of Keybind.getAll()) {
		if (shouldPreserveConflictingAction(action, options)) continue;
		for (const {combo: shortcutCombo} of MARKDOWN_FORMATTING_SHORTCUTS) {
			if (doesStoredComboMatchShortcut(combo, shortcutCombo)) {
				actions.add(action);
				break;
			}
		}
	}
	for (const entry of Keybind.getCustomKeybinds() as ReadonlyArray<CustomKeybindEntry>) {
		if (!entry.action || !entry.enabled) continue;
		if (shouldPreserveConflictingAction(entry.action, options)) continue;
		for (const {combo: shortcutCombo} of MARKDOWN_FORMATTING_SHORTCUTS) {
			if (doesStoredComboMatchShortcut(entry.combo, shortcutCombo)) {
				actions.add(entry.action);
				break;
			}
		}
	}
	return actions;
};

class MarkdownKeybindScope {
	private mutedActions = new Set<KeybindCommand>();
	private activeScopes = new Map<symbol, MarkdownKeybindScopeOptions>();

	acquire(options: MarkdownKeybindScopeOptions = {}): () => void {
		const token = Symbol('markdown-keybind-scope');
		this.activeScopes.set(token, options);
		this.refreshMutedKeybinds();
		let released = false;
		return () => {
			if (released) return;
			released = true;
			this.activeScopes.delete(token);
			this.refreshMutedKeybinds();
		};
	}

	private refreshMutedKeybinds(): void {
		this.restoreMutedKeybinds();
		if (this.activeScopes.size === 0) return;
		const preserveEditableFocusActions = [...this.activeScopes.values()].some(
			(options) => options.preserveEditableFocusActions,
		);
		const actions = getConflictingKeybindActions({preserveEditableFocusActions});
		for (const action of actions) {
			this.mutedActions.add(action);
		}
		Keybind.muteActions(this.mutedActions);
	}

	private restoreMutedKeybinds(): void {
		if (!this.mutedActions.size) return;
		Keybind.unmuteActions(this.mutedActions);
		this.mutedActions.clear();
	}
}

const markdownKeybindScope = new MarkdownKeybindScope();
export const useMarkdownFormattingShortcut = ({
	textareaRef,
	value,
	setValue,
	handleTextChange,
	previousValueRef,
}: {
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	handleTextChange: (newValue: string, oldValue: string) => void;
	previousValueRef: React.MutableRefObject<string>;
}): ((event: KeyboardEvent<HTMLTextAreaElement>) => void) => {
	return useCallback(
		(event: KeyboardEvent<HTMLTextAreaElement>) => {
			const textarea = textareaRef.current;
			if (!textarea) {
				return;
			}
			const selectionStart = textarea.selectionStart ?? 0;
			const selectionEnd = textarea.selectionEnd ?? 0;
			const inboxCombo = Keybind.getByAction('chat_toggle_inbox').combo;
			if (doesEventMatchShortcut(event, inboxCombo) && selectionStart === selectionEnd && value.trim().length === 0) {
				event.preventDefault();
				event.stopPropagation();
				ComponentBus.dispatch('INBOX_OPEN');
				return;
			}
			for (const {combo: shortcutCombo, wrapper} of MARKDOWN_FORMATTING_SHORTCUTS) {
				if (!doesEventMatchShortcut(event, shortcutCombo)) {
					continue;
				}
				if (selectionStart === selectionEnd) {
					return;
				}
				const selectedText = value.slice(selectionStart, selectionEnd);
				const wrapperLength = wrapper.length;
				const alreadyWrappedInside =
					selectedText.length >= wrapperLength * 2 &&
					selectedText.startsWith(wrapper) &&
					selectedText.endsWith(wrapper);
				const hasPrefixWrapper =
					wrapperLength > 0 &&
					selectionStart >= wrapperLength &&
					value.slice(selectionStart - wrapperLength, selectionStart) === wrapper;
				const hasSuffixWrapper =
					wrapperLength > 0 &&
					selectionEnd + wrapperLength <= value.length &&
					value.slice(selectionEnd, selectionEnd + wrapperLength) === wrapper;
				let newValue: string;
				let newSelectionStart: number;
				let newSelectionEnd: number;
				if (alreadyWrappedInside) {
					const unwrappedText = selectedText.slice(wrapperLength, selectedText.length - wrapperLength);
					newValue = value.slice(0, selectionStart) + unwrappedText + value.slice(selectionEnd);
					newSelectionStart = selectionStart;
					newSelectionEnd = selectionStart + unwrappedText.length;
				} else if (hasPrefixWrapper && hasSuffixWrapper) {
					newValue =
						value.slice(0, selectionStart - wrapperLength) + selectedText + value.slice(selectionEnd + wrapperLength);
					newSelectionStart = selectionStart - wrapperLength;
					newSelectionEnd = selectionEnd - wrapperLength;
				} else {
					const wrappedText = `${wrapper}${selectedText}${wrapper}`;
					newValue = value.slice(0, selectionStart) + wrappedText + value.slice(selectionEnd);
					newSelectionStart = selectionStart + wrapperLength;
					newSelectionEnd = selectionEnd + wrapperLength;
				}
				event.preventDefault();
				event.stopPropagation();
				const appliedNativeEdit = replaceTextRange(textarea, newValue, 0, value.length);
				if (!appliedNativeEdit) {
					handleTextChange(newValue, previousValueRef.current);
					setValue(newValue);
				}
				setTextSelectionSoon(textarea, newSelectionStart, newSelectionEnd);
				return;
			}
		},
		[handleTextChange, previousValueRef, setValue, textareaRef, value],
	);
};
export const useMarkdownKeybinds = (active: boolean, options: MarkdownKeybindScopeOptions = {}): void => {
	useEffect(() => {
		if (!active) {
			return;
		}
		const release = markdownKeybindScope.acquire(options);
		return release;
	}, [active, options.preserveEditableFocusActions]);
};
