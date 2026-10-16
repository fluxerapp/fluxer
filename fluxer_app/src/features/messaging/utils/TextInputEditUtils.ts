// SPDX-License-Identifier: AGPL-3.0-or-later

export type EditableTextInput = HTMLInputElement | HTMLTextAreaElement;

export function isEditableTextInput(element: Element | null): element is EditableTextInput {
	return element instanceof HTMLInputElement || element instanceof HTMLTextAreaElement;
}

const DIALOG_PASTE_TARGET_SELECTOR = '[role="dialog"], [aria-modal="true"]';

export function isDialogPasteTarget(target: EventTarget | null): boolean {
	return target instanceof Element && target.closest(DIALOG_PASTE_TARGET_SELECTOR) !== null;
}

function replaceTextRange(input: EditableTextInput, text: string, start: number, end: number): boolean {
	const rangeStart = Math.min(start, end);
	const rangeEnd = Math.max(start, end);
	const expectedValue = input.value.slice(0, rangeStart) + text + input.value.slice(rangeEnd);
	const selectionMode = 'end';
	if (typeof document !== 'undefined' && document.activeElement !== input) {
		try {
			input.focus({preventScroll: true});
		} catch {}
	}
	if (typeof document !== 'undefined' && document.activeElement === input) {
		try {
			input.setSelectionRange(rangeStart, rangeEnd);
			const command = text.length === 0 ? 'delete' : 'insertText';
			const ok = document.execCommand(command, false, text);
			if (ok && input.value === expectedValue) {
				return true;
			}
		} catch {}
	}
	try {
		input.setRangeText(text, rangeStart, rangeEnd, selectionMode);
	} catch {
		return false;
	}
	input.dispatchEvent(
		new InputEvent('input', {
			bubbles: true,
			data: text || null,
			inputType: text.length === 0 ? 'deleteContent' : 'insertText',
		}),
	);
	return true;
}

export function replaceSelectedText(input: EditableTextInput, text: string): boolean {
	const start = input.selectionStart ?? input.value.length;
	const end = input.selectionEnd ?? input.value.length;
	return replaceTextRange(input, text, start, end);
}
