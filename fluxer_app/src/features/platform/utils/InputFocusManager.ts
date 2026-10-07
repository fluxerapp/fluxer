// SPDX-License-Identifier: AGPL-3.0-or-later

import MobileLayout from '@app/features/ui/state/MobileLayout';
import Modal from '@app/features/ui/state/Modal';
import Popout from '@app/features/ui/state/Popout';

export type FocusableElementType = HTMLInputElement | HTMLTextAreaElement | HTMLDivElement;

class InputFocusManager {
	private static instance: InputFocusManager | null = null;

	static getInstance(): InputFocusManager {
		if (!InputFocusManager.instance) {
			InputFocusManager.instance = new InputFocusManager();
		}
		return InputFocusManager.instance;
	}

	private constructor() {}

	private isFocusableElement(element: Element | null): element is FocusableElementType {
		if (!element) return false;
		if (element instanceof HTMLInputElement || element instanceof HTMLTextAreaElement) {
			return true;
		}
		if (element instanceof HTMLDivElement && (element as HTMLDivElement).contentEditable === 'true') {
			return true;
		}
		return false;
	}

	isInputFocused(excludingElement?: FocusableElementType): boolean {
		const activeElement = document.activeElement;
		if (!this.isFocusableElement(activeElement)) {
			return false;
		}
		if (excludingElement && activeElement === excludingElement) {
			return false;
		}
		return true;
	}

	canFocusTextarea(textareaElement?: FocusableElementType): boolean {
		if (textareaElement) {
			if (textareaElement instanceof HTMLInputElement || textareaElement instanceof HTMLTextAreaElement) {
				if (textareaElement.disabled) return false;
			}
			if (
				textareaElement.getAttribute('aria-disabled') === 'true' ||
				textareaElement.closest('[hidden], [inert], [aria-hidden="true"]')
			) {
				return false;
			}
		}
		const hasModalOpen = Modal?.hasModalOpen?.() ?? false;
		const hasPopoutsOpen = (Popout?.getPopouts?.() ?? []).length > 0;
		const isMobileLayout = !!MobileLayout?.enabled;
		const inputFocused = this.isInputFocused(textareaElement);
		return !(isMobileLayout || hasModalOpen || hasPopoutsOpen || inputFocused);
	}

	safeFocus(element: FocusableElementType, force: boolean = false): boolean {
		if (!force && !this.canFocusTextarea(element)) {
			return false;
		}
		if (element instanceof HTMLElement) {
			element.focus();
			return true;
		}
		return false;
	}
}

const inputFocusManager = InputFocusManager.getInstance();

export function isInputFocused(excludingElement?: FocusableElementType) {
	return inputFocusManager.isInputFocused(excludingElement);
}

export function canFocusTextarea(textareaElement?: FocusableElementType) {
	return inputFocusManager.canFocusTextarea(textareaElement);
}

export function safeFocus(element: FocusableElementType, force?: boolean) {
	return inputFocusManager.safeFocus(element, force);
}
