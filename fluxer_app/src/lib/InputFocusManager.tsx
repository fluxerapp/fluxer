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

import MobileLayoutStore from '~/stores/MobileLayoutStore';
import ModalStore from '~/stores/ModalStore';
import PopoutStore from '~/stores/PopoutStore';

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
		const hasModalOpen = ModalStore?.hasModalOpen?.() ?? false;
		const hasPopoutsOpen = (PopoutStore?.getPopouts?.() ?? []).length > 0;
		const isMobileLayout = !!MobileLayoutStore?.enabled;

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

export const inputFocusManager = InputFocusManager.getInstance();

export const isInputFocused = (excludingElement?: FocusableElementType) =>
	inputFocusManager.isInputFocused(excludingElement);

export const canFocusTextarea = (textareaElement?: FocusableElementType) =>
	inputFocusManager.canFocusTextarea(textareaElement);

export const safeFocus = (element: FocusableElementType, force?: boolean) =>
	inputFocusManager.safeFocus(element, force);
