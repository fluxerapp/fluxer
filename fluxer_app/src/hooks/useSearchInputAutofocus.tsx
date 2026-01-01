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

import React from 'react';
import {isTextInputKeyEvent} from '~/lib/isTextInputKeyEvent';
import ModalStore from '~/stores/ModalStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';

const MODAL_KEYBOARD_SELECTOR = '[role="dialog"], .modal-backdrop';

const isTextEntryElement = (element: Element | null): boolean => {
	if (element instanceof HTMLInputElement || element instanceof HTMLTextAreaElement) {
		return true;
	}
	return element instanceof HTMLElement && element.isContentEditable;
};

const isElementInsideModal = (element: Element | null): boolean => {
	return Boolean(element?.closest(MODAL_KEYBOARD_SELECTOR));
};

interface SearchInputRef {
	current: HTMLInputElement | null;
}

export const useSearchInputAutofocus = (inputRef: SearchInputRef) => {
	React.useEffect(() => {
		const shouldBlockDueToModal = (): boolean => {
			if (!ModalStore.hasModalOpen()) {
				return false;
			}
			const input = inputRef.current;
			return !isElementInsideModal(input);
		};

		if (!shouldBlockDueToModal()) {
			inputRef.current?.focus({preventScroll: true});
		}

		const handleGlobalKeyDown = (event: KeyboardEvent) => {
			if (QuickSwitcherStore.getIsOpen()) {
				return;
			}

			if (shouldBlockDueToModal()) {
				return;
			}

			const activeElement = document.activeElement;

			if (activeElement === inputRef.current) {
				return;
			}

			if (isTextEntryElement(activeElement)) {
				return;
			}

			if (!isTextInputKeyEvent(event)) {
				return;
			}

			inputRef.current?.focus({preventScroll: true});
		};

		document.addEventListener('keydown', handleGlobalKeyDown);
		return () => document.removeEventListener('keydown', handleGlobalKeyDown);
	}, [inputRef]);
};
