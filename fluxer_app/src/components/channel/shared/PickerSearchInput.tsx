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

import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import React from 'react';
import {Input} from '~/components/form/Input';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {useInputFocusManagement} from '~/hooks/useInputFocusManagement';
import {isTextInputKeyEvent} from '~/lib/isTextInputKeyEvent';
import ContextMenuStore from '~/stores/ContextMenuStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import ModalStore from '~/stores/ModalStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import styles from './PickerSearchInput.module.css';

const MODAL_KEYBOARD_SELECTOR = '[role="dialog"], .modal-backdrop';

const isNodeInsideModal = (node?: Node | null) => {
	if (!(node instanceof Element)) return false;
	return Boolean(node.closest(MODAL_KEYBOARD_SELECTOR));
};

const isModalKeyboardEvent = (event: KeyboardEvent) => {
	if (isNodeInsideModal(event.target as Node | null)) {
		return true;
	}

	if (typeof event.composedPath === 'function') {
		for (const node of event.composedPath()) {
			if (isNodeInsideModal(node as Node)) {
				return true;
			}
		}
	}

	return isNodeInsideModal(document.activeElement);
};

interface PickerSearchInputProps {
	value: string;
	onChange: (value: string) => void;
	placeholder?: string;
	inputRef?: React.Ref<HTMLInputElement>;
	onKeyDown?: (event: React.KeyboardEvent<HTMLInputElement>) => void;
	maxLength?: number;
	showBackButton?: boolean;
	onBackButtonClick?: () => void;
}

const assignRef = <T,>(ref: React.Ref<T> | null | undefined, value: T | null) => {
	if (!ref) return;
	if (typeof ref === 'function') {
		ref(value);
		return;
	}
	(ref as React.MutableRefObject<T | null>).current = value;
};

export const PickerSearchInput = React.forwardRef<HTMLInputElement, PickerSearchInputProps>(
	(
		{value, onChange, placeholder, inputRef, onKeyDown, maxLength = 100, showBackButton = false, onBackButtonClick},
		forwardedRef,
	) => {
		const _inputElementRef = React.useRef<HTMLInputElement | null>(null);
		const {t} = useLingui();
		const inputElementRef = React.useRef<HTMLInputElement | null>(null);
		const {canFocus, safeFocusTextarea} = useInputFocusManagement(inputElementRef);
		const valueRef = React.useRef(value);

		React.useEffect(() => {
			valueRef.current = value;
		}, [value]);

		const setInputRefs = React.useCallback(
			(element: HTMLInputElement | null) => {
				inputElementRef.current = element;
				assignRef(inputRef, element);
				assignRef(forwardedRef, element);
			},
			[forwardedRef, inputRef],
		);

		const handleChange = React.useCallback(
			(event: React.ChangeEvent<HTMLInputElement>) => {
				onChange(event.target.value);
			},
			[onChange],
		);

		const handleClear = () => {
			onChange('');
		};

		React.useEffect(() => {
			if (MobileLayoutStore.enabled) {
				return;
			}

			const timer = setTimeout(() => {
				if (ModalStore.hasModalOpen()) {
					return;
				}
				safeFocusTextarea();
			}, 100);

			return () => {
				clearTimeout(timer);
			};
		}, [safeFocusTextarea]);

		React.useEffect(() => {
			const handleKeyDown = (event: KeyboardEvent) => {
				const input = inputElementRef.current;
				if (!input) {
					return;
				}

				if (!canFocus()) {
					return;
				}

				if (document.activeElement === input) {
					return;
				}

				if (QuickSwitcherStore.getIsOpen()) {
					return;
				}

				if (ContextMenuStore.contextMenu) {
					return;
				}

				if (ModalStore.hasModalOpen()) {
					return;
				}

				if (isModalKeyboardEvent(event)) {
					return;
				}

				if (KeyboardModeStore.keyboardModeEnabled) {
					const focusedElement = document.activeElement;
					if (focusedElement?.closest('[data-message-id]')) {
						return;
					}
				}

				if (!isTextInputKeyEvent(event)) {
					return;
				}

				if (event.key === 'Dead') {
					safeFocusTextarea(true);
					return;
				}

				event.preventDefault();
				safeFocusTextarea(true);
				onChange(valueRef.current + event.key);
			};

			window.addEventListener('keydown', handleKeyDown);
			return () => {
				window.removeEventListener('keydown', handleKeyDown);
			};
		}, [canFocus, onChange, safeFocusTextarea]);

		return (
			<div className={styles.searchInputContainer}>
				{showBackButton && onBackButtonClick && (
					<FocusRing offset={-2}>
						<button
							type="button"
							className={styles.backButton}
							onClick={onBackButtonClick}
							aria-label={t`Clear search`}
						>
							<ArrowLeftIcon size={20} weight="regular" />
						</button>
					</FocusRing>
				)}
				<Input
					ref={setInputRefs}
					value={value}
					placeholder={placeholder ?? t`Search`}
					onChange={handleChange}
					onKeyDown={onKeyDown}
					maxLength={maxLength}
					className={styles.searchInput}
					leftIcon={<MagnifyingGlassIcon size={18} weight="regular" />}
					rightElement={
						value ? (
							<button type="button" className={styles.clearButton} onClick={handleClear} aria-label={t`Clear search`}>
								<XIcon size={18} weight="regular" />
							</button>
						) : undefined
					}
				/>
			</div>
		);
	},
);

PickerSearchInput.displayName = 'PickerSearchInput';
