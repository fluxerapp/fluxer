// SPDX-License-Identifier: AGPL-3.0-or-later

import {useInputFocusManagement} from '@app/features/app/hooks/useInputFocusManagement';
import styles from '@app/features/channel/components/shared/PickerSearchInput.module.css';
import {CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import {isTextInputKeyEvent} from '@app/features/platform/utils/IsTextInputKeyEvent';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import ContextMenu from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Modal from '@app/features/ui/state/Modal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import React, {useCallback, useEffect, useRef} from 'react';

const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment: 'Button or menu action label in the channel and chat picker search input. Keep it concise.',
});
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
	rightCustomElement?: React.ReactNode;
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
		{
			value,
			onChange,
			placeholder,
			inputRef,
			onKeyDown,
			maxLength = 100,
			showBackButton = false,
			onBackButtonClick,
			rightCustomElement,
		},
		forwardedRef,
	) => {
		const {i18n} = useLingui();
		const inputElementRef = useRef<HTMLInputElement | null>(null);
		const {canFocus, safeFocusTextarea} = useInputFocusManagement(inputElementRef);
		const valueRef = useRef(value);
		useEffect(() => {
			valueRef.current = value;
		}, [value]);
		const setInputRefs = useCallback(
			(element: HTMLInputElement | null) => {
				inputElementRef.current = element;
				assignRef(inputRef, element);
				assignRef(forwardedRef, element);
			},
			[forwardedRef, inputRef],
		);
		const handleChange = useCallback(
			(event: React.ChangeEvent<HTMLInputElement>) => {
				onChange(event.target.value);
			},
			[onChange],
		);
		const handleClear = () => {
			onChange('');
		};
		useEffect(() => {
			if (MobileLayout.enabled) {
				return;
			}
			const timer = setTimeout(() => {
				if (Modal.hasModalOpen()) {
					return;
				}
				safeFocusTextarea();
			}, 100);
			return () => {
				clearTimeout(timer);
			};
		}, [safeFocusTextarea]);
		useEffect(() => {
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
				if (QuickSwitcher.getIsOpen()) {
					return;
				}
				if (ContextMenu.contextMenu) {
					return;
				}
				if (Modal.hasModalOpen()) {
					return;
				}
				if (isModalKeyboardEvent(event)) {
					return;
				}
				if (KeyboardMode.keyboardModeEnabled && MessageFocus.focusedMessageId) {
					return;
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
			<div className={styles.searchInputContainer} data-flx="channel.picker-search-input.search-input-container">
				{showBackButton && onBackButtonClick && (
					<FocusRing offset={-2} data-flx="channel.picker-search-input.focus-ring">
						<button
							type="button"
							className={styles.backButton}
							onClick={onBackButtonClick}
							aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
							data-flx="channel.picker-search-input.back-button"
						>
							<ArrowLeftIcon
								size={remFromPx(20)}
								weight="regular"
								data-flx="channel.picker-search-input.arrow-left-icon"
							/>
						</button>
					</FocusRing>
				)}
				<Input
					ref={setInputRefs}
					value={value}
					placeholder={placeholder ?? i18n._(SEARCH_DESCRIPTOR)}
					onChange={handleChange}
					onKeyDown={onKeyDown}
					maxLength={maxLength}
					className={styles.searchInput}
					data-autofocus=""
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(18)}
							weight="regular"
							data-flx="channel.picker-search-input.magnifying-glass-icon"
						/>
					}
					rightElement={
						<div
							className={styles.rightElementContainer}
							data-flx="channel.picker-search-input.right-element-container"
						>
							{rightCustomElement}
							{value ? (
								<button
									type="button"
									className={styles.clearButton}
									onClick={handleClear}
									aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
									data-flx="channel.picker-search-input.clear-button"
								>
									<XIcon size={remFromPx(18)} weight="bold" data-flx="channel.picker-search-input.x-icon" />
								</button>
							) : null}
						</div>
					}
					data-flx="channel.picker-search-input.search-input.change"
				/>
			</div>
		);
	},
);

PickerSearchInput.displayName = 'PickerSearchInput';
