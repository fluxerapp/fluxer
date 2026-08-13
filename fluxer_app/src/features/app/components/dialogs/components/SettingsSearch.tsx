// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/components/SettingsSearch.module.css';
import {
	CLEAR_SEARCH_DESCRIPTOR,
	SEARCH_SETTINGS_FIELD_LABEL_DESCRIPTOR,
	SEARCH_SETTINGS_PLACEHOLDER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

interface SettingsSearchProps {
	className?: string;
	placeholder?: string;
	value?: string;
	onChange?: (value: string) => void;
}

export const SettingsSearch: React.FC<SettingsSearchProps> = observer(
	({className, placeholder, value: controlledValue, onChange}) => {
		const {i18n} = useLingui();
		const [internalQuery, setInternalQuery] = useState('');
		const query = controlledValue !== undefined ? controlledValue : internalQuery;
		const searchInputRef = useRef<HTMLInputElement>(null);
		const shouldMaintainFocusRef = useRef(false);
		useLayoutEffect(() => {
			if (shouldMaintainFocusRef.current && searchInputRef.current) {
				const activeElement = document.activeElement;
				if (activeElement !== searchInputRef.current) {
					searchInputRef.current.focus();
				}
			}
		});
		useEffect(() => {
			if (shouldMaintainFocusRef.current && searchInputRef.current) {
				requestAnimationFrame(() => {
					if (shouldMaintainFocusRef.current && searchInputRef.current) {
						searchInputRef.current.focus();
					}
				});
			}
		}, [query]);
		useEffect(() => {
			const handleKeyDown = (event: KeyboardEvent) => {
				if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === 'f') {
					event.preventDefault();
					event.stopPropagation();
					searchInputRef.current?.focus();
				}
			};
			document.addEventListener('keydown', handleKeyDown, true);
			return () => document.removeEventListener('keydown', handleKeyDown, true);
		}, []);
		const handleQueryChange = useCallback(
			(newValue: string) => {
				if (controlledValue !== undefined) {
					onChange?.(newValue);
				} else {
					setInternalQuery(newValue);
				}
			},
			[controlledValue, onChange],
		);
		const handleKeyDown = useCallback((event: React.KeyboardEvent) => {
			if (event.key === 'Escape') {
				searchInputRef.current?.blur();
			}
		}, []);
		const handleFocus = useCallback(() => {
			shouldMaintainFocusRef.current = true;
		}, []);
		const handleBlur = useCallback(() => {
			shouldMaintainFocusRef.current = false;
		}, []);
		const handleClear = useCallback(() => {
			handleQueryChange('');
			searchInputRef.current?.focus();
		}, [handleQueryChange]);
		const rightElement = query ? (
			<FocusRing offset={-2} data-flx="app.settings-search.focus-ring">
				<button
					type="button"
					onClick={handleClear}
					className={styles.clearButton}
					aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
					data-flx="app.settings-search.clear-button"
				>
					<XIcon size={remFromPx(14)} weight="bold" data-flx="app.settings-search.x-icon" />
				</button>
			</FocusRing>
		) : undefined;
		return (
			<div className={clsx(styles.container, className)} role="search" data-flx="app.settings-search.container">
				<div className={styles.inputContainer} data-flx="app.settings-search.input-container">
					<Input
						ref={searchInputRef}
						type="text"
						value={query}
						className={styles.searchInput}
						onChange={(e) => handleQueryChange(e.target.value)}
						onKeyDown={handleKeyDown}
						onFocus={handleFocus}
						onBlur={handleBlur}
						placeholder={placeholder ?? i18n._(SEARCH_SETTINGS_PLACEHOLDER_DESCRIPTOR)}
						aria-label={i18n._(SEARCH_SETTINGS_FIELD_LABEL_DESCRIPTOR)}
						leftIcon={
							<MagnifyingGlassIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="app.settings-search.magnifying-glass-icon"
							/>
						}
						rightElement={rightElement}
						data-flx="app.settings-search.input.query-change.text"
					/>
				</div>
			</div>
		);
	},
);
