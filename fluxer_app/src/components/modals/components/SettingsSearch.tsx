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
import {MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';
import {Input} from '../../form/Input';
import styles from './SettingsSearch.module.css';

interface SettingsSearchProps {
	className?: string;
	placeholder?: string;
	value?: string;
	onChange?: (value: string) => void;
}

export const SettingsSearch: React.FC<SettingsSearchProps> = observer(
	({className, placeholder, value: controlledValue, onChange}) => {
		const {t} = useLingui();
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
			<button type="button" onClick={handleClear} className={styles.clearButton} aria-label={t`Clear search`}>
				<XIcon size={14} weight="bold" />
			</button>
		) : undefined;

		return (
			<div className={clsx(styles.container, className)} role="search">
				<div className={styles.inputContainer}>
					<Input
						ref={searchInputRef}
						type="text"
						value={query}
						onChange={(e) => handleQueryChange(e.target.value)}
						onKeyDown={handleKeyDown}
						onFocus={handleFocus}
						onBlur={handleBlur}
						placeholder={placeholder ?? t`Search settings...`}
						aria-label={t`Search settings`}
						leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
						rightElement={rightElement}
					/>
				</div>
			</div>
		);
	},
);
