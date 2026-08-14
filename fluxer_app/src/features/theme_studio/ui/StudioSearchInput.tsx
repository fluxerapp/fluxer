// SPDX-License-Identifier: AGPL-3.0-or-later

import {CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, XCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {forwardRef, type InputHTMLAttributes, useImperativeHandle, useRef} from 'react';
import styles from './StudioSearchInput.module.css';

const SEARCH_DESCRIPTOR = msg({
	message: 'Search…',
	comment: 'Button or menu action label in the theme studio studio search input. Keep it concise.',
});
const SEARCH_2_DESCRIPTOR = msg({
	message: 'Search',
	comment: 'Button or menu action label in the theme studio studio search input. Keep it concise.',
});

export interface StudioSearchInputHandle {
	focus: () => void;
}

interface StudioSearchInputProps extends Omit<InputHTMLAttributes<HTMLInputElement>, 'value' | 'onChange' | 'type'> {
	value: string;
	onChange: (next: string) => void;
	shortcut?: string;
	className?: string;
}

export const StudioSearchInput = forwardRef<StudioSearchInputHandle, StudioSearchInputProps>(function StudioSearchInput(
	{value, onChange, shortcut, className, placeholder, ...rest},
	ref,
) {
	const {i18n} = useLingui();
	const inputRef = useRef<HTMLInputElement | null>(null);
	useImperativeHandle(ref, () => ({
		focus: () => inputRef.current?.focus(),
	}));
	return (
		<label className={clsx(styles.wrapper, className)} data-flx="theme-studio.ui.studio-search-input.wrapper">
			<span className={styles.icon} data-flx="theme-studio.ui.studio-search-input.icon">
				<MagnifyingGlassIcon
					size={remFromPx(14)}
					weight="bold"
					data-flx="theme-studio.ui.studio-search-input.magnifying-glass-icon"
				/>
			</span>
			<input
				ref={inputRef}
				type="search"
				className={styles.input}
				value={value}
				placeholder={placeholder ?? i18n._(SEARCH_DESCRIPTOR)}
				onChange={(event) => onChange(event.target.value)}
				aria-label={i18n._(SEARCH_2_DESCRIPTOR)}
				data-flx="theme-studio.ui.studio-search-input.input.change.search"
				{...rest}
			/>
			{value.length > 0 ? (
				<button
					type="button"
					aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
					className={styles.clear}
					onClick={() => {
						onChange('');
						inputRef.current?.focus();
					}}
					data-flx="theme-studio.ui.studio-search-input.clear.change.button"
				>
					<XCircleIcon
						size={remFromPx(14)}
						weight="fill"
						data-flx="theme-studio.ui.studio-search-input.x-circle-icon"
					/>
				</button>
			) : shortcut ? (
				<span className={styles.shortcut} data-flx="theme-studio.ui.studio-search-input.shortcut">
					{shortcut}
				</span>
			) : null}
		</label>
	);
});
