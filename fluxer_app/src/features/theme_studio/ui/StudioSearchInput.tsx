// SPDX-License-Identifier: AGPL-3.0-or-later

import {CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme_studio/ui/StudioSearchInput.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, XCircleIcon} from '@phosphor-icons/react';
import {forwardRef, type InputHTMLAttributes, useImperativeHandle, useRef} from 'react';

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

interface StudioSearchInputProps
	extends Omit<InputHTMLAttributes<HTMLInputElement>, 'value' | 'onChange' | 'type' | 'className'> {
	value: string;
	onChange: (next: string) => void;
}

export const StudioSearchInput = forwardRef<StudioSearchInputHandle, StudioSearchInputProps>(function StudioSearchInput(
	{value, onChange, placeholder, ...rest},
	ref,
) {
	const {i18n} = useLingui();
	const inputRef = useRef<HTMLInputElement | null>(null);
	useImperativeHandle(ref, () => ({
		focus: () => inputRef.current?.focus(),
	}));
	return (
		<label className={styles.wrapper} data-flx="theme-studio.ui.studio-search-input.wrapper">
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
			) : null}
		</label>
	);
});
