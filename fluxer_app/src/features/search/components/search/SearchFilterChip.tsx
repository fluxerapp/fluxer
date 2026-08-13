// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/search/components/search/SearchFilterChip.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

const REMOVE_FILTER_DESCRIPTOR = msg({
	message: 'Remove filter',
	comment:
		'Button or menu action label in the search search filter chip. Keep it concise. Keep the tone plain and specific.',
});

interface SearchFilterChipProps {
	label: string;
	value?: string;
	onPress?: () => void;
	onRemove?: () => void;
	isActive?: boolean;
	icon?: React.ReactNode;
}

export const SearchFilterChip: React.FC<SearchFilterChipProps> = ({
	label,
	value,
	onPress,
	onRemove,
	isActive = false,
	icon,
}) => {
	const {i18n} = useLingui();
	return (
		<button
			type="button"
			className={clsx(styles.chip, isActive && styles.chipActive)}
			aria-pressed={isActive}
			onClick={onPress}
			data-flx="search.search.search-filter-chip.chip.press.button"
		>
			{icon && (
				<span className={styles.chipIcon} data-flx="search.search.search-filter-chip.chip-icon">
					{icon}
				</span>
			)}
			<span className={styles.chipContent} data-flx="search.search.search-filter-chip.chip-content">
				<span
					className={clsx(styles.chipLabel, isActive && styles.chipLabelActive)}
					data-flx="search.search.search-filter-chip.chip-label"
				>
					{label}
				</span>
				{value && (
					<span
						className={clsx(styles.chipValue, isActive && styles.chipValueActive)}
						data-flx="search.search.search-filter-chip.chip-value"
					>
						{value}
					</span>
				)}
			</span>
			{isActive && onRemove && (
				<button
					type="button"
					className={styles.removeButton}
					onClick={(e) => {
						e.stopPropagation();
						onRemove();
					}}
					aria-label={i18n._(REMOVE_FILTER_DESCRIPTOR)}
					data-flx="search.search.search-filter-chip.remove-button.stop-propagation"
				>
					<XIcon size={remFromPx(12)} weight="bold" data-flx="search.search.search-filter-chip.x-icon" />
				</button>
			)}
		</button>
	);
};
