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
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import styles from './SearchFilterChip.module.css';

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
	const {t} = useLingui();
	return (
		<button type="button" className={clsx(styles.chip, isActive && styles.chipActive)} onClick={onPress}>
			{icon && <span className={styles.chipIcon}>{icon}</span>}
			<span className={styles.chipContent}>
				<span className={clsx(styles.chipLabel, isActive && styles.chipLabelActive)}>{label}</span>
				{value && <span className={clsx(styles.chipValue, isActive && styles.chipValueActive)}>{value}</span>}
			</span>
			{isActive && onRemove && (
				<button
					type="button"
					className={styles.removeButton}
					onClick={(e) => {
						e.stopPropagation();
						onRemove();
					}}
					aria-label={t`Remove filter`}
				>
					<XIcon size={12} weight="bold" />
				</button>
			)}
		</button>
	);
};
