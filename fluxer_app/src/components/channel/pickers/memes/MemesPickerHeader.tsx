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
import {GifIcon, ImageIcon, MusicNoteIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import styles from '~/components/channel/MemesPicker.module.css';
import {PickerSearchInput} from '~/components/channel/shared/PickerSearchInput';

export type ContentType = 'all' | 'image' | 'video' | 'audio' | 'gif';

interface FilterOption {
	type: ContentType;
	label: string;
	icon?: React.ReactNode;
}

export function MemesPickerHeader({
	searchTerm,
	onSearchTermChange,
	onClearSearch,
	selectedFilter,
	onFilterChange,
	inputRef,
}: {
	searchTerm: string;
	onSearchTermChange: (value: string) => void;
	onClearSearch: () => void;
	selectedFilter: ContentType;
	onFilterChange: (filter: ContentType) => void;
	inputRef: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>;
}) {
	const {t} = useLingui();

	const FILTER_OPTIONS: Array<FilterOption> = [
		{type: 'all', label: t`All`},
		{type: 'image', label: t`Images`, icon: <ImageIcon className={styles.filterPillIcon} />},
		{type: 'video', label: t`Videos`, icon: <VideoCameraIcon className={styles.filterPillIcon} />},
		{type: 'audio', label: t`Audio`, icon: <MusicNoteIcon className={styles.filterPillIcon} />},
		{type: 'gif', label: t`GIFs`, icon: <GifIcon className={styles.filterPillIcon} />},
	];

	return (
		<div className={styles.headerContainer}>
			<PickerSearchInput
				value={searchTerm}
				onChange={onSearchTermChange}
				placeholder={t`Search saved media`}
				inputRef={inputRef}
				showBackButton={Boolean(searchTerm)}
				onBackButtonClick={onClearSearch}
			/>

			<div className={styles.filterList}>
				{FILTER_OPTIONS.map((option) => {
					const isActive = selectedFilter === option.type;
					return (
						<button
							key={option.type}
							type="button"
							onClick={() => onFilterChange(option.type)}
							className={clsx(styles.filterPill, isActive && styles.filterPillActive)}
						>
							{option.icon}
							{option.label}
						</button>
					);
				})}
			</div>
		</div>
	);
}
