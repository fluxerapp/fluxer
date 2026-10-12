// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/MemesPicker.module.css';
import {PickerSearchInput} from '@app/features/channel/components/shared/PickerSearchInput';
import {AUDIO_DESCRIPTOR, GIFS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GifIcon, ImageIcon, MusicNoteIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

const ALL_DESCRIPTOR = msg({
	message: 'All',
	comment: 'Short label in the channel and chat memes picker header. Keep it concise.',
});
const IMAGES_DESCRIPTOR = msg({
	message: 'Images',
	comment: 'Short label in the channel and chat memes picker header. Keep it concise.',
});
const VIDEOS_DESCRIPTOR = msg({
	message: 'Videos',
	comment: 'Short label in the channel and chat memes picker header. Keep it concise.',
});
const SEARCH_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Search saved media',
	comment: 'Button or menu action label in the channel and chat memes picker header. Keep it concise.',
});

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
	className,
	showBackButton = true,
}: {
	className?: string;
	showBackButton?: boolean;
	searchTerm: string;
	onSearchTermChange: (value: string) => void;
	onClearSearch: () => void;
	selectedFilter: ContentType;
	onFilterChange: (filter: ContentType) => void;
	inputRef: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>;
}) {
	const {i18n} = useLingui();
	const FILTER_OPTIONS: Array<FilterOption> = [
		{type: 'all', label: i18n._(ALL_DESCRIPTOR)},
		{
			type: 'image',
			label: i18n._(IMAGES_DESCRIPTOR),
			icon: (
				<ImageIcon
					className={styles.filterPillIcon}
					data-flx="channel.pickers.memes.memes-picker-header.filter-pill-icon"
				/>
			),
		},
		{
			type: 'video',
			label: i18n._(VIDEOS_DESCRIPTOR),
			icon: (
				<VideoCameraIcon
					className={styles.filterPillIcon}
					data-flx="channel.pickers.memes.memes-picker-header.filter-pill-icon--2"
				/>
			),
		},
		{
			type: 'audio',
			label: i18n._(AUDIO_DESCRIPTOR),
			icon: (
				<MusicNoteIcon
					className={styles.filterPillIcon}
					data-flx="channel.pickers.memes.memes-picker-header.filter-pill-icon--3"
				/>
			),
		},
		{
			type: 'gif',
			label: i18n._(GIFS_DESCRIPTOR),
			icon: (
				<GifIcon
					className={styles.filterPillIcon}
					data-flx="channel.pickers.memes.memes-picker-header.filter-pill-icon--4"
				/>
			),
		},
	];
	return (
		<div
			className={className ?? styles.headerContainer}
			data-flx="channel.pickers.memes.memes-picker-header.header-container"
		>
			<PickerSearchInput
				value={searchTerm}
				onChange={onSearchTermChange}
				placeholder={i18n._(SEARCH_SAVED_MEDIA_DESCRIPTOR)}
				inputRef={inputRef}
				showBackButton={showBackButton && Boolean(searchTerm)}
				onBackButtonClick={onClearSearch}
				data-flx="channel.pickers.memes.memes-picker-header.picker-search-input.search-term-change"
			/>
			<div className={styles.filterList} data-flx="channel.pickers.memes.memes-picker-header.filter-list">
				{FILTER_OPTIONS.map((option) => {
					const isActive = selectedFilter === option.type;
					return (
						<button
							key={option.type}
							type="button"
							onClick={() => onFilterChange(option.type)}
							aria-pressed={isActive}
							className={clsx(styles.filterPill, isActive && styles.filterPillActive)}
							data-flx="channel.pickers.memes.memes-picker-header.filter-pill.filter-change.button"
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
