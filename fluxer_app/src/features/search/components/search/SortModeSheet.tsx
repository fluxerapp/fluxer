// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelSearchSortMode} from '@app/features/channel/hooks/useChannelSearch';
import styles from '@app/features/search/components/search/SortModeSheet.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {IconProps} from '@phosphor-icons/react';
import {CheckIcon, ClockClockwiseIcon, ClockCounterClockwiseIcon, SparkleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

const NEWEST_FIRST_DESCRIPTOR = msg({
	message: 'Newest first',
	comment: 'Sort mode option label in the mobile search sort sheet.',
});
const SHOW_MOST_RECENT_MESSAGES_FIRST_DESCRIPTOR = msg({
	message: 'Show most recent messages first',
	comment: 'Description text for the Newest first option in the mobile search sort sheet.',
});
const OLDEST_FIRST_DESCRIPTOR = msg({
	message: 'Oldest first',
	comment: 'Sort mode option label in the mobile search sort sheet.',
});
const SHOW_OLDEST_MESSAGES_FIRST_DESCRIPTOR = msg({
	message: 'Show oldest messages first',
	comment: 'Description text for the Oldest first option in the mobile search sort sheet.',
});
const MOST_RELEVANT_DESCRIPTOR = msg({
	message: 'Most relevant',
	comment: 'Sort mode option label in the mobile search sort sheet. Sort by relevance score.',
});
const SHOW_MOST_RELEVANT_MESSAGES_FIRST_DESCRIPTOR = msg({
	message: 'Show most relevant messages first',
	comment: 'Description text for the Most relevant option in the mobile search sort sheet.',
});
const SORT_RESULTS_BY_DESCRIPTOR = msg({
	message: 'Sort results by',
	comment: 'Title of the mobile search sort mode bottom sheet.',
});

interface SortOption {
	mode: ChannelSearchSortMode;
	label: MessageDescriptor;
	description: MessageDescriptor;
	icon: React.ComponentType<IconProps>;
}

const SORT_OPTIONS: Array<SortOption> = [
	{
		mode: 'newest',
		label: NEWEST_FIRST_DESCRIPTOR,
		description: SHOW_MOST_RECENT_MESSAGES_FIRST_DESCRIPTOR,
		icon: ClockClockwiseIcon,
	},
	{
		mode: 'oldest',
		label: OLDEST_FIRST_DESCRIPTOR,
		description: SHOW_OLDEST_MESSAGES_FIRST_DESCRIPTOR,
		icon: ClockCounterClockwiseIcon,
	},
	{
		mode: 'relevant',
		label: MOST_RELEVANT_DESCRIPTOR,
		description: SHOW_MOST_RELEVANT_MESSAGES_FIRST_DESCRIPTOR,
		icon: SparkleIcon,
	},
];

interface SortModeSheetProps {
	isOpen: boolean;
	onClose: () => void;
	selectedMode: ChannelSearchSortMode;
	onModeChange: (mode: ChannelSearchSortMode) => void;
}

export const SortModeSheet: React.FC<SortModeSheetProps> = ({isOpen, onClose, selectedMode, onModeChange}) => {
	const {i18n} = useLingui();
	const handleSelect = (mode: ChannelSearchSortMode) => {
		onModeChange(mode);
		onClose();
	};
	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			title={i18n._(SORT_RESULTS_BY_DESCRIPTOR)}
			disablePadding
			data-flx="search.search.sort-mode-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="search.search.sort-mode-sheet.container">
				<div className={styles.optionsContainer} data-flx="search.search.sort-mode-sheet.options-container">
					{SORT_OPTIONS.map((option) => {
						const isSelected = selectedMode === option.mode;
						const Icon = option.icon;
						return (
							<button
								key={option.mode}
								type="button"
								aria-pressed={isSelected}
								className={clsx(styles.option, isSelected && styles.optionSelected)}
								onClick={() => handleSelect(option.mode)}
								data-flx="search.search.sort-mode-sheet.option.select.button"
							>
								<div className={styles.optionLeft} data-flx="search.search.sort-mode-sheet.option-left">
									<Icon
										size={remFromPx(22)}
										className={clsx(styles.optionIcon, isSelected && styles.optionIconSelected)}
										weight="regular"
										data-flx="search.search.sort-mode-sheet.option-icon"
									/>
									<div className={styles.optionText} data-flx="search.search.sort-mode-sheet.option-text">
										<span
											className={clsx(styles.optionLabel, isSelected && styles.optionLabelSelected)}
											data-flx="search.search.sort-mode-sheet.option-label"
										>
											{i18n._(option.label)}
										</span>
										<span
											className={styles.optionDescription}
											data-flx="search.search.sort-mode-sheet.option-description"
										>
											{i18n._(option.description)}
										</span>
									</div>
								</div>
								{isSelected && (
									<CheckIcon
										size={remFromPx(20)}
										className={styles.checkIcon}
										weight="bold"
										data-flx="search.search.sort-mode-sheet.check-icon"
									/>
								)}
							</button>
						);
					})}
				</div>
			</div>
		</BottomSheet>
	);
};
