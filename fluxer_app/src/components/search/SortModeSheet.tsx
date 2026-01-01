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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {IconProps} from '@phosphor-icons/react';
import {CheckIcon, ClockClockwiseIcon, ClockCounterClockwiseIcon, SparkleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import type {ChannelSearchSortMode} from '~/hooks/useChannelSearch';
import styles from './SortModeSheet.module.css';

interface SortOption {
	mode: ChannelSearchSortMode;
	label: MessageDescriptor;
	description: MessageDescriptor;
	icon: React.ComponentType<IconProps>;
}

const SORT_OPTIONS: Array<SortOption> = [
	{
		mode: 'newest',
		label: msg`Newest First`,
		description: msg`Show most recent messages first`,
		icon: ClockClockwiseIcon,
	},
	{
		mode: 'oldest',
		label: msg`Oldest First`,
		description: msg`Show oldest messages first`,
		icon: ClockCounterClockwiseIcon,
	},
	{
		mode: 'relevant',
		label: msg`Most Relevant`,
		description: msg`Show most relevant messages first`,
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
			title={i18n._(msg`Sort results by`)}
			disablePadding
		>
			<div className={styles.container}>
				<div className={styles.optionsContainer}>
					{SORT_OPTIONS.map((option) => {
						const isSelected = selectedMode === option.mode;
						const Icon = option.icon;

						return (
							<button
								key={option.mode}
								type="button"
								className={clsx(styles.option, isSelected && styles.optionSelected)}
								onClick={() => handleSelect(option.mode)}
							>
								<div className={styles.optionLeft}>
									<Icon
										size={22}
										className={clsx(styles.optionIcon, isSelected && styles.optionIconSelected)}
										weight="regular"
									/>
									<div className={styles.optionText}>
										<span className={clsx(styles.optionLabel, isSelected && styles.optionLabelSelected)}>
											{i18n._(option.label)}
										</span>
										<span className={styles.optionDescription}>{i18n._(option.description)}</span>
									</div>
								</div>
								{isSelected && <CheckIcon size={20} className={styles.checkIcon} weight="bold" />}
							</button>
						);
					})}
				</div>
			</div>
		</BottomSheet>
	);
};
