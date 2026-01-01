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
import {msg, t} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type {IconProps} from '@phosphor-icons/react';
import {
	BrowserIcon,
	CheckIcon,
	FileIcon,
	ImageIcon,
	LinkIcon,
	MusicNoteIcon,
	StickerIcon,
	VideoIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import styles from './HasFilterSheet.module.css';

export type HasFilterType = 'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link';

interface HasFilterOption {
	type: HasFilterType;
	label: MessageDescriptor;
	icon: React.ComponentType<IconProps>;
}

const HAS_FILTER_OPTIONS: Array<HasFilterOption> = [
	{type: 'image', label: msg`Image`, icon: ImageIcon},
	{type: 'video', label: msg`Video`, icon: VideoIcon},
	{type: 'sound', label: msg`Sound`, icon: MusicNoteIcon},
	{type: 'file', label: msg`File`, icon: FileIcon},
	{type: 'link', label: msg`Link`, icon: LinkIcon},
	{type: 'embed', label: msg`Embed`, icon: BrowserIcon},
	{type: 'sticker', label: msg`Sticker`, icon: StickerIcon},
];

interface HasFilterSheetProps {
	isOpen: boolean;
	onClose: () => void;
	selectedFilters: Array<HasFilterType>;
	onFiltersChange: (filters: Array<HasFilterType>) => void;
}

export const HasFilterSheet: React.FC<HasFilterSheetProps> = ({isOpen, onClose, selectedFilters, onFiltersChange}) => {
	const {i18n} = useLingui();

	const toggleFilter = (type: HasFilterType) => {
		if (selectedFilters.includes(type)) {
			onFiltersChange(selectedFilters.filter((f) => f !== type));
		} else {
			onFiltersChange([...selectedFilters, type]);
		}
	};

	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			title={t(i18n)`Filter by content`}
			disablePadding
		>
			<div className={styles.container}>
				<p className={styles.subtitle}>
					<Trans>Show messages that contain:</Trans>
				</p>

				<Scroller className={styles.scroller} fade={false}>
					<div className={styles.optionsContainer}>
						{HAS_FILTER_OPTIONS.map((option) => {
							const isSelected = selectedFilters.includes(option.type);
							const Icon = option.icon;

							return (
								<button
									key={option.type}
									type="button"
									className={clsx(styles.option, isSelected && styles.optionSelected)}
									onClick={() => toggleFilter(option.type)}
								>
									<div className={styles.optionLeft}>
										<Icon
											size={22}
											className={clsx(styles.optionIcon, isSelected && styles.optionIconSelected)}
											weight="regular"
										/>
										<span className={clsx(styles.optionLabel, isSelected && styles.optionLabelSelected)}>
											{i18n._(option.label)}
										</span>
									</div>

									{isSelected && <CheckIcon size={20} className={styles.checkIcon} weight="bold" />}
								</button>
							);
						})}
					</div>
				</Scroller>

				<div className={styles.footer}>
					<Button variant="primary" onClick={onClose}>
						<Trans>Done</Trans>
					</Button>
				</div>
			</div>
		</BottomSheet>
	);
};
