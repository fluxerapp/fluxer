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
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {DateTime} from 'luxon';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {AutocompleteOption} from './AutocompleteOption';
import styles from './MessageSearchBar.module.css';

interface DateSectionProps {
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (dateValue: string) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
}

export const DateSection: React.FC<DateSectionProps> = observer(
	({selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId}) => {
		const {t} = useLingui();

		const now = DateTime.local();
		const fmt = (dt: DateTime) => dt.toFormat('yyyy-MM-dd');
		const fmtTime = (dt: DateTime) => dt.toFormat("yyyy-MM-dd'T'HH:mm");

		const options = [
			{label: 'Today', value: fmt(now)},
			{label: 'Yesterday', value: fmt(now.minus({days: 1}))},
			{label: 'Now', value: fmtTime(now)},
		];

		return (
			<div className={styles.popoutSection}>
				<div className={styles.popoutSectionHeader}>
					<span className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}>
						<MagnifyingGlassIcon weight="regular" size={14} />
						{t`Date Options`}
					</span>
				</div>
				{options.map((opt: {label: string; value: string}, index) => (
					<AutocompleteOption
						key={opt.label}
						index={index}
						isSelected={index === selectedIndex}
						isHovered={index === hoverIndex}
						onSelect={() => onSelect(opt.value)}
						onMouseEnter={() => onMouseEnter(index)}
						onMouseLeave={onMouseLeave}
						listboxId={listboxId}
					>
						<div className={styles.optionLabel}>
							<div className={styles.optionContent}>
								<div className={styles.optionText}>
									<div className={styles.optionTitle}>{opt.label}</div>
									<div className={styles.optionDescription}>{opt.value}</div>
								</div>
							</div>
						</div>
					</AutocompleteOption>
				))}
			</div>
		);
	},
);
