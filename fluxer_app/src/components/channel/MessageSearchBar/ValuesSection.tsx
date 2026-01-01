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
import {FunnelIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {SearchValueOption} from '~/utils/SearchUtils';
import {AutocompleteOption} from './AutocompleteOption';
import styles from './MessageSearchBar.module.css';

interface ValuesSectionProps {
	options: Array<SearchValueOption>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (value: SearchValueOption) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
}

export const ValuesSection: React.FC<ValuesSectionProps> = observer(
	({options, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId}) => {
		const {t} = useLingui();

		if (options.length === 0) return null;

		return (
			<div className={styles.popoutSection}>
				<div className={styles.popoutSectionHeader}>
					<span className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}>
						<FunnelIcon weight="regular" size={14} />
						{t`Values`}
					</span>
				</div>
				{options.map((valueOption, index) => (
					<AutocompleteOption
						key={valueOption.value}
						index={index}
						isSelected={index === selectedIndex}
						isHovered={index === hoverIndex}
						onSelect={() => onSelect(valueOption)}
						onMouseEnter={() => onMouseEnter(index)}
						onMouseLeave={onMouseLeave}
						listboxId={listboxId}
					>
						<div className={styles.optionLabel}>
							<div className={`${styles.optionContent} ${styles.valueOptionContent}`}>
								<div className={styles.valueOptionText}>
									<div className={styles.valueOptionTitle}>
										<span className={styles.searchFilter}>{valueOption.label}</span>
										{valueOption.isDefault && <span className={styles.valueOptionDefault}>{t`Default`}</span>}
									</div>
									{valueOption.description && (
										<span className={styles.optionDescription}>{valueOption.description}</span>
									)}
								</div>
							</div>
						</div>
					</AutocompleteOption>
				))}
			</div>
		);
	},
);
