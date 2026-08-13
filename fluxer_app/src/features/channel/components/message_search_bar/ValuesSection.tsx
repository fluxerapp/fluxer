// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import type {SearchValueOption} from '@app/features/search/utils/SearchUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FunnelIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const VALUES_DESCRIPTOR = msg({
	message: 'Values',
	comment: 'Section header in the message search popout listing the values for the currently-typed filter. Title Case.',
});
const DEFAULT_DESCRIPTOR = msg({
	message: 'Default',
	comment: 'Pill rendered next to the default value of a filter (e.g. sort:timestamp). Title Case.',
});

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
		const {i18n} = useLingui();
		if (options.length === 0) return null;
		return (
			<div className={styles.popoutSection} data-flx="channel.message-search-bar.values-section.popout-section">
				<div
					className={styles.popoutSectionHeader}
					data-flx="channel.message-search-bar.values-section.popout-section-header"
				>
					<span
						className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
						data-flx="channel.message-search-bar.values-section.flex"
					>
						<FunnelIcon
							weight="regular"
							size={remFromPx(14)}
							data-flx="channel.message-search-bar.values-section.funnel-icon"
						/>
						{i18n._(VALUES_DESCRIPTOR)}
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
						data-flx="channel.message-search-bar.values-section.autocomplete-option.select"
					>
						<div className={styles.optionLabel} data-flx="channel.message-search-bar.values-section.option-label">
							<div
								className={`${styles.optionContent} ${styles.valueOptionContent}`}
								data-flx="channel.message-search-bar.values-section.option-content"
							>
								<div
									className={styles.valueOptionText}
									data-flx="channel.message-search-bar.values-section.value-option-text"
								>
									<div
										className={styles.valueOptionTitle}
										data-flx="channel.message-search-bar.values-section.value-option-title"
									>
										<span
											className={styles.searchFilter}
											data-flx="channel.message-search-bar.values-section.search-filter"
										>
											{valueOption.label}
										</span>
										{valueOption.isDefault && (
											<span
												className={styles.valueOptionDefault}
												data-flx="channel.message-search-bar.values-section.value-option-default"
											>
												{i18n._(DEFAULT_DESCRIPTOR)}
											</span>
										)}
									</div>
									{valueOption.description && (
										<span
											className={styles.optionDescription}
											data-flx="channel.message-search-bar.values-section.option-description"
										>
											{valueOption.description}
										</span>
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
