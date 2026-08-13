// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DATE_OPTIONS_DESCRIPTOR = msg({
	message: 'Date options',
	comment: 'Section header in the message search popout listing date presets and the custom date input.',
});

export interface DateSectionOption {
	label: string;
	value: string;
}

interface DateSectionProps {
	options: Array<DateSectionOption>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (option: DateSectionOption) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
}

export const DateSection: React.FC<DateSectionProps> = observer(
	({options, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId}) => {
		const {i18n} = useLingui();
		if (options.length === 0) return null;
		return (
			<div className={styles.popoutSection} data-flx="channel.message-search-bar.date-section.popout-section">
				<div
					className={styles.popoutSectionHeader}
					data-flx="channel.message-search-bar.date-section.popout-section-header"
				>
					<span
						className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
						data-flx="channel.message-search-bar.date-section.flex"
					>
						<MagnifyingGlassIcon
							weight="regular"
							size={remFromPx(14)}
							data-flx="channel.message-search-bar.date-section.magnifying-glass-icon"
						/>
						{i18n._(DATE_OPTIONS_DESCRIPTOR)}
					</span>
				</div>
				{options.map((opt, index) => (
					<AutocompleteOption
						key={opt.value}
						index={index}
						isSelected={index === selectedIndex}
						isHovered={index === hoverIndex}
						onSelect={() => onSelect(opt)}
						onMouseEnter={() => onMouseEnter(index)}
						onMouseLeave={onMouseLeave}
						listboxId={listboxId}
						data-flx="channel.message-search-bar.date-section.autocomplete-option.select"
					>
						<div className={styles.optionLabel} data-flx="channel.message-search-bar.date-section.option-label">
							<div className={styles.optionContent} data-flx="channel.message-search-bar.date-section.option-content">
								<div className={styles.optionText} data-flx="channel.message-search-bar.date-section.option-text">
									<div className={styles.optionTitle} data-flx="channel.message-search-bar.date-section.option-title">
										{opt.label}
									</div>
									<div
										className={styles.optionDescription}
										data-flx="channel.message-search-bar.date-section.option-description"
									>
										{opt.value}
									</div>
								</div>
							</div>
						</div>
					</AutocompleteOption>
				))}
			</div>
		);
	},
);
