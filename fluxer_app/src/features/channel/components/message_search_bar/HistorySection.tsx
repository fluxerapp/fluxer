// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';
import {FilterOption} from '@app/features/channel/components/message_search_bar/FilterOption';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import type {HistoryFilterRow} from '@app/features/channel/components/message_search_bar/MessageSearchBarTypes';
import type {SearchHistoryEntry} from '@app/features/search/state/SearchHistory';
import {formatSearchHistoryEntryForStreamerMode} from '@app/features/search/utils/SearchPrivacyUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ClockIcon, FunnelIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SEARCH_FILTERS_DESCRIPTOR = msg({
	message: 'Search filters',
	comment: 'Section header in the message search popout listing available filter operators.',
});
const RECENT_SEARCHES_DESCRIPTOR = msg({
	message: 'Recent searches',
	comment: 'Section header in the message search popout listing previously-run searches.',
});
const CLEAR_DESCRIPTOR = msg({
	message: 'Clear',
	comment: 'Inline button in the Recent searches section that wipes the search history list.',
});

interface HistorySectionProps {
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (entry: SearchHistoryEntry) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
	onHistoryClear: () => void;
	onFilterSelect: (row: HistoryFilterRow, index: number) => void;
	onFilterMouseEnter: (index: number) => void;
	onFilterMouseLeave?: () => void;
	filterRows: Array<HistoryFilterRow>;
	historyOptions: Array<SearchHistoryEntry>;
}

export const HistorySection: React.FC<HistorySectionProps> = observer(
	({
		selectedIndex,
		hoverIndex,
		onSelect,
		onMouseEnter,
		onMouseLeave,
		listboxId,
		onHistoryClear,
		onFilterSelect,
		onFilterMouseEnter,
		onFilterMouseLeave,
		filterRows,
		historyOptions,
	}) => {
		const {i18n} = useLingui();
		return (
			<>
				<div className={styles.popoutSection} data-flx="channel.message-search-bar.history-section.popout-section">
					<div
						className={styles.popoutSectionHeader}
						data-flx="channel.message-search-bar.history-section.popout-section-header"
					>
						<span
							className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
							data-flx="channel.message-search-bar.history-section.flex"
						>
							<FunnelIcon
								weight="regular"
								size={remFromPx(12)}
								data-flx="channel.message-search-bar.history-section.funnel-icon"
							/>
							{i18n._(SEARCH_FILTERS_DESCRIPTOR)}
						</span>
					</div>
					{filterRows.map((row: HistoryFilterRow, index) => {
						if (row.kind === 'filter') {
							return (
								<FilterOption
									key={row.option.key}
									option={row.option}
									index={index}
									isSelected={selectedIndex === index}
									isHovered={index === hoverIndex}
									onSelect={() => onFilterSelect(row, index)}
									onMouseEnter={() => onFilterMouseEnter(index)}
									onMouseLeave={onFilterMouseLeave}
									listboxId={listboxId}
									data-flx="channel.message-search-bar.history-section.filter-option.filter-select"
								/>
							);
						}
						return null;
					})}
				</div>
				{historyOptions.length > 0 && (
					<div className={styles.popoutSection} data-flx="channel.message-search-bar.history-section.popout-section--2">
						<div
							className={styles.popoutSectionHeader}
							data-flx="channel.message-search-bar.history-section.popout-section-header--2"
						>
							<span
								className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
								data-flx="channel.message-search-bar.history-section.flex--2"
							>
								<ClockIcon
									weight="regular"
									size={remFromPx(12)}
									data-flx="channel.message-search-bar.history-section.clock-icon"
								/>
								{i18n._(RECENT_SEARCHES_DESCRIPTOR)}
							</span>
							<button
								type="button"
								onClick={(e) => {
									e.preventDefault();
									e.stopPropagation();
									onHistoryClear();
								}}
								className={`${styles.flex} ${styles.itemsCenter} ${styles.gap1}`}
								data-flx="channel.message-search-bar.history-section.flex.prevent-default.button"
							>
								<TrashIcon
									weight="regular"
									size={remFromPx(10)}
									data-flx="channel.message-search-bar.history-section.trash-icon"
								/>
								{i18n._(CLEAR_DESCRIPTOR)}
							</button>
						</div>
						{historyOptions.map((entry: SearchHistoryEntry, index) => {
							const displayEntry = formatSearchHistoryEntryForStreamerMode(entry);
							return (
								<AutocompleteOption
									key={`${entry.query}:${entry.ts}`}
									index={filterRows.length + index}
									isSelected={selectedIndex === filterRows.length + index}
									isHovered={filterRows.length + index === hoverIndex}
									onSelect={() => onSelect(entry)}
									onMouseEnter={() => onMouseEnter(filterRows.length + index)}
									onMouseLeave={onMouseLeave}
									listboxId={listboxId}
									data-flx="channel.message-search-bar.history-section.autocomplete-option.select"
								>
									<div
										className={styles.optionLabel}
										data-flx="channel.message-search-bar.history-section.option-label"
									>
										<div
											className={styles.optionContent}
											data-flx="channel.message-search-bar.history-section.option-content"
										>
											<div
												className={styles.optionText}
												data-flx="channel.message-search-bar.history-section.option-text"
											>
												<span
													className={`${styles.optionTitle} ${styles.historyOptionTitle}`}
													data-flx="channel.message-search-bar.history-section.option-title"
												>
													{displayEntry.query}
												</span>
											</div>
										</div>
									</div>
								</AutocompleteOption>
							);
						})}
					</div>
				)}
			</>
		);
	},
);
