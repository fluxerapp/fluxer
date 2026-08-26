// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';
import {FilterOption} from '@app/features/channel/components/message_search_bar/FilterOption';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import type {PlaintextAutocompleteRow} from '@app/features/channel/components/message_search_bar/MessageSearchBarTypes';
import {resolveChannelSuggestionDisplayName} from '@app/features/channel/components/message_search_bar/MessageSearchBarUtils';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FunnelIcon, MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SEARCH_FILTERS_DESCRIPTOR = msg({
	message: 'Search filters',
	comment: 'Section header in the message search popout listing available filter operators.',
});

interface PlaintextSectionProps {
	rows: Array<PlaintextAutocompleteRow>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (row: PlaintextAutocompleteRow) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
}

const resolveRowGroup = (row: PlaintextAutocompleteRow): string => {
	switch (row.kind) {
		case 'filter-key':
			return 'filter-key';
		default:
			return row.filterKey;
	}
};

interface PlaintextRowGroup {
	group: string;
	entries: Array<{row: PlaintextAutocompleteRow; index: number}>;
}

const groupConsecutiveRows = (rows: Array<PlaintextAutocompleteRow>): Array<PlaintextRowGroup> => {
	const groups: Array<PlaintextRowGroup> = [];
	rows.forEach((row, index) => {
		const group = resolveRowGroup(row);
		const current = groups.at(-1);
		if (current != null && current.group === group) {
			current.entries.push({row, index});
			return;
		}
		groups.push({group, entries: [{row, index}]});
	});
	return groups;
};

export const PlaintextSection: React.FC<PlaintextSectionProps> = observer(
	({rows, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId}) => {
		const {i18n} = useLingui();
		if (rows.length === 0) return null;
		return (
			<>
				{groupConsecutiveRows(rows).map(({group, entries}) => (
					<div
						key={group}
						className={styles.popoutSection}
						data-flx="channel.message-search-bar.plaintext-section.popout-section"
					>
						<div
							className={styles.popoutSectionHeader}
							data-flx="channel.message-search-bar.plaintext-section.popout-section-header"
						>
							<span
								className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
								data-flx="channel.message-search-bar.plaintext-section.flex"
							>
								{group === 'filter-key' ? (
									<FunnelIcon
										weight="regular"
										size={remFromPx(12)}
										data-flx="channel.message-search-bar.plaintext-section.funnel-icon"
									/>
								) : (
									<MagnifyingGlassIcon
										weight="regular"
										size={remFromPx(14)}
										data-flx="channel.message-search-bar.plaintext-section.magnifying-glass-icon"
									/>
								)}
								{group === 'filter-key' ? i18n._(SEARCH_FILTERS_DESCRIPTOR) : `${group}:`}
							</span>
						</div>
						{entries.map(({row, index}) => {
							if (row.kind === 'filter-key') {
								return (
									<FilterOption
										key={`filter-${row.filter.key}`}
										option={row.filter}
										index={index}
										isSelected={selectedIndex === index}
										isHovered={hoverIndex === index}
										onSelect={() => onSelect(row)}
										onMouseEnter={() => onMouseEnter(index)}
										onMouseLeave={onMouseLeave}
										listboxId={listboxId}
										data-flx="channel.message-search-bar.plaintext-section.filter-option.select"
									/>
								);
							}
							const key =
								row.kind === 'user-suggestion'
									? `${row.filterKey}-user-${row.user.id}`
									: `${row.filterKey}-channel-${row.channel.id}`;
							const content =
								row.kind === 'user-suggestion' ? (
									<span
										className={`${styles.userRow} ${styles.gap2}`}
										data-flx="channel.message-search-bar.plaintext-section.user-row"
									>
										<StatusAwareAvatar
											user={row.user}
											size={16}
											data-flx="channel.message-search-bar.plaintext-section.status-aware-avatar"
										/>
										<span
											className={`${styles.minW0} ${styles.overflowHidden}`}
											data-flx="channel.message-search-bar.plaintext-section.min-w0"
										>
											{NicknameUtils.formatUserTagForStreamerMode(row.user)}
										</span>
									</span>
								) : (
									<span
										className={styles.channelRow}
										data-flx="channel.message-search-bar.plaintext-section.channel-row"
									>
										{ChannelUtils.getIcon(row.channel, {className: styles.channelIcon})}
										<span
											className={styles.channelName}
											data-flx="channel.message-search-bar.plaintext-section.channel-name"
										>
											{resolveChannelSuggestionDisplayName(row.channel)}
										</span>
									</span>
								);
							return (
								<AutocompleteOption
									key={key}
									index={index}
									isSelected={selectedIndex === index}
									isHovered={hoverIndex === index}
									onSelect={() => onSelect(row)}
									onMouseEnter={() => onMouseEnter(index)}
									onMouseLeave={onMouseLeave}
									listboxId={listboxId}
									data-flx="channel.message-search-bar.plaintext-section.autocomplete-option.select"
								>
									<div
										className={styles.optionLabel}
										data-flx="channel.message-search-bar.plaintext-section.option-label"
									>
										<div
											className={styles.optionContent}
											data-flx="channel.message-search-bar.plaintext-section.option-content"
										>
											{content}
										</div>
									</div>
								</AutocompleteOption>
							);
						})}
					</div>
				))}
			</>
		);
	},
);
