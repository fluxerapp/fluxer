// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelSearchResults.module.css';
import {
	type ChannelSearchSortMode,
	getHeaderTitleDescriptor,
	renderScopeIcon,
	renderSortIcon,
	SEARCH_SCOPE_DESCRIPTOR,
	SORT_MODE_DESCRIPTOR,
} from '@app/features/channel/components/channel_search_results/ChannelSearchResultsShared';
import type {SearchMachineState} from '@app/features/channel/components/SearchResultsUtils';
import {DEFAULT_SCOPE_VALUE} from '@app/features/channel/components/SearchScopeOptions';
import type {MessageSearchScope} from '@app/features/search/utils/SearchUtils';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

interface SearchResultsHeaderProps {
	machineState: SearchMachineState;
	scopeOptions: Array<{value: MessageSearchScope; label: string}>;
	activeScope: MessageSearchScope | null;
	sortModeOptions: Array<{mode: ChannelSearchSortMode; label: string}>;
	sortMode: ChannelSearchSortMode;
	onScopeMenuOpen: (event: React.MouseEvent<HTMLButtonElement>) => void;
	onSortMenuOpen: (event: React.MouseEvent<HTMLButtonElement>) => void;
}

export const SearchResultsHeader: React.FC<SearchResultsHeaderProps> = ({
	machineState,
	scopeOptions,
	activeScope,
	sortModeOptions,
	sortMode,
	onScopeMenuOpen,
	onSortMenuOpen,
}) => {
	const {i18n} = useLingui();
	const showSpinner = machineState.status === 'loading' || machineState.status === 'indexing';
	const headerTitleDescriptor = getHeaderTitleDescriptor(machineState);
	const activeScopeOption = scopeOptions.find((option) => option.value === activeScope) ?? scopeOptions[0];
	const activeSortOption = sortModeOptions.find((option) => option.mode === sortMode) ?? sortModeOptions[0];
	return (
		<div className={styles.header} data-flx="channel.channel-search-results.render-header.header">
			<div className={styles.headerLoading} data-flx="channel.channel-search-results.render-header.header-loading">
				{showSpinner && <Spinner size="small" data-flx="channel.channel-search-results.render-header.spinner" />}
				<h2
					className={styles.headerTitle}
					role="status"
					aria-live="polite"
					aria-atomic="true"
					data-flx="channel.channel-search-results.render-header.header-title"
				>
					{i18n._(headerTitleDescriptor)}
				</h2>
			</div>
			{!showSpinner && (
				<div className={styles.headerActions} data-flx="channel.channel-search-results.render-header.header-actions">
					<Button
						type="button"
						variant="secondary"
						square
						compact
						fitContent
						className={styles.scopeButton}
						icon={renderScopeIcon(activeScope ?? DEFAULT_SCOPE_VALUE, 18)}
						onClick={onScopeMenuOpen}
						aria-label={i18n._(SEARCH_SCOPE_DESCRIPTOR, {label: activeScopeOption.label})}
						data-flx="channel.channel-search-results.render-header.scope-button.scope-menu-open"
					/>
					<Button
						type="button"
						variant="secondary"
						square
						compact
						fitContent
						className={styles.sortButton}
						icon={renderSortIcon(activeSortOption.mode, 18)}
						onClick={onSortMenuOpen}
						aria-label={i18n._(SORT_MODE_DESCRIPTOR, {label: activeSortOption.label})}
						data-flx="channel.channel-search-results.render-header.sort-button.sort-menu-open"
					/>
				</div>
			)}
		</div>
	);
};
