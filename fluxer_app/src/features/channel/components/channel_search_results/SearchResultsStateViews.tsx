// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelSearchResults.module.css';
import {
	ERROR_DESCRIPTOR,
	INDEXING_CHANNEL_DESCRIPTOR,
	NO_PART_OF_THIS_QUERY_APPLIED_DESCRIPTOR,
	NO_RESULTS_DESCRIPTOR,
	NOTHING_TO_SEARCH_FOR_DESCRIPTOR,
	TRY_A_DIFFERENT_SEARCH_QUERY_DESCRIPTOR,
	WE_RE_INDEXING_THIS_CHANNEL_FOR_THE_FIRST_DESCRIPTOR,
} from '@app/features/channel/components/channel_search_results/ChannelSearchResultsShared';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {useLingui} from '@lingui/react/macro';
import {CircleNotchIcon, MagnifyingGlassIcon, WarningCircleIcon} from '@phosphor-icons/react';
import type React from 'react';

export const SearchIndexingState: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<div className={styles.loadingState} data-flx="channel.channel-search-results.render-content.loading-state">
			<CircleNotchIcon
				className={styles.loadingIcon}
				data-flx="channel.channel-search-results.render-content.loading-icon"
			/>
			<div className={styles.loadingContent} data-flx="channel.channel-search-results.render-content.loading-content">
				<h3 className={styles.loadingHeading} data-flx="channel.channel-search-results.render-content.loading-heading">
					{i18n._(INDEXING_CHANNEL_DESCRIPTOR)}
				</h3>
				<p className={styles.loadingText} data-flx="channel.channel-search-results.render-content.loading-text">
					{i18n._(WE_RE_INDEXING_THIS_CHANNEL_FOR_THE_FIRST_DESCRIPTOR)}
				</p>
			</div>
		</div>
	);
};

interface SearchErrorStateProps {
	error: string;
	onRetry: () => void;
}

export const SearchErrorState: React.FC<SearchErrorStateProps> = ({error, onRetry}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.errorState} data-flx="channel.channel-search-results.render-content.error-state">
			<div className={styles.errorContent} data-flx="channel.channel-search-results.render-content.error-content">
				<h3 className={styles.errorHeading} data-flx="channel.channel-search-results.render-content.error-heading">
					{i18n._(ERROR_DESCRIPTOR)}
				</h3>
				<p className={styles.errorText} data-flx="channel.channel-search-results.render-content.error-text">
					{error}
				</p>
				<Button
					variant="secondary"
					small
					onClick={onRetry}
					data-flx="channel.channel-search-results.render-content.button.perform-search"
				>
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
};
export const SearchEmptyState: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<div className={styles.emptyState} data-flx="channel.channel-search-results.render-content.empty-state">
			<div
				className={styles.emptyStateContent}
				data-flx="channel.channel-search-results.render-content.empty-state-content"
			>
				<MagnifyingGlassIcon
					className={styles.emptyStateIcon}
					data-flx="channel.channel-search-results.render-content.empty-state-icon"
				/>
				<div
					className={styles.emptyStateTextWrapper}
					data-flx="channel.channel-search-results.render-content.empty-state-text-wrapper"
				>
					<h3
						className={styles.emptyStateHeading}
						data-flx="channel.channel-search-results.render-content.empty-state-heading"
					>
						{i18n._(NO_RESULTS_DESCRIPTOR)}
					</h3>
					<p
						className={styles.emptyStateText}
						data-flx="channel.channel-search-results.render-content.empty-state-text"
					>
						{i18n._(TRY_A_DIFFERENT_SEARCH_QUERY_DESCRIPTOR)}
					</p>
				</div>
			</div>
		</div>
	);
};

interface SearchUnappliedQueryStateProps {
	query: string;
}

export const SearchUnappliedQueryState: React.FC<SearchUnappliedQueryStateProps> = ({query}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.emptyState} data-flx="channel.channel-search-results.render-content.unapplied-state">
			<div
				className={styles.emptyStateContent}
				data-flx="channel.channel-search-results.render-content.unapplied-state-content"
			>
				<WarningCircleIcon
					className={styles.emptyStateIcon}
					data-flx="channel.channel-search-results.render-content.unapplied-state-icon"
				/>
				<div
					className={styles.emptyStateTextWrapper}
					data-flx="channel.channel-search-results.render-content.unapplied-state-text-wrapper"
				>
					<h3
						className={styles.emptyStateHeading}
						role="status"
						aria-live="polite"
						data-flx="channel.channel-search-results.render-content.unapplied-state-heading"
					>
						{i18n._(NOTHING_TO_SEARCH_FOR_DESCRIPTOR)}
					</h3>
					<p
						className={styles.emptyStateText}
						data-flx="channel.channel-search-results.render-content.unapplied-state-text"
					>
						{i18n._(NO_PART_OF_THIS_QUERY_APPLIED_DESCRIPTOR, {query})}
					</p>
				</div>
			</div>
		</div>
	);
};
