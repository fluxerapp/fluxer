// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, SortAscendingIcon} from '@phosphor-icons/react';
import type React from 'react';

const SEARCH_BY_USERNAME_OR_ID_DESCRIPTOR = msg({
	message: 'Search by username or ID',
	comment: 'Placeholder text in the search input on the community members page.',
});
const SORT_DESCRIPTOR = msg({
	message: 'Sort',
	comment: 'Accessible label for the sort-mode select on the community members page.',
});

export interface MembersTableToolbarProps {
	displayedCount: number;
	totalCount: number;
	showFooter: boolean;
	inputValue: string;
	onInputChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
	onSortMenuOpen: (event: React.MouseEvent<HTMLButtonElement>) => void;
	indexing: boolean;
}

export function MembersTableToolbar({
	displayedCount,
	totalCount,
	showFooter,
	inputValue,
	onInputChange,
	onSortMenuOpen,
	indexing,
}: MembersTableToolbarProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.toolbar} data-flx="channel.guild-members-page.members-table-view.toolbar">
			<div className={styles.toolbarLeft} data-flx="channel.guild-members-page.members-table-view.toolbar-left">
				<h2 className={styles.toolbarTitle} data-flx="channel.guild-members-page.members-table-view.toolbar-title">
					<Trans>Recent members</Trans>
				</h2>
				{showFooter && (
					<span
						className={styles.toolbarSubtitle}
						data-flx="channel.guild-members-page.members-table-view.toolbar-subtitle"
					>
						<Trans>
							Showing {displayedCount} of {totalCount} total members
						</Trans>
					</span>
				)}
			</div>
			<div className={styles.toolbarRight} data-flx="channel.guild-members-page.members-table-view.toolbar-right">
				<Input
					type="text"
					placeholder={i18n._(SEARCH_BY_USERNAME_OR_ID_DESCRIPTOR)}
					value={inputValue}
					onChange={onInputChange}
					disabled={indexing}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="channel.guild-members-page.members-table-view.magnifying-glass-icon"
						/>
					}
					className={styles.searchInput}
					data-flx="channel.guild-members-page.members-table-view.search-input.input-change.text"
				/>
				<Button
					variant="secondary"
					leftIcon={
						<SortAscendingIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="channel.guild-members-page.members-table-view.sort-ascending-icon"
						/>
					}
					className={styles.sortButton}
					onClick={onSortMenuOpen}
					disabled={indexing}
					aria-haspopup="menu"
					data-flx="channel.guild-members-page.members-table-view.sort-button.sort-menu-open"
				>
					{i18n._(SORT_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
}
