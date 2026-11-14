// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import {
	MAX_VISIBLE_PAGES,
	PAGE_SIZE_OPTIONS,
} from '@app/features/channel/components/guild_members_page/GuildMembersPageShared';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Pagination} from '@app/features/ui/components/Pagination';
import {Trans} from '@lingui/react/macro';

export interface MembersTableFooterProps {
	pageSize: number;
	onPageSizeChange: (value: number) => void;
	showPagination: boolean;
	currentPage: number;
	totalPages: number;
	isSearching: boolean;
	onPageSelect: (page: number) => void;
}

export function MembersTableFooter({
	pageSize,
	onPageSizeChange,
	showPagination,
	currentPage,
	totalPages,
	isSearching,
	onPageSelect,
}: MembersTableFooterProps) {
	return (
		<div className={styles.footer} data-flx="channel.guild-members-page.members-table-view.footer">
			<div className={styles.footerLeft} data-flx="channel.guild-members-page.members-table-view.footer-left">
				<span className={styles.footerLabel} data-flx="channel.guild-members-page.members-table-view.footer-label">
					<Trans>Rows per page</Trans>
				</span>
				<div
					className={styles.pageSizeSelect}
					data-flx="channel.guild-members-page.members-table-view.page-size-select"
				>
					<Combobox<number>
						value={pageSize}
						options={PAGE_SIZE_OPTIONS}
						onChange={onPageSizeChange}
						data-flx="channel.guild-members-page.members-table-view.select.page-size-change"
					/>
				</div>
			</div>
			{showPagination && (
				<Pagination
					currentPage={currentPage}
					visiblePageSlots={MAX_VISIBLE_PAGES}
					className={styles.paginationBar}
					totalPages={totalPages}
					disabled={isSearching}
					onJumpToPage={onPageSelect}
					data-flx="channel.guild-members-page.members-table-view.members-pagination"
				/>
			)}
		</div>
	);
}
