// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import {
	PAGE_SIZE_OPTIONS,
	type PaginationEllipsisSide,
	type PaginationItem,
} from '@app/features/channel/components/guild_members_page/GuildMembersPageShared';
import {MembersPagination} from '@app/features/channel/components/guild_members_page/MembersPagination';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Trans} from '@lingui/react/macro';
import type React from 'react';

export interface MembersTableFooterProps {
	pageSize: number;
	onPageSizeChange: (value: number) => void;
	showPagination: boolean;
	paginationRange: ReadonlyArray<PaginationItem>;
	currentPage: number;
	totalPages: number;
	isSearching: boolean;
	activeEllipsis: PaginationEllipsisSide | null;
	pageJumpValue: string;
	ellipsisInputRef: React.RefObject<HTMLInputElement | null>;
	onPageSelect: (page: number) => void;
	setActiveEllipsis: React.Dispatch<React.SetStateAction<PaginationEllipsisSide | null>>;
	setPageJumpValue: React.Dispatch<React.SetStateAction<string>>;
}

export function MembersTableFooter({
	pageSize,
	onPageSizeChange,
	showPagination,
	paginationRange,
	currentPage,
	totalPages,
	isSearching,
	activeEllipsis,
	pageJumpValue,
	ellipsisInputRef,
	onPageSelect,
	setActiveEllipsis,
	setPageJumpValue,
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
				<MembersPagination
					paginationRange={paginationRange}
					currentPage={currentPage}
					totalPages={totalPages}
					isSearching={isSearching}
					activeEllipsis={activeEllipsis}
					pageJumpValue={pageJumpValue}
					ellipsisInputRef={ellipsisInputRef}
					onPageSelect={onPageSelect}
					setActiveEllipsis={setActiveEllipsis}
					setPageJumpValue={setPageJumpValue}
					data-flx="channel.guild-members-page.members-table-view.members-pagination"
				/>
			)}
		</div>
	);
}
