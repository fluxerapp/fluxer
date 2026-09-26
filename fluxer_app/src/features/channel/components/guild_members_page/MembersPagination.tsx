// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import type {
	PaginationEllipsisSide,
	PaginationItem,
} from '@app/features/channel/components/guild_members_page/GuildMembersPageShared';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback} from 'react';

const GO_TO_PAGE_DESCRIPTOR = msg({
	message: 'Go to page {pageItem}',
	comment: 'Accessible label for a pagination button on the community members page.',
});
const GO_TO_PAGE_2_DESCRIPTOR = msg({
	message: 'Go to page',
	comment: 'Accessible label for the jump-to-page input on the community members pagination control.',
});
const JUMP_TO_PAGE_DESCRIPTOR = msg({
	message: 'Jump to page',
	comment: 'Placeholder text in the pagination jump input on the community members page.',
});

export interface MembersPaginationProps {
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

export function MembersPagination({
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
}: MembersPaginationProps) {
	const {i18n} = useLingui();
	const closeJumpInput = useCallback(() => {
		setActiveEllipsis(null);
		setPageJumpValue('');
	}, [setActiveEllipsis, setPageJumpValue]);
	const handleJumpSubmit = useCallback(
		(event: React.SubmitEvent<HTMLFormElement>) => {
			event.preventDefault();
			const nextPage = parseInt(pageJumpValue, 10);
			if (!Number.isNaN(nextPage) && nextPage >= 1 && nextPage <= totalPages) {
				onPageSelect(nextPage);
			}
			closeJumpInput();
		},
		[closeJumpInput, onPageSelect, pageJumpValue, totalPages],
	);
	const openJumpInput = useCallback(
		(side: PaginationEllipsisSide) => {
			setActiveEllipsis(side);
			setPageJumpValue('');
		},
		[setActiveEllipsis, setPageJumpValue],
	);
	return (
		<div className={styles.footerRight} data-flx="channel.guild-members-page.members-pagination.footer-right">
			<div className={styles.paginationBar} data-flx="channel.guild-members-page.members-pagination.pagination-bar">
				<div
					className={styles.paginationWrapper}
					data-flx="channel.guild-members-page.members-pagination.pagination-wrapper"
				>
					{paginationRange.map((pageItem) => {
						if (typeof pageItem === 'number') {
							return (
								<FocusRing
									key={pageItem}
									offset={-2}
									ringClassName={styles.focusRingCircular}
									data-flx="channel.guild-members-page.members-pagination.focus-ring"
								>
									<button
										type="button"
										onClick={() => onPageSelect(pageItem)}
										className={clsx(styles.pageButton, pageItem === currentPage && styles.pageButtonActive)}
										aria-current={pageItem === currentPage ? 'page' : undefined}
										aria-label={i18n._(GO_TO_PAGE_DESCRIPTOR, {pageItem})}
										disabled={isSearching}
										data-flx="channel.guild-members-page.members-pagination.page-button.page-select"
									>
										{pageItem}
									</button>
								</FocusRing>
							);
						}
						const side: PaginationEllipsisSide = pageItem === 'ellipsis-left' ? 'left' : 'right';
						const isActive = activeEllipsis === side;
						if (isActive) {
							return (
								<form
									key={`ellipsis-input-${side}`}
									className={styles.pageInputForm}
									onSubmit={handleJumpSubmit}
									data-flx="channel.guild-members-page.members-pagination.page-input-form.jump-submit"
								>
									<label
										htmlFor={`guild-members-pagination-input-${side}`}
										className={styles.pageInputLabel}
										data-flx="channel.guild-members-page.members-pagination.page-input-label"
									>
										{i18n._(GO_TO_PAGE_2_DESCRIPTOR)}
									</label>
									<FocusRing
										offset={-2}
										ringClassName={styles.focusRingCircular}
										data-flx="channel.guild-members-page.members-pagination.focus-ring--2"
									>
										<input
											id={`guild-members-pagination-input-${side}`}
											ref={ellipsisInputRef}
											type="number"
											data-flx="channel.guild-members-page.members-pagination.page-input.set-page-jump-value.number"
											{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
											min={1}
											max={totalPages}
											inputMode="numeric"
											value={pageJumpValue}
											onChange={(e) => setPageJumpValue(e.target.value)}
											onBlur={closeJumpInput}
											className={styles.pageInput}
											placeholder={side === 'left' ? '1' : totalPages.toString()}
										/>
									</FocusRing>
								</form>
							);
						}
						return (
							<FocusRing
								key={`ellipsis-${side}`}
								offset={-2}
								ringClassName={styles.focusRingCircular}
								data-flx="channel.guild-members-page.members-pagination.focus-ring--3"
							>
								<button
									type="button"
									onClick={() => openJumpInput(side)}
									className={styles.ellipsisButton}
									aria-label={i18n._(JUMP_TO_PAGE_DESCRIPTOR)}
									disabled={isSearching}
									data-flx="channel.guild-members-page.members-pagination.ellipsis-button.open-jump-input"
								>
									&hellip;
								</button>
							</FocusRing>
						);
					})}
				</div>
			</div>
		</div>
	);
}
