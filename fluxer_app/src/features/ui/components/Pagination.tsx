// SPDX-License-Identifier: AGPL-3.0-or-later

import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import styles from '@app/features/ui/components/Pagination.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

export type PaginationItem = number | 'ellipsis-left' | 'ellipsis-right';

export const GO_TO_PAGE_DESCRIPTOR = msg({
	message: 'Go to page {page}',
	comment: 'Accessible label of each pagination page button. page is the 1-indexed page number.',
});

export const GO_TO_PAGE_2_DESCRIPTOR = msg({
	message: 'Go to page',
	comment: 'Accessible label of the inline page-number input shown when the user opens the pagination ellipsis.',
});

export const JUMP_TO_PAGE_DESCRIPTOR = msg({
	message: 'Jump to page',
	comment: 'Accessible label of the ellipsis button in the pagination bar that reveals the page-number input.',
});

export const buildPaginationRange = (
	currentPage: number,
	totalPages: number,
	maxVisible: number,
): Array<PaginationItem> => {
	if (totalPages <= 0) return [];
	const effectiveMax = Math.max(3, maxVisible);
	if (totalPages <= effectiveMax) {
		return Array.from({length: totalPages}, (_, index) => index + 1);
	}
	const innerSlots = Math.max(1, effectiveMax - 2);
	let start = currentPage - Math.floor(innerSlots / 2);
	let end = currentPage + Math.ceil(innerSlots / 2) - 1;
	start = Math.max(2, start);
	end = Math.min(totalPages - 1, end);
	while (end - start + 1 < innerSlots) {
		if (start > 2) {
			start -= 1;
		} else if (end < totalPages - 1) {
			end += 1;
		} else {
			break;
		}
	}
	const range: Array<PaginationItem> = [1];
	if (start > 2) range.push('ellipsis-left');
	for (let page = start; page <= end; page++) {
		range.push(page);
	}
	if (end < totalPages - 1) range.push('ellipsis-right');
	range.push(totalPages);
	return range;
};

interface PaginationProps {
	currentPage: number;
	totalPages: number;
	visiblePageSlots: number;
	onJumpToPage: (page: number) => void;
	disabled?: boolean;
	className?: string;
}

export const Pagination: React.FC<PaginationProps> = ({
	currentPage,
	totalPages,
	visiblePageSlots,
	onJumpToPage,
	disabled = false,
	className,
}) => {
	const {i18n} = useLingui();
	const [pageJumpValue, setPageJumpValue] = useState('');
	const [activeEllipsis, setActiveEllipsis] = useState<'left' | 'right' | null>(null);
	const ellipsisInputRef = useRef<HTMLInputElement | null>(null);
	useEffect(() => {
		setPageJumpValue('');
		setActiveEllipsis(null);
	}, [currentPage, totalPages]);
	useEffect(() => {
		if (activeEllipsis && ellipsisInputRef.current) {
			ellipsisInputRef.current.focus();
			ellipsisInputRef.current.select();
		}
	}, [activeEllipsis]);
	if (totalPages <= 1) return null;
	const paginationRange = buildPaginationRange(currentPage, totalPages, visiblePageSlots);
	return (
		<div
			className={clsx(styles.paginationBar, className)}
			data-flx="channel.channel-search-results.render-content.pagination-bar"
		>
			<div
				className={styles.paginationWrapper}
				data-flx="channel.channel-search-results.render-content.pagination-wrapper"
			>
				{paginationRange.map((page) => {
					if (typeof page === 'number') {
						return (
							<FocusRing
								key={page}
								offset={-2}
								ringClassName={styles.focusRingCircular}
								data-flx="channel.channel-search-results.render-content.focus-ring"
							>
								<button
									type="button"
									disabled={disabled}
									onClick={() => {
										if (page !== currentPage) {
											onJumpToPage(page);
											setPageJumpValue('');
											setActiveEllipsis(null);
										}
									}}
									className={clsx(styles.pageButton, page === currentPage && styles.pageButtonActive)}
									aria-current={page === currentPage ? 'page' : undefined}
									aria-label={i18n._(GO_TO_PAGE_DESCRIPTOR, {page})}
									data-flx="channel.channel-search-results.render-content.page-button"
								>
									{page}
								</button>
							</FocusRing>
						);
					}
					const side = page === 'ellipsis-left' ? 'left' : 'right';
					const isActive = activeEllipsis === side;
					if (isActive) {
						return (
							<form
								key={`ellipsis-input-${side}`}
								className={styles.pageInputForm}
								onSubmit={(e) => {
									e.preventDefault();
									const nextPage = parseInt(pageJumpValue, 10);
									if (!Number.isNaN(nextPage) && nextPage >= 1 && nextPage <= totalPages) {
										if (nextPage !== currentPage) {
											onJumpToPage(nextPage);
										}
									}
									setPageJumpValue('');
									setActiveEllipsis(null);
								}}
								data-flx="channel.channel-search-results.render-content.page-input-form.prevent-default"
							>
								<label
									htmlFor={`channel-search-pagination-input-${side}`}
									className={styles.pageInputLabel}
									data-flx="channel.channel-search-results.render-content.page-input-label"
								>
									{i18n._(GO_TO_PAGE_2_DESCRIPTOR)}
								</label>
								<FocusRing offset={-2} data-flx="channel.channel-search-results.render-content.focus-ring--2">
									<input
										id={`channel-search-pagination-input-${side}`}
										ref={ellipsisInputRef}
										type="number"
										disabled={disabled}
										data-flx="channel.channel-search-results.render-content.page-input.set-page-jump-value.number"
										{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
										min={1}
										max={totalPages}
										inputMode="numeric"
										value={pageJumpValue}
										onChange={(e) => setPageJumpValue(e.target.value)}
										onBlur={() => {
											setActiveEllipsis(null);
											setPageJumpValue('');
										}}
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
							data-flx="channel.channel-search-results.render-content.focus-ring--3"
						>
							<button
								type="button"
								disabled={disabled}
								onClick={() => {
									setActiveEllipsis(side);
									setPageJumpValue('');
								}}
								className={styles.ellipsisButton}
								aria-label={i18n._(JUMP_TO_PAGE_DESCRIPTOR)}
								data-flx="channel.channel-search-results.render-content.ellipsis-button.set-active-ellipsis"
							>
								&hellip;
							</button>
						</FocusRing>
					);
				})}
			</div>
		</div>
	);
};
