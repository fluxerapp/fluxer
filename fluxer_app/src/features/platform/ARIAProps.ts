// SPDX-License-Identifier: AGPL-3.0-or-later

const ARIA_CURRENT_PAGE = Object.freeze({'aria-current': 'page'} as const);
const ARIA_CURRENT_OMITTED = Object.freeze({} as const);

export type ARIACurrentPageProps = typeof ARIA_CURRENT_PAGE | typeof ARIA_CURRENT_OMITTED;
export function ariaCurrentPage(isCurrent: boolean): ARIACurrentPageProps {
	if (isCurrent) {
		return ARIA_CURRENT_PAGE;
	}
	return ARIA_CURRENT_OMITTED;
}
