// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import {formatNumber} from '@app/features/i18n/utils/IntlCache';

interface BadgeState {
	mentionCount: number;
	hasUnread: boolean;
}

let titleProductName = PRODUCT_NAME;
let readBadgeLocale: () => string = () => 'en-US';
let currentTitleParts: ReadonlyArray<string> = [];
let currentBadgeState: BadgeState = {mentionCount: 0, hasUnread: false};

const buildDocumentTitle = (parts: ReadonlyArray<string>): string => {
	if (!parts.length) {
		return titleProductName;
	}
	return [titleProductName, ...parts].join(' | ');
};
const applyBadgePrefix = (baseTitle: string, badge: BadgeState): string => {
	if (badge.mentionCount > 0) {
		return `(${formatNumber(badge.mentionCount, readBadgeLocale())}) ${baseTitle}`;
	}
	if (badge.hasUnread) {
		return `• ${baseTitle}`;
	}
	return baseTitle;
};
const updateDocumentTitle = (): void => {
	document.title = applyBadgePrefix(buildDocumentTitle(currentTitleParts), currentBadgeState);
};
export const setDocumentTitleProductName = (productName: string): void => {
	if (titleProductName === productName) return;
	titleProductName = productName;
	updateDocumentTitle();
};
export const setDocumentTitleBadge = (mentionCount: number, hasUnread: boolean, readLocale: () => string): void => {
	readBadgeLocale = readLocale;
	currentBadgeState = {mentionCount, hasUnread};
	updateDocumentTitle();
};
export function pushDocumentTitleParts(parts: ReadonlyArray<string>): () => void {
	const previousParts = currentTitleParts;
	currentTitleParts = parts;
	updateDocumentTitle();
	return () => {
		currentTitleParts = previousParts;
		updateDocumentTitle();
	};
}
