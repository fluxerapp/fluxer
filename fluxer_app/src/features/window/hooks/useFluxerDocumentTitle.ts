// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {useEffect} from 'react';

type TitlePart = string | null | undefined;
type TitleInput = TitlePart | Array<TitlePart>;

interface UseDocumentTitleOptions {
	preserveTitleOnUnmount?: boolean;
}

interface BadgeState {
	mentionCount: number;
	hasUnread: boolean;
}

let titleProductName = PRODUCT_NAME;
let currentTitleParts: ReadonlyArray<string> = [];
let currentBadgeState: BadgeState = {mentionCount: 0, hasUnread: false};

const normalizeTitleParts = (value?: TitleInput): Array<string> => {
	if (!value) {
		return [];
	}
	const parts = Array.isArray(value) ? value : [value];
	return parts.map((part) => part?.trim()).filter((part): part is string => Boolean(part));
};
const buildDocumentTitle = (parts: ReadonlyArray<string>): string => {
	if (!parts.length) {
		return titleProductName;
	}
	return [titleProductName, ...parts].join(' | ');
};
const applyBadgePrefix = (baseTitle: string, badge: BadgeState): string => {
	if (badge.mentionCount > 0) {
		return `(${formatNumber(badge.mentionCount, getCurrentLocale())}) ${baseTitle}`;
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
export const updateDocumentTitleBadge = (mentionCount: number, hasUnread: boolean): void => {
	currentBadgeState = {mentionCount, hasUnread};
	updateDocumentTitle();
};
export const useFluxerDocumentTitle = (title?: TitleInput, options?: UseDocumentTitleOptions) => {
	const partsKey = JSON.stringify(normalizeTitleParts(title));
	const preserveTitleOnUnmount = options?.preserveTitleOnUnmount;
	useEffect(() => {
		const previousParts = currentTitleParts;
		currentTitleParts = JSON.parse(partsKey) as Array<string>;
		updateDocumentTitle();
		return () => {
			if (!preserveTitleOnUnmount) {
				currentTitleParts = previousParts;
				updateDocumentTitle();
			}
		};
	}, [partsKey, preserveTitleOnUnmount]);
};
