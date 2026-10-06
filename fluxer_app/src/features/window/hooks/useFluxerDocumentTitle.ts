// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {useEffect, useRef} from 'react';

const TITLE_PREFIX = PRODUCT_NAME;

type TitlePart = string | null | undefined;
type TitleInput = TitlePart | Array<TitlePart>;

interface UseDocumentTitleOptions {
	preserveTitleOnUnmount?: boolean;
}

interface BadgeState {
	mentionCount: number;
	hasUnread: boolean;
}

let currentBaseTitle = TITLE_PREFIX;
let currentBadgeState: BadgeState = {mentionCount: 0, hasUnread: false};

const normalizeTitleParts = (value?: TitleInput): Array<string> => {
	if (!value) {
		return [];
	}
	const parts = Array.isArray(value) ? value : [value];
	return parts.map((part) => part?.trim()).filter((part): part is string => Boolean(part));
};
const buildDocumentTitle = (parts: Array<string>): string => {
	if (!parts.length) {
		return TITLE_PREFIX;
	}
	return [TITLE_PREFIX, ...parts].join(' | ');
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
	document.title = applyBadgePrefix(currentBaseTitle, currentBadgeState);
};
export const updateDocumentTitleBadge = (mentionCount: number, hasUnread: boolean): void => {
	currentBadgeState = {mentionCount, hasUnread};
	updateDocumentTitle();
};
export const useFluxerDocumentTitle = (title?: TitleInput, options?: UseDocumentTitleOptions) => {
	const parts = normalizeTitleParts(title);
	const fullTitle = buildDocumentTitle(parts);
	const prevTitleRef = useRef<string | undefined>(undefined);
	useEffect(() => {
		prevTitleRef.current = currentBaseTitle;
		currentBaseTitle = fullTitle;
		updateDocumentTitle();
		return () => {
			if (!options?.preserveTitleOnUnmount && prevTitleRef.current) {
				currentBaseTitle = prevTitleRef.current;
				updateDocumentTitle();
			}
		};
	}, [fullTitle, options?.preserveTitleOnUnmount]);
};
