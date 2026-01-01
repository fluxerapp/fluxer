/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useEffect, useRef} from 'react';

const TITLE_PREFIX = 'Fluxer';

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
		return `(${badge.mentionCount}) ${baseTitle}`;
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
