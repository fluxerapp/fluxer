// SPDX-License-Identifier: AGPL-3.0-or-later

import {getFormattingLocale} from '@app/features/user/utils/LocaleUtils';
import {pushDocumentTitleParts, setDocumentTitleBadge} from '@app/features/window/state/DocumentTitle';
import {useEffect} from 'react';

type TitlePart = string | null | undefined;
type TitleInput = TitlePart | Array<TitlePart>;

const normalizeTitleParts = (value?: TitleInput): Array<string> => {
	if (!value) {
		return [];
	}
	const parts = Array.isArray(value) ? value : [value];
	return parts.map((part) => part?.trim()).filter((part): part is string => Boolean(part));
};
export const updateDocumentTitleBadge = (mentionCount: number, hasUnread: boolean): void => {
	setDocumentTitleBadge(mentionCount, hasUnread, getFormattingLocale);
};

export const useFluxerDocumentTitle = (title?: TitleInput) => {
	const partsKey = JSON.stringify(normalizeTitleParts(title));
	useEffect(() => pushDocumentTitleParts(JSON.parse(partsKey) as Array<string>), [partsKey]);
};
