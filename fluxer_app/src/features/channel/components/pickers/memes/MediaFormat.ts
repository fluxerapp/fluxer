// SPDX-License-Identifier: AGPL-3.0-or-later

import {formatDuration as formatDurationBase} from '@fluxer/date_utils/src/DateDuration';

export const formatDuration = (seconds: number | null | undefined, locale: string): string => {
	if (!seconds || seconds <= 0) return formatDurationBase(0, locale);
	return formatDurationBase(seconds, locale);
};
export const getFileExtension = (filename: string, contentType: string): string => {
	const extension = filename.split('.').pop()?.toUpperCase();
	if (extension && extension.length <= 4) return extension;
	const typeMatch = contentType.match(/\/([^;]+)/);
	return typeMatch?.[1]?.toUpperCase() || 'FILE';
};
