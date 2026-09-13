// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';

const FILE_SIZE_UNIT_BYTES = 1024;
const FILE_SIZE_UNITS = ['byte', 'kilobyte', 'megabyte', 'gigabyte'] as const;

export function formatFileSize(locale: string, bytes: number): string {
	if (!Number.isSafeInteger(bytes) || bytes < 0) {
		throw new RangeError('File size must be a non-negative safe integer');
	}
	const unitIndex =
		bytes === 0
			? 0
			: Math.min(Math.floor(Math.log(bytes) / Math.log(FILE_SIZE_UNIT_BYTES)), FILE_SIZE_UNITS.length - 1);
	return getCachedNumberFormat(locale, {
		style: 'unit',
		unit: FILE_SIZE_UNITS[unitIndex],
		unitDisplay: 'narrow',
		maximumFractionDigits: 1,
	}).format(bytes / FILE_SIZE_UNIT_BYTES ** unitIndex);
}
