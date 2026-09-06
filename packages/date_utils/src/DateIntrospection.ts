// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';
import type {DateFieldType} from '@fluxer/date_utils/src/DateTypes';

export function getDateFieldOrder(locale: string): Array<DateFieldType> {
	const formatter = getDateFormatter(locale, {
		year: 'numeric',
		month: '2-digit',
		day: '2-digit',
	});
	const parts = formatter.formatToParts(new Date(2000, 0, 1));
	const order: Array<DateFieldType> = [];
	for (const part of parts) {
		if (part.type === 'month' && !order.includes('month')) {
			order.push('month');
		} else if (part.type === 'day' && !order.includes('day')) {
			order.push('day');
		} else if (part.type === 'year' && !order.includes('year')) {
			order.push('year');
		}
	}
	return order;
}

export function getMonthNames(locale: string, format: 'long' | 'short' = 'long'): Array<string> {
	return Array.from({length: 12}, (_, index) => {
		const monthDate = new Date(2000, index, 1);
		return getDateFormatter(locale, {month: format}).format(monthDate);
	});
}
