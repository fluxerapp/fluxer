// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';

export function localeUses12Hour(locale: string): boolean {
	return getDateFormatter(locale, {hour: 'numeric'}).resolvedOptions().hour12 === true;
}
