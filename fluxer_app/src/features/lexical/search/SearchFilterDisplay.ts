// SPDX-License-Identifier: AGPL-3.0-or-later

import {SearchFilterCategory} from '@app/features/lexical/search/SearchFilterMeta';

export function resolveDisplayValue(
	category: SearchFilterCategory,
	value: string,
	resolvedName: string | null,
): string {
	if (resolvedName != null && resolvedName.length > 0) {
		return resolvedName;
	}
	if (category === SearchFilterCategory.CHANNEL) {
		return `#${value.replace(/^#/, '')}`;
	}
	if (category === SearchFilterCategory.USER) {
		return value.replace(/#\d{4}(?=,|$)/g, '');
	}
	return value;
}
