// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveContentLocale} from '@app/api/content_i18n/ContentI18n';

const LIST_SEPARATORS: Readonly<Record<string, string>> = {
	ar: '، ',
	ja: '、',
	'zh-CN': '、',
	'zh-TW': '、',
};

export function joinLocalizedList(items: ReadonlyArray<string>, locale: string | null | undefined): string {
	return items.join(LIST_SEPARATORS[resolveContentLocale(locale)] ?? ', ');
}
