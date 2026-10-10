// SPDX-License-Identifier: AGPL-3.0-or-later

import {Locales} from '@fluxer/constants/src/Locales';
import {getCachedListFormatter} from '@pkgs/list_utils/src/ListFormattingCache';
import {formatListWithFallback} from '@pkgs/list_utils/src/ListFormattingFallback';
import {isIntlListFormatLocaleSupported, isIntlListFormatSupported} from '@pkgs/list_utils/src/ListFormattingSupport';
import type {
	ListFormatStyle,
	ListFormatType,
	ListFormatterConfig,
	ResolvedListFormatterConfig,
} from '@pkgs/list_utils/src/ListFormattingTypes';

const DEFAULT_LOCALE = Locales.EN_US;
const DEFAULT_LIST_FORMAT_STYLE: ListFormatStyle = 'long';
const DEFAULT_LIST_FORMAT_TYPE: ListFormatType = 'conjunction';

function normalizeLocale(locale: string): string {
	if (!isIntlListFormatLocaleSupported(locale)) {
		return DEFAULT_LOCALE;
	}
	return locale;
}

function resolveListFormatterConfig(config: ListFormatterConfig): ResolvedListFormatterConfig {
	const requestedLocale = config.locale?.trim();
	const locale = requestedLocale == null || requestedLocale === '' ? DEFAULT_LOCALE : normalizeLocale(requestedLocale);
	const style = config.style ?? DEFAULT_LIST_FORMAT_STYLE;
	const type = config.type ?? DEFAULT_LIST_FORMAT_TYPE;
	return {
		locale,
		style,
		type,
	};
}

function formatWithIntl(items: ReadonlyArray<string>, config: ResolvedListFormatterConfig): string {
	try {
		return getCachedListFormatter(config).format(items);
	} catch {
		return formatListWithFallback(items, config.type);
	}
}

function formatItems(items: ReadonlyArray<string>, config: ResolvedListFormatterConfig): string {
	if (items.length === 0) {
		return '';
	}
	if (items.length === 1) {
		return items[0] ?? '';
	}
	if (!isIntlListFormatSupported()) {
		return formatListWithFallback(items, config.type);
	}
	return formatWithIntl(items, config);
}

export function formatListWithConfig(items: ReadonlyArray<string>, config: ListFormatterConfig = {}): string {
	const resolvedConfig = resolveListFormatterConfig(config);
	return formatItems(items, resolvedConfig);
}
