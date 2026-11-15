// SPDX-License-Identifier: AGPL-3.0-or-later

const RTL_LANGUAGES: ReadonlySet<string> = new Set(['ar', 'he', 'fa', 'ur']);

export function getLocaleDirection(locale: string | null | undefined): 'rtl' | 'ltr' {
	const language = locale?.split(/[-_]/)[0]?.toLowerCase() ?? '';
	return RTL_LANGUAGES.has(language) ? 'rtl' : 'ltr';
}
