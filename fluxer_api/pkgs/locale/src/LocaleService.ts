// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LocaleCode} from '@fluxer/constants/src/Locales';
import {resolveLocaleFromAcceptLanguageHeader} from '@pkgs/locale/src/resolution/AcceptLanguageNegotiation';

export function parseAcceptLanguage(acceptLanguageHeader: string | null | undefined): LocaleCode {
	return resolveLocaleFromAcceptLanguageHeader(acceptLanguageHeader);
}
