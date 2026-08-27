// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n, {loadLocaleCatalog, normalizeLocale} from '@app/app/I18n';
import {getCachedCollator} from '@app/features/i18n/utils/IntlCache';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import {DiscoverySupportedLanguages} from '@fluxer/constants/src/DiscoveryConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ARABIC_DESCRIPTOR = msg({
	message: 'Arabic',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const BULGARIAN_DESCRIPTOR = msg({
	message: 'Bulgarian',
	comment: 'Accessible label in the language and locale names. Keep it concise.',
});
const CZECH_DESCRIPTOR = msg({
	message: 'Czech',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const DANISH_DESCRIPTOR = msg({
	message: 'Danish',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const GERMAN_DESCRIPTOR = msg({
	message: 'German',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const GREEK_DESCRIPTOR = msg({
	message: 'Greek',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const ENGLISH_DESCRIPTOR = msg({
	message: 'English',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const ENGLISH_US_DESCRIPTOR = msg({
	message: 'English (US)',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const SPANISH_SPAIN_DESCRIPTOR = msg({
	message: 'Spanish (Spain)',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const SPANISH_LATIN_AMERICA_DESCRIPTOR = msg({
	message: 'Spanish (Latin America)',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const FINNISH_DESCRIPTOR = msg({
	message: 'Finnish',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const FRENCH_DESCRIPTOR = msg({
	message: 'French',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const HEBREW_DESCRIPTOR = msg({
	message: 'Hebrew',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const HINDI_DESCRIPTOR = msg({
	message: 'Hindi',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const CROATIAN_DESCRIPTOR = msg({
	message: 'Croatian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const HUNGARIAN_DESCRIPTOR = msg({
	message: 'Hungarian',
	comment: 'Accessible label in the language and locale names. Keep it concise.',
});
const INDONESIAN_DESCRIPTOR = msg({
	message: 'Indonesian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const ITALIAN_DESCRIPTOR = msg({
	message: 'Italian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const JAPANESE_DESCRIPTOR = msg({
	message: 'Japanese',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const KOREAN_DESCRIPTOR = msg({
	message: 'Korean',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const LITHUANIAN_DESCRIPTOR = msg({
	message: 'Lithuanian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const DUTCH_DESCRIPTOR = msg({
	message: 'Dutch',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const NORWEGIAN_DESCRIPTOR = msg({
	message: 'Norwegian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const POLISH_DESCRIPTOR = msg({
	message: 'Polish',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const PORTUGUESE_BRAZIL_DESCRIPTOR = msg({
	message: 'Portuguese (Brazil)',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const ROMANIAN_DESCRIPTOR = msg({
	message: 'Romanian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const RUSSIAN_DESCRIPTOR = msg({
	message: 'Russian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const SWEDISH_DESCRIPTOR = msg({
	message: 'Swedish',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const THAI_DESCRIPTOR = msg({
	message: 'Thai',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const TURKISH_DESCRIPTOR = msg({
	message: 'Turkish',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const UKRAINIAN_DESCRIPTOR = msg({
	message: 'Ukrainian',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const VIETNAMESE_DESCRIPTOR = msg({
	message: 'Vietnamese',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const CHINESE_SIMPLIFIED_DESCRIPTOR = msg({
	message: 'Chinese (simplified)',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const CHINESE_TRADITIONAL_DESCRIPTOR = msg({
	message: 'Chinese (traditional)',
	comment: 'Short label in the language and locale names. Keep it concise.',
});
const logger = new Logger('LocaleUtils');

interface LocaleInfo {
	code: string;
	name: MessageDescriptor;
	nativeName: string;
	flag: string;
	region?: string;
}

const SUPPORTED_LOCALES: Array<LocaleInfo> = [
	{code: 'ar', name: ARABIC_DESCRIPTOR, nativeName: 'العربية', flag: '🇸🇦'},
	{code: 'bg', name: BULGARIAN_DESCRIPTOR, nativeName: 'Български', flag: '🇧🇬'},
	{code: 'cs', name: CZECH_DESCRIPTOR, nativeName: 'Čeština', flag: '🇨🇿'},
	{code: 'da', name: DANISH_DESCRIPTOR, nativeName: 'Dansk', flag: '🇩🇰'},
	{code: 'de', name: GERMAN_DESCRIPTOR, nativeName: 'Deutsch', flag: '🇩🇪'},
	{code: 'el', name: GREEK_DESCRIPTOR, nativeName: 'Ελληνικά', flag: '🇬🇷'},
	{code: 'en-GB', name: ENGLISH_DESCRIPTOR, nativeName: 'English', flag: '🇬🇧'},
	{code: 'en-US', name: ENGLISH_US_DESCRIPTOR, nativeName: 'English (US)', flag: '🇺🇸'},
	{code: 'es-ES', name: SPANISH_SPAIN_DESCRIPTOR, nativeName: 'Español (España)', flag: '🇪🇸'},
	{code: 'es-419', name: SPANISH_LATIN_AMERICA_DESCRIPTOR, nativeName: 'Español (Latinoamérica)', flag: '🌎'},
	{code: 'fi', name: FINNISH_DESCRIPTOR, nativeName: 'Suomi', flag: '🇫🇮'},
	{code: 'fr', name: FRENCH_DESCRIPTOR, nativeName: 'Français', flag: '🇫🇷'},
	{code: 'he', name: HEBREW_DESCRIPTOR, nativeName: 'עברית', flag: '🇮🇱'},
	{code: 'hi', name: HINDI_DESCRIPTOR, nativeName: 'हिन्दी', flag: '🇮🇳'},
	{code: 'hr', name: CROATIAN_DESCRIPTOR, nativeName: 'Hrvatski', flag: '🇭🇷'},
	{code: 'hu', name: HUNGARIAN_DESCRIPTOR, nativeName: 'Magyar', flag: '🇭🇺'},
	{code: 'id', name: INDONESIAN_DESCRIPTOR, nativeName: 'Bahasa Indonesia', flag: '🇮🇩'},
	{code: 'it', name: ITALIAN_DESCRIPTOR, nativeName: 'Italiano', flag: '🇮🇹'},
	{code: 'ja', name: JAPANESE_DESCRIPTOR, nativeName: '日本語', flag: '🇯🇵'},
	{code: 'ko', name: KOREAN_DESCRIPTOR, nativeName: '한국어', flag: '🇰🇷'},
	{code: 'lt', name: LITHUANIAN_DESCRIPTOR, nativeName: 'Lietuvių', flag: '🇱🇹'},
	{code: 'nl', name: DUTCH_DESCRIPTOR, nativeName: 'Nederlands', flag: '🇳🇱'},
	{code: 'no', name: NORWEGIAN_DESCRIPTOR, nativeName: 'Norsk', flag: '🇳🇴'},
	{code: 'pl', name: POLISH_DESCRIPTOR, nativeName: 'Polski', flag: '🇵🇱'},
	{code: 'pt-BR', name: PORTUGUESE_BRAZIL_DESCRIPTOR, nativeName: 'Português (Brasil)', flag: '🇧🇷'},
	{code: 'ro', name: ROMANIAN_DESCRIPTOR, nativeName: 'Română', flag: '🇷🇴'},
	{code: 'ru', name: RUSSIAN_DESCRIPTOR, nativeName: 'Русский', flag: '🇷🇺'},
	{code: 'sv-SE', name: SWEDISH_DESCRIPTOR, nativeName: 'Svenska', flag: '🇸🇪'},
	{code: 'th', name: THAI_DESCRIPTOR, nativeName: 'ไทย', flag: '🇹🇭'},
	{code: 'tr', name: TURKISH_DESCRIPTOR, nativeName: 'Türkçe', flag: '🇹🇷'},
	{code: 'uk', name: UKRAINIAN_DESCRIPTOR, nativeName: 'Українська', flag: '🇺🇦'},
	{code: 'vi', name: VIETNAMESE_DESCRIPTOR, nativeName: 'Tiếng Việt', flag: '🇻🇳'},
	{code: 'zh-CN', name: CHINESE_SIMPLIFIED_DESCRIPTOR, nativeName: '中文 (简体)', flag: '🇨🇳'},
	{code: 'zh-TW', name: CHINESE_TRADITIONAL_DESCRIPTOR, nativeName: '中文 (繁體)', flag: '🇹🇼'},
];
const DEFAULT_LOCALE = 'en-US';

let localeChangeSequence = 0;
let localLocaleOverride: string | null = null;

export function getCurrentLocale(): string {
	return UserSettings.getLocale() || DEFAULT_LOCALE;
}

export function getCurrentOrDetectedLocale(): string {
	if (localLocaleOverride) {
		return localLocaleOverride;
	}
	const currentLocale = UserSettings.getLocale();
	if (UserSettings.isHydrated() || currentLocale !== DEFAULT_LOCALE) {
		return currentLocale || DEFAULT_LOCALE;
	}
	return normalizeLocale(i18n.locale || DEFAULT_LOCALE);
}

export function setLocale(localeCode: string): void {
	const locale = SUPPORTED_LOCALES.find((supportedLocale) => supportedLocale.code === localeCode);
	if (!locale) {
		logger.warn(`Unsupported locale: ${localeCode}`);
		return;
	}
	const normalized = normalizeLocale(locale.code);
	const previousLocale = normalizeLocale(getCurrentLocale());
	if (normalized === previousLocale) {
		return;
	}
	const sequence = ++localeChangeSequence;
	void (async () => {
		try {
			await loadLocaleCatalog(normalized);
		} catch (error) {
			logger.error(`Failed to load locale ${localeCode}:`, error);
			return;
		}
		if (sequence !== localeChangeSequence) {
			return;
		}
		try {
			await UserSettingsCommands.update({
				locale: normalized,
			});
		} catch (error) {
			logger.error(`Failed to persist locale ${localeCode}:`, error);
			if (sequence !== localeChangeSequence) {
				return;
			}
			try {
				await loadLocaleCatalog(previousLocale);
			} catch (restoreError) {
				logger.error(`Failed to restore locale ${previousLocale}:`, restoreError);
			}
		}
	})();
}

export function setLocalLocale(localeCode: string): void {
	const locale = SUPPORTED_LOCALES.find((supportedLocale) => supportedLocale.code === localeCode);
	if (!locale) {
		logger.warn(`Unsupported locale: ${localeCode}`);
		return;
	}
	const normalized = normalizeLocale(locale.code);
	localLocaleOverride = normalized;
	const sequence = ++localeChangeSequence;
	UserSettings.applyLocalLocale(normalized);
	void (async () => {
		try {
			await loadLocaleCatalog(normalized);
		} catch (error) {
			logger.error(`Failed to load locale ${localeCode}:`, error);
			return;
		}
		if (sequence !== localeChangeSequence) {
			return;
		}
		UserSettings.applyLocalLocale(normalized);
	})();
}

interface TranslatedLocaleInfo {
	code: string;
	name: string;
	nativeName: string;
	flag: string;
	region?: string;
}

interface DiscoveryLanguageInfo {
	code: string;
	label: string;
}

export function getLocalizedLocaleName(code: string): string {
	const info = SUPPORTED_LOCALES.find((locale) => locale.code === code);
	return info ? i18n._(info.name) : code;
}

export function getDiscoveryLanguageLabel(code: string): string {
	if (code === 'en-US') {
		return i18n._(ENGLISH_DESCRIPTOR);
	}
	return getLocalizedLocaleName(code);
}

export function getSortedDiscoveryLanguages(): Array<DiscoveryLanguageInfo> {
	const collator = getCachedCollator(i18n.locale || undefined, {sensitivity: 'base'});
	return DiscoverySupportedLanguages.map((language) => ({
		code: language.code,
		label: getDiscoveryLanguageLabel(language.code),
	})).sort((a, b) => collator.compare(a.label, b.label) || a.code.localeCompare(b.code));
}

export function getSortedLocales(): Array<TranslatedLocaleInfo> {
	return [...SUPPORTED_LOCALES]
		.map((locale) => ({
			...locale,
			name: i18n._(locale.name),
		}))
		.sort((a, b) => a.code.localeCompare(b.code));
}
