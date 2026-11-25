// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const DiscoveryCategories = {
	GAMING: 0,
	MUSIC: 1,
	ENTERTAINMENT: 2,
	EDUCATION: 3,
	SCIENCE_AND_TECHNOLOGY: 4,
	CONTENT_CREATOR: 5,
	ANIME_AND_MANGA: 6,
	MOVIES_AND_TV: 7,
	OTHER: 8,
} as const;

export type DiscoveryCategory = ValueOf<typeof DiscoveryCategories>;

export const DiscoveryCategoryLabels: Record<DiscoveryCategory, string> = {
	0: 'Gaming',
	1: 'Music',
	2: 'Entertainment',
	3: 'Education',
	4: 'Science & Technology',
	5: 'Streamers & Artists',
	6: 'Anime & Manga',
	7: 'Movies & TV',
	8: 'Other',
};
export const DiscoveryApplicationStatus = {
	PENDING: 'pending',
	APPROVED: 'approved',
	REJECTED: 'rejected',
	REMOVED: 'removed',
} as const;

export const DISCOVERY_DESCRIPTION_MIN_LENGTH = 10;
export const DISCOVERY_DESCRIPTION_MAX_LENGTH = 300;
export const DISCOVERY_TAG_MIN_LENGTH = 2;
export const DISCOVERY_TAG_MAX_LENGTH = 30;
export const DISCOVERY_MAX_TAGS = 10;
export const DISCOVERY_DEFAULT_LANGUAGE = 'en-US';
export const DiscoverySupportedLanguages: ReadonlyArray<{
	code: string;
	name: string;
	nativeName: string;
}> = [
	{code: 'ar', name: 'Arabic', nativeName: 'العربية'},
	{code: 'bg', name: 'Bulgarian', nativeName: 'Български'},
	{code: 'cs', name: 'Czech', nativeName: 'Čeština'},
	{code: 'da', name: 'Danish', nativeName: 'Dansk'},
	{code: 'de', name: 'German', nativeName: 'Deutsch'},
	{code: 'el', name: 'Greek', nativeName: 'Ελληνικά'},
	{code: 'en-US', name: 'English', nativeName: 'English'},
	{code: 'es-ES', name: 'Spanish (Spain)', nativeName: 'Español (España)'},
	{code: 'es-419', name: 'Spanish (Latin America)', nativeName: 'Español (Latinoamérica)'},
	{code: 'fi', name: 'Finnish', nativeName: 'Suomi'},
	{code: 'fr', name: 'French', nativeName: 'Français'},
	{code: 'he', name: 'Hebrew', nativeName: 'עברית'},
	{code: 'hi', name: 'Hindi', nativeName: 'हिन्दी'},
	{code: 'hr', name: 'Croatian', nativeName: 'Hrvatski'},
	{code: 'hu', name: 'Hungarian', nativeName: 'Magyar'},
	{code: 'id', name: 'Indonesian', nativeName: 'Bahasa Indonesia'},
	{code: 'it', name: 'Italian', nativeName: 'Italiano'},
	{code: 'ja', name: 'Japanese', nativeName: '日本語'},
	{code: 'ko', name: 'Korean', nativeName: '한국어'},
	{code: 'lt', name: 'Lithuanian', nativeName: 'Lietuvių'},
	{code: 'nl', name: 'Dutch', nativeName: 'Nederlands'},
	{code: 'no', name: 'Norwegian', nativeName: 'Norsk'},
	{code: 'pl', name: 'Polish', nativeName: 'Polski'},
	{code: 'pt-BR', name: 'Portuguese (Brazil)', nativeName: 'Português (Brasil)'},
	{code: 'ro', name: 'Romanian', nativeName: 'Română'},
	{code: 'ru', name: 'Russian', nativeName: 'Русский'},
	{code: 'sv-SE', name: 'Swedish', nativeName: 'Svenska'},
	{code: 'th', name: 'Thai', nativeName: 'ไทย'},
	{code: 'tr', name: 'Turkish', nativeName: 'Türkçe'},
	{code: 'uk', name: 'Ukrainian', nativeName: 'Українська'},
	{code: 'vi', name: 'Vietnamese', nativeName: 'Tiếng Việt'},
	{code: 'zh-CN', name: 'Chinese (Simplified)', nativeName: '中文 (简体)'},
	{code: 'zh-TW', name: 'Chinese (Traditional)', nativeName: '中文 (繁體)'},
];
const DISCOVERY_LANGUAGE_CODES: ReadonlySet<string> = new Set(DiscoverySupportedLanguages.map((l) => l.code));

export function isValidDiscoveryLanguage(code: string): boolean {
	return DISCOVERY_LANGUAGE_CODES.has(code);
}

export function normalizeDiscoveryTag(tag: string): string {
	return tag.trim().toLowerCase().replace(/\s+/g, ' ');
}

export function isValidDiscoveryTag(tag: string): boolean {
	const normalized = normalizeDiscoveryTag(tag);
	if (normalized.length < DISCOVERY_TAG_MIN_LENGTH || normalized.length > DISCOVERY_TAG_MAX_LENGTH) {
		return false;
	}
	return /^[\p{L}\p{N}][\p{L}\p{N} \-_+&]*$/u.test(normalized);
}
