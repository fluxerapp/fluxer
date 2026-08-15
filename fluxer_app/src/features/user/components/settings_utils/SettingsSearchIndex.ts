// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {PREMIUM_PRODUCT_FULL_NAME, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import {onLocaleChange} from '@app/features/i18n/utils/LocaleChangeListener';
import type {SettingsTab} from '@app/features/user/components/settings_utils/SettingsConstants';
import {
	getSettingsAudience,
	type SettingsCategoryTag,
	type SettingsStatusBadgeKind,
} from '@app/features/user/components/settings_utils/SettingsMetadata';
import {
	ACCOUNT_SETTINGS_TAB,
	isAccountNestedSettingsTab,
} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import {
	getSearchableItemsFromRegistry,
	getSectionDefinition,
	type SearchableSettingItem,
	type UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {accessibilityIndex} from '@app/features/user/components/settings_utils/search_index/AccessibilityIndex';
import {accountSecurityIndex} from '@app/features/user/components/settings_utils/search_index/AccountSecurityIndex';
import {advancedSettingsIndex} from '@app/features/user/components/settings_utils/search_index/AdvancedSettingsIndex';
import {appearanceIndex} from '@app/features/user/components/settings_utils/search_index/AppearanceIndex';
import {applicationsIndex} from '@app/features/user/components/settings_utils/search_index/ApplicationsIndex';
import {authorizedAppsIndex} from '@app/features/user/components/settings_utils/search_index/AuthorizedAppsIndex';
import {blockedUsersIndex} from '@app/features/user/components/settings_utils/search_index/BlockedUsersIndex';
import {chatSettingsIndex} from '@app/features/user/components/settings_utils/search_index/ChatSettingsIndex';
import {clientDeveloperSettingsIndex} from '@app/features/user/components/settings_utils/search_index/ClientDeveloperSettingsIndex';
import {desktopSettingsIndex} from '@app/features/user/components/settings_utils/search_index/DesktopSettingsIndex';
import {devicesIndex} from '@app/features/user/components/settings_utils/search_index/DevicesIndex';
import {embedDebuggerIndex} from '@app/features/user/components/settings_utils/search_index/EmbedDebuggerIndex';
import {expressionPacksIndex} from '@app/features/user/components/settings_utils/search_index/ExpressionPacksIndex';
import {giftInventoryIndex} from '@app/features/user/components/settings_utils/search_index/GiftInventoryIndex';
import {keybindsIndex} from '@app/features/user/components/settings_utils/search_index/KeybindsIndex';
import {languageIndex} from '@app/features/user/components/settings_utils/search_index/LanguageIndex';
import {linkedAccountsIndex} from '@app/features/user/components/settings_utils/search_index/LinkedAccountsIndex';
import {myProfileIndex} from '@app/features/user/components/settings_utils/search_index/MyProfileIndex';
import {notificationsIndex} from '@app/features/user/components/settings_utils/search_index/NotificationsIndex';
import {plutoniumIndex} from '@app/features/user/components/settings_utils/search_index/PlutoniumIndex';
import type {
	SearchableSettingDescriptor,
	SearchableSettingKeyword,
} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {voiceVideoIndex} from '@app/features/user/components/settings_utils/search_index/VoiceVideoIndex';
import type {I18n} from '@lingui/core';

export interface SettingsSearchResult {
	tab: SettingsTab;
	matchedItems: Array<SearchableSettingItem>;
	score: number;
}

function isSearchableDescriptorVisible(item: SearchableSettingDescriptor): boolean {
	return !item.isVisible || item.isVisible();
}

function translateSearchableKeyword(keyword: SearchableSettingKeyword, activeI18n: I18n): string {
	if (typeof keyword === 'string') {
		return keyword;
	}
	return activeI18n._(keyword, {
		premiumProductName: PREMIUM_PRODUCT_NAME,
		premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME,
	});
}

function translateSearchableDescriptor(item: SearchableSettingDescriptor, activeI18n: I18n): SearchableSettingItem {
	const {id, tabType, sourceTabType, sectionId, sourceSectionId, label, keywords, description, tags, addedAt, badges} =
		item;
	const resolvedSourceTabType = sourceTabType ?? tabType;
	const section = sectionId ? getSectionDefinition(sectionId, resolvedSourceTabType) : undefined;
	return {
		id,
		tabType,
		sourceTabType: resolvedSourceTabType,
		sectionId,
		sourceSectionId: sourceSectionId ?? sectionId,
		label: translateSearchableKeyword(label, activeI18n),
		keywords: keywords.map((keyword) => translateSearchableKeyword(keyword, activeI18n)),
		description: description ? translateSearchableKeyword(description, activeI18n) : undefined,
		audience: getSettingsAudience({
			id,
			audience: item.audience,
			isAdvanced: section?.isAdvanced,
		}),
		tags,
		addedAt,
		badges,
	};
}

const ADDITIONAL_SEARCHABLE_ITEMS: Array<SearchableSettingDescriptor> = [
	...myProfileIndex,
	...accountSecurityIndex,
	...linkedAccountsIndex,
	...authorizedAppsIndex,
	...blockedUsersIndex,
	...devicesIndex,
	...plutoniumIndex,
	...giftInventoryIndex,
	...expressionPacksIndex,
	...appearanceIndex,
	...notificationsIndex,
	...chatSettingsIndex,
	...voiceVideoIndex,
	...accessibilityIndex,
	...languageIndex,
	...keybindsIndex,
	...desktopSettingsIndex,
	...advancedSettingsIndex,
	...clientDeveloperSettingsIndex,
	...applicationsIndex,
	...embedDebuggerIndex,
];

function createSearchableItems(): Array<SearchableSettingItem> {
	const registryItems = getSearchableItemsFromRegistry(i18n);
	const additionalItems = ADDITIONAL_SEARCHABLE_ITEMS.filter(isSearchableDescriptorVisible).map((item) =>
		translateSearchableDescriptor(item, i18n),
	);
	return [...registryItems, ...additionalItems];
}

let cachedSearchableItems: Array<SearchableSettingItem> | null = null;

export function getSearchableItems(): Array<SearchableSettingItem> {
	if (!cachedSearchableItems) {
		cachedSearchableItems = createSearchableItems();
	}
	return cachedSearchableItems;
}

export function invalidateSearchCache(): void {
	cachedSearchableItems = null;
}

onLocaleChange(invalidateSearchCache);

function normalizeSearchQuery(query: string): string {
	return query.toLowerCase().trim().replace(/\s+/g, ' ');
}

function getQueryWords(normalizedQuery: string): Array<string> {
	return normalizedQuery.split(' ').filter((word) => word.length > 0);
}

function calculateTextMatchScore(
	text: string,
	word: string,
	exactScore: number,
	prefixScore: number,
	containsScore: number,
): number {
	if (!text.includes(word)) {
		return 0;
	}
	if (text === word) {
		return exactScore;
	}
	return text.startsWith(word) ? prefixScore : containsScore;
}

function calculateKeywordMatchScore(keywords: Array<string>, word: string): number {
	return keywords.reduce((score, keyword) => score + calculateTextMatchScore(keyword, word, 80, 40, 20), 0);
}

function calculateMatchScore(item: SearchableSettingItem, queryWords: Array<string>): number {
	let score = 0;

	const labelLower = item.label.toLowerCase();
	const descriptionLower = item.description?.toLowerCase() ?? '';
	const keywordsLower = [...item.keywords, ...(item.tags ?? [])].map((value) => value.toLowerCase());

	for (const word of queryWords) {
		let wordScore = calculateTextMatchScore(labelLower, word, 100, 50, 25);

		wordScore += calculateKeywordMatchScore(keywordsLower, word);

		if (descriptionLower.includes(word)) {
			wordScore += 10;
		}

		if (wordScore === 0) {
			return 0;
		}

		score += wordScore;
	}

	return score;
}

function createTabsByType(tabs: Array<SettingsTab>): Map<UserSettingsTabType, SettingsTab> {
	const tabsByType = new Map<UserSettingsTabType, SettingsTab>();
	for (const tab of tabs) {
		if (!tabsByType.has(tab.type)) {
			tabsByType.set(tab.type, tab);
		}
	}
	return tabsByType;
}

function addMatchedItemToResults(
	resultsByTab: Map<UserSettingsTabType, SettingsSearchResult>,
	tab: SettingsTab,
	item: SearchableSettingItem,
	score: number,
): void {
	const existing = resultsByTab.get(item.tabType);
	if (existing) {
		existing.matchedItems.push(item);
		existing.score = Math.max(existing.score, score);
		return;
	}
	resultsByTab.set(item.tabType, {
		tab,
		matchedItems: [item],
		score,
	});
}

export function searchSettings(query: string, tabs: Array<SettingsTab>): Array<SettingsSearchResult> {
	const normalizedQuery = normalizeSearchQuery(query);
	if (!normalizedQuery) {
		return [];
	}
	const queryWords = getQueryWords(normalizedQuery);
	const items = getSearchableItems();
	const tabsByType = createTabsByType(tabs);
	const resultsByTab = new Map<UserSettingsTabType, SettingsSearchResult>();
	for (const item of items) {
		const resultItem = (() => {
			if (item.audience === 'advanced' && item.tabType !== 'advanced_settings') {
				return {
					...item,
					sourceTabType: item.sourceTabType ?? item.tabType,
					sourceSectionId: item.sourceSectionId ?? item.sectionId,
					tabType: 'advanced_settings',
				} satisfies SearchableSettingItem;
			}
			if (isAccountNestedSettingsTab(item.tabType)) {
				return {
					...item,
					sourceTabType: item.sourceTabType ?? item.tabType,
					sourceSectionId: item.sourceSectionId ?? item.sectionId,
					tabType: ACCOUNT_SETTINGS_TAB,
				} satisfies SearchableSettingItem;
			}
			return item;
		})();
		const tab = tabsByType.get(resultItem.tabType);
		if (!tab) {
			continue;
		}
		const score = calculateMatchScore(resultItem, queryWords);
		if (score > 0) {
			addMatchedItemToResults(resultsByTab, tab, resultItem, score);
		}
	}
	const results = Array.from(resultsByTab.values());
	results.sort((a, b) => b.score - a.score);
	return results;
}

export function getAdvancedSettingItems(): Array<SearchableSettingItem> {
	return getSearchableItems().filter((item) => item.audience === 'advanced');
}

export function filterAdvancedSettingItems(
	query: string,
	tags: ReadonlySet<SettingsCategoryTag>,
	items: Array<SearchableSettingItem> = getAdvancedSettingItems(),
): Array<SearchableSettingItem> {
	const normalizedQuery = normalizeSearchQuery(query);
	const queryWords = getQueryWords(normalizedQuery);
	return items
		.filter((item) => tags.size === 0 || item.tags?.some((tag) => tags.has(tag)) === true)
		.map((item) => ({
			item,
			score: queryWords.length === 0 ? 1 : calculateMatchScore(item, queryWords),
		}))
		.filter(({score}) => score > 0)
		.sort((a, b) => b.score - a.score || a.item.label.localeCompare(b.item.label))
		.map(({item}) => item);
}

export function getSearchableItemStatusBadges(item: SearchableSettingItem): ReadonlyArray<SettingsStatusBadgeKind> {
	return item.badges ?? [];
}

export function getMatchedSectionIds(results: Array<SettingsSearchResult>): Set<string> {
	const sectionIds = new Set<string>();
	for (const result of results) {
		for (const item of result.matchedItems) {
			if (item.sectionId) {
				sectionIds.add(item.sectionId);
			}
		}
	}
	return sectionIds;
}

export function getMatchedTabTypes(results: Array<SettingsSearchResult>): Set<UserSettingsTabType> {
	return new Set(results.map((r) => r.tab.type));
}
