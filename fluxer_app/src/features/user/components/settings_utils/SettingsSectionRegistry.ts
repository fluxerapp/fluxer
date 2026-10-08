// SPDX-License-Identifier: AGPL-3.0-or-later

import {getSettingsAudience} from '@app/features/user/components/settings_utils/SettingsMetadata';
import {accessibilitySections} from '@app/features/user/components/settings_utils/section_registry/AccessibilitySections';
import {accountSecuritySections} from '@app/features/user/components/settings_utils/section_registry/AccountSecuritySections';
import {appearanceSections} from '@app/features/user/components/settings_utils/section_registry/AppearanceSections';
import {chatSettingsSections} from '@app/features/user/components/settings_utils/section_registry/ChatSettingsSections';
import {generalSettingsSections} from '@app/features/user/components/settings_utils/section_registry/GeneralSettingsSections';
import {notificationsSections} from '@app/features/user/components/settings_utils/section_registry/NotificationsSections';
import {privacySafetySections} from '@app/features/user/components/settings_utils/section_registry/PrivacySafetySections';
import type {
	SearchableSettingItem,
	SectionDefinition,
	SettingsSectionConfig,
	UserSettingsTabType,
} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {voiceVideoSections} from '@app/features/user/components/settings_utils/section_registry/VoiceVideoSections';
import type {I18n} from '@lingui/core';

export type {
	SearchableSettingItem,
	SectionDefinition,
	SettingsSectionConfig,
	UserSettingsTabType,
} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';

const SECTION_REGISTRY = [
	...generalSettingsSections,
	...appearanceSections,
	...accessibilitySections,
	...chatSettingsSections,
	...voiceVideoSections,
	...privacySafetySections,
	...accountSecuritySections,
	...notificationsSections,
] as const satisfies ReadonlyArray<SectionDefinition>;

export type SettingsSectionId = (typeof SECTION_REGISTRY)[number]['id'];
export type SettingsSectionDefinition = (typeof SECTION_REGISTRY)[number];

function isSectionVisible(section: SectionDefinition): boolean {
	return !section.isVisible || section.isVisible();
}

function sectionMatchesTab(section: SectionDefinition, tabType: UserSettingsTabType): boolean {
	return section.tabType === tabType;
}

function sectionMatchesAllowedTabs(
	section: SectionDefinition,
	allowedTabTypes: Set<UserSettingsTabType> | undefined,
): boolean {
	return !allowedTabTypes || allowedTabTypes.has(section.tabType);
}

function toSettingsSectionConfig(section: SectionDefinition, i18n: I18n): SettingsSectionConfig {
	return {
		id: section.id,
		label: i18n._(section.label),
		isAdvanced: section.isAdvanced,
		audience: getSettingsAudience(section),
		tags: section.tags,
		addedAt: section.addedAt,
		badges: section.badges,
	};
}

function toSearchableSettingItem(section: SectionDefinition, i18n: I18n): SearchableSettingItem {
	return {
		id: `section-${section.tabType}-${section.id}`,
		tabType: section.tabType,
		sourceTabType: section.tabType,
		sectionId: section.id,
		sourceSectionId: section.id,
		label: i18n._(section.label),
		keywords: section.keywords.map((keyword) => {
			if (typeof keyword === 'string') return keyword;
			if (typeof keyword === 'function') return keyword();
			return i18n._(keyword);
		}),
		description: section.description ? i18n._(section.description) : undefined,
		audience: getSettingsAudience(section),
		tags: section.tags,
		addedAt: section.addedAt,
		badges: section.badges,
	};
}

export function getSectionsForTab(tabType: UserSettingsTabType, i18n: I18n): Array<SettingsSectionConfig> {
	return getVisibleSectionsForTab(tabType).map((section) => toSettingsSectionConfig(section, i18n));
}

export function getSectionIdsForTab(tabType: UserSettingsTabType): Array<string> {
	return getVisibleSectionsForTab(tabType).map((section) => section.id);
}

export function tabHasSections(tabType: UserSettingsTabType): boolean {
	return SECTION_REGISTRY.some((section) => section.tabType === tabType);
}

export function tabHasMultipleLinkableSections(tabType: UserSettingsTabType): boolean {
	return getVisibleSectionsForTab(tabType).length > 1;
}

export function getAllSectionDefinitions(): Array<SettingsSectionDefinition> {
	return Array.from(SECTION_REGISTRY);
}

export function getSectionDefinition(
	sectionId: string,
	tabType?: UserSettingsTabType,
): SettingsSectionDefinition | undefined {
	return SECTION_REGISTRY.find((section) => section.id === sectionId && (!tabType || section.tabType === tabType));
}

export function getVisibleSectionsForTab(tabType: UserSettingsTabType): Array<SettingsSectionDefinition> {
	return SECTION_REGISTRY.filter((section) => sectionMatchesTab(section, tabType) && isSectionVisible(section));
}

export function isSectionIdValid(sectionId: string, tabType?: UserSettingsTabType): boolean {
	return SECTION_REGISTRY.some((section) => {
		if (section.id !== sectionId) return false;
		if (tabType && section.tabType !== tabType) return false;
		return true;
	});
}

export function getSearchableItemsFromRegistry(
	i18n: I18n,
	allowedTabTypes?: Set<UserSettingsTabType>,
): Array<SearchableSettingItem> {
	return SECTION_REGISTRY.filter(
		(section) => sectionMatchesAllowedTabs(section, allowedTabTypes) && isSectionVisible(section),
	).map((section) => toSearchableSettingItem(section, i18n));
}
