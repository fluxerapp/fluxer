// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserSettingsSubtabType} from '@app/features/user/components/settings_utils/SettingsConstants';
import type {
	SearchableSettingItem,
	UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';

export function getAdvancedSettingSourceTab(item: SearchableSettingItem): UserSettingsTabType {
	return item.sourceTabType ?? item.tabType;
}

export function getAdvancedSettingSourceSection(item: SearchableSettingItem): UserSettingsSubtabType | undefined {
	return (item.sourceSectionId ?? item.sectionId) as UserSettingsSubtabType | undefined;
}

export function getAdvancedSettingRowElementId(itemId: string): string {
	return `advanced-setting-${itemId}`;
}
