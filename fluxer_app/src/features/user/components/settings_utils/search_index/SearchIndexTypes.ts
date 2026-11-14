// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SettingsMetadata} from '@app/features/user/components/settings_utils/SettingsMetadata';
import type {UserSettingsTabType} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import type {MessageDescriptor} from '@lingui/core';

export type SearchableSettingKeyword = MessageDescriptor | string | (() => string);

export interface SearchableSettingDescriptor extends SettingsMetadata {
	id: string;
	tabType: UserSettingsTabType;
	sourceTabType?: UserSettingsTabType;
	sectionId?: string;
	sourceSectionId?: string;
	label: SearchableSettingKeyword;
	keywords: Array<SearchableSettingKeyword>;
	description?: SearchableSettingKeyword;
	isVisible?: () => boolean;
}
