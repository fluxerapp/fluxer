// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	SettingsAudience,
	SettingsCategoryTag,
	SettingsMetadata,
	SettingsStatusBadgeKind,
} from '@app/features/user/components/settings_utils/SettingsMetadata';
import type {MessageDescriptor} from '@lingui/core';

export type SectionKeyword = MessageDescriptor | string;

export type UserSettingsTabType =
	| 'my_profile'
	| 'account_security'
	| 'plutonium'
	| 'gift_inventory'
	| 'privacy_safety'
	| 'authorized_apps'
	| 'blocked_users'
	| 'devices'
	| 'appearance'
	| 'accessibility'
	| 'chat_settings'
	| 'voice_video'
	| 'notifications'
	| 'desktop_settings'
	| 'advanced_settings'
	| 'client_developer_settings'
	| 'embed_debugger'
	| 'applications'
	| 'component_gallery'
	| 'language'
	| 'keybinds'
	| 'linked_accounts';

export interface SectionDefinition extends SettingsMetadata {
	id: string;
	tabType: UserSettingsTabType;
	label: MessageDescriptor;
	description?: MessageDescriptor;
	keywords: ReadonlyArray<SectionKeyword>;
	isAdvanced: boolean;
	isVisible?: () => boolean;
}

export interface SettingsSectionConfig {
	id: string;
	label: string;
	isAdvanced: boolean;
	audience?: SettingsAudience;
	tags?: ReadonlyArray<SettingsCategoryTag>;
	addedAt?: string;
	badges?: ReadonlyArray<SettingsStatusBadgeKind>;
}

export interface SearchableSettingItem extends SettingsMetadata {
	id: string;
	tabType: UserSettingsTabType;
	sourceTabType?: UserSettingsTabType;
	sectionId?: string;
	sourceSectionId?: string;
	label: string;
	keywords: Array<string>;
	description?: string;
}
