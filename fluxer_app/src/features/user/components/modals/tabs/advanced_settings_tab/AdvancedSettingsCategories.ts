// SPDX-License-Identifier: AGPL-3.0-or-later

import {MEDIA_DESCRIPTOR as MEDIA_TAG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {SettingsCategoryTag} from '@app/features/user/components/settings_utils/SettingsMetadata';
import type {
	SearchableSettingItem,
	UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {
	ACCESSIBILITY_DESCRIPTOR as ACCESSIBILITY_TAG_DESCRIPTOR,
	ACCOUNT_DESCRIPTOR as ACCOUNT_TAG_DESCRIPTOR,
	PRIVACY_DESCRIPTOR as PRIVACY_TAG_DESCRIPTOR,
	VOICE_DESCRIPTOR as VOICE_TAG_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {
	LOOK_AND_FEEL_DESCRIPTOR as APPEARANCE_TAG_DESCRIPTOR,
	MESSAGES_AND_MEDIA_DESCRIPTOR as CHAT_TAG_DESCRIPTOR,
	DESKTOP_DESCRIPTOR as DESKTOP_TAG_DESCRIPTOR,
	DEVELOPER_DESCRIPTOR as DEVELOPER_TAG_DESCRIPTOR,
	NOTIFICATIONS_2_DESCRIPTOR as NOTIFICATIONS_TAG_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import type {MessageDescriptor} from '@lingui/core';

export const ADVANCED_SETTINGS_TAG_ORDER: ReadonlyArray<SettingsCategoryTag> = [
	'account',
	'privacy',
	'appearance',
	'accessibility',
	'chat',
	'media',
	'voice',
	'notifications',
	'desktop',
	'developer',
];

export const ADVANCED_SETTINGS_TAG_LABELS = {
	account: ACCOUNT_TAG_DESCRIPTOR,
	privacy: PRIVACY_TAG_DESCRIPTOR,
	appearance: APPEARANCE_TAG_DESCRIPTOR,
	accessibility: ACCESSIBILITY_TAG_DESCRIPTOR,
	chat: CHAT_TAG_DESCRIPTOR,
	media: MEDIA_TAG_DESCRIPTOR,
	voice: VOICE_TAG_DESCRIPTOR,
	notifications: NOTIFICATIONS_TAG_DESCRIPTOR,
	desktop: DESKTOP_TAG_DESCRIPTOR,
	developer: DEVELOPER_TAG_DESCRIPTOR,
} satisfies Record<SettingsCategoryTag, MessageDescriptor>;

const SOURCE_TAB_CATEGORY: Partial<Record<UserSettingsTabType, SettingsCategoryTag>> = {
	my_profile: 'account',
	account_security: 'account',
	devices: 'account',
	linked_accounts: 'account',
	plutonium: 'account',
	gift_inventory: 'account',
	privacy_safety: 'privacy',
	authorized_apps: 'privacy',
	blocked_users: 'privacy',
	appearance: 'appearance',
	accessibility: 'accessibility',
	chat_settings: 'chat',
	voice_video: 'voice',
	notifications: 'notifications',
	desktop_settings: 'desktop',
	client_developer_settings: 'developer',
};

export interface AdvancedSettingsCategoryGroup {
	key: SettingsCategoryTag;
	label: string;
	items: Array<SearchableSettingItem>;
}

export function getAdvancedSettingsCategorySectionId(tag: SettingsCategoryTag): string {
	return `advanced-settings-${tag}`;
}

export function isAdvancedSettingsCategorySectionId(sectionId: string): boolean {
	return ADVANCED_SETTINGS_TAG_ORDER.some((tag) => getAdvancedSettingsCategorySectionId(tag) === sectionId);
}

function getItemSourceTab(item: SearchableSettingItem): UserSettingsTabType {
	return item.sourceTabType ?? item.tabType;
}

export function getAdvancedSettingsCategory(item: SearchableSettingItem): SettingsCategoryTag {
	const explicitTag = ADVANCED_SETTINGS_TAG_ORDER.find((tag) => item.tags?.includes(tag));
	if (explicitTag) return explicitTag;
	return SOURCE_TAB_CATEGORY[getItemSourceTab(item)] ?? 'account';
}

export function groupAdvancedItemsByCategory(
	items: Array<SearchableSettingItem>,
	tagLabels: Record<SettingsCategoryTag, string>,
): Array<AdvancedSettingsCategoryGroup> {
	const groups = new Map<SettingsCategoryTag, AdvancedSettingsCategoryGroup>();
	for (const item of items) {
		const tag = getAdvancedSettingsCategory(item);
		const existing = groups.get(tag);
		if (existing) {
			existing.items.push(item);
			continue;
		}
		groups.set(tag, {
			key: tag,
			label: tagLabels[tag],
			items: [item],
		});
	}
	return ADVANCED_SETTINGS_TAG_ORDER.flatMap((tag) => {
		const group = groups.get(tag);
		return group ? [group] : [];
	});
}
