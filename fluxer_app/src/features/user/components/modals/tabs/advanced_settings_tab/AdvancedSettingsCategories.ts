// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SettingsCategoryTag} from '@app/features/user/components/settings_utils/SettingsMetadata';
import type {
	SearchableSettingItem,
	UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ACCOUNT_TAG_DESCRIPTOR = msg({message: 'Account', comment: 'Advanced settings category tag.'});
const PRIVACY_TAG_DESCRIPTOR = msg({message: 'Privacy', comment: 'Advanced settings category tag.'});
const APPEARANCE_TAG_DESCRIPTOR = msg({message: 'Appearance', comment: 'Advanced settings category tag.'});
const ACCESSIBILITY_TAG_DESCRIPTOR = msg({message: 'Accessibility', comment: 'Advanced settings category tag.'});
const CHAT_TAG_DESCRIPTOR = msg({message: 'Chat', comment: 'Advanced settings category tag.'});
const MEDIA_TAG_DESCRIPTOR = msg({message: 'Media', comment: 'Advanced settings category tag.'});
const VOICE_TAG_DESCRIPTOR = msg({message: 'Voice', comment: 'Advanced settings category tag.'});
const NOTIFICATIONS_TAG_DESCRIPTOR = msg({message: 'Notifications', comment: 'Advanced settings category tag.'});
const DESKTOP_TAG_DESCRIPTOR = msg({message: 'Desktop', comment: 'Advanced settings category tag.'});
const DEVELOPER_TAG_DESCRIPTOR = msg({message: 'Developer', comment: 'Advanced settings category tag.'});

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
