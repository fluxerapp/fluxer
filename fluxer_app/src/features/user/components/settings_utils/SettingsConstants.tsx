// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_FULL_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {guessPlatform, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {
	ADVANCED_SETTINGS_TAG_LABELS,
	isAdvancedSettingsCategorySectionId,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsCategories';
import {ACCOUNT_NESTED_TAB_TYPES} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import {
	getAllSectionDefinitions as registryGetAllSectionDefinitions,
	getSectionDefinition as registryGetSectionDefinition,
	getSectionIdsForTab as registryGetSectionIdsForTab,
	getSectionsForTab as registryGetSectionsForTab,
	getVisibleSectionsForTab as registryGetVisibleSectionsForTab,
	tabHasMultipleLinkableSections as registryTabHasMultipleLinkableSections,
	tabHasSections as registryTabHasSections,
	type SettingsSectionConfig,
	type SettingsSectionId,
	type UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import Users from '@app/features/user/state/Users';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {
	BellIcon,
	ChatCircleIcon,
	CodeIcon,
	CrownIcon,
	DesktopIcon,
	DevicesIcon,
	EyeSlashIcon,
	GearIcon,
	GiftIcon,
	type Icon,
	type IconWeight,
	KeyboardIcon,
	MicrophoneIcon,
	PaintBrushIcon,
	PaletteIcon,
	PersonSimpleCircleIcon,
	ProhibitIcon,
	RobotIcon,
	ShieldIcon,
	TranslateIcon,
	UserIcon,
	UserListIcon,
} from '@phosphor-icons/react';
import type React from 'react';

const BLOCKED_USERS_DESCRIPTOR = msg({
	message: 'Blocked users',
	comment: 'User settings tab listing accounts the current user has blocked.',
});
const LINKED_DEVICES_DESCRIPTOR = msg({
	message: 'Devices',
	comment: 'User settings tab for managing devices linked to the current account.',
});
const ACCESSIBILITY_DESCRIPTOR = msg({
	message: 'Accessibility',
	comment: 'User settings tab for accessibility preferences (visual, motion, screen reader, etc.).',
});
const APPLICATIONS_DESCRIPTOR = msg({
	message: 'Applications',
	comment: 'Developer settings tab for managing the current user developer applications and bots.',
});
const WINDOWS_SETTINGS_DESCRIPTOR = msg({
	message: 'Windows app',
	comment: 'User settings tab label shown on Windows desktop builds. Refers to the Microsoft Windows OS.',
});
const LINUX_SETTINGS_DESCRIPTOR = msg({
	message: 'Linux app',
	comment: 'User settings tab label shown on Linux desktop builds.',
});
const MACOS_SETTINGS_DESCRIPTOR = msg({
	message: 'macOS app',
	comment: 'User settings tab label shown on macOS desktop builds.',
});
const DESKTOP_SETTINGS_FALLBACK_DESCRIPTOR = msg({
	message: 'Desktop app',
	comment:
		'User settings tab label shown on desktop builds when the OS could not be identified (fallback for the per-OS labels).',
});
const PROFILE_DESCRIPTOR = msg({
	message: 'Profile',
	comment: 'User settings tab for editing the current user profile.',
});
const ACCOUNT_SECURITY_DESCRIPTOR = msg({
	message: 'Account',
	comment: 'User settings tab for account login, password, MFA, and security.',
});
const GIFTS_AND_CODES_DESCRIPTOR = msg({
	message: 'Gifts',
	comment: 'User settings tab for gift inventory and redemption codes.',
});
const PRIVACY_SAFETY_DESCRIPTOR = msg({
	message: 'Privacy',
	comment: 'User settings tab for privacy and safety controls.',
});
const AUTHORIZED_APPS_DESCRIPTOR = msg({
	message: 'Authorized apps',
	comment: 'User settings tab for OAuth or connected apps authorized by the user.',
});
const CONNECTIONS_DESCRIPTOR = msg({
	message: 'Connections',
	context: 'user-settings-tab',
	comment: 'User settings tab for linked external accounts.',
});
const LOOK_AND_FEEL_DESCRIPTOR = msg({
	message: 'Appearance',
	comment: 'User settings tab for app appearance and layout options.',
});
const MESSAGES_AND_MEDIA_DESCRIPTOR = msg({
	message: 'Chat',
	comment: 'User settings tab for chat message, media, and input behavior.',
});
const AUDIO_AND_VIDEO_DESCRIPTOR = msg({
	message: 'Voice & video',
	comment: 'User settings tab for microphone, speaker, camera, and call stats.',
});
const SHORTCUTS_DESCRIPTOR = msg({
	message: 'Shortcuts',
	comment: 'User settings tab for keyboard shortcuts and custom bindings.',
});
const NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Notifications',
	context: 'user-settings-tab',
	comment: 'User settings tab for notification and alert sound controls.',
});
const LANGUAGE_AND_TIME_DESCRIPTOR = msg({
	message: 'Language',
	comment: 'User settings tab for language, locale, and time display preferences.',
});
const DESKTOP_SETTINGS_DESCRIPTOR = msg({
	message: 'Desktop app',
	comment: 'User settings tab for desktop app integration and platform settings.',
});
const ADVANCED_SETTINGS_DESCRIPTOR = msg({
	message: 'Advanced',
	comment: 'User settings tab for power-user and experimental application preferences.',
});
const EMBED_DEBUGGER_DESCRIPTOR = msg({
	message: 'Embed debugger',
	comment: 'Developer settings tab for testing URL unfurls and rendered embeds.',
});
const DESIGN_SYSTEM_DESCRIPTOR = msg({
	message: 'Design system',
	comment: 'Developer settings tab showing UI component examples.',
});

export type AppearanceTabType =
	| 'theme'
	| 'hdr'
	| 'chat-font-scaling'
	| 'app-zoom-level'
	| 'streamer-mode'
	| 'messages'
	| 'interface'
	| 'channel-list'
	| 'active-now';
export type AccessibilityTabType = 'visual' | 'screen-reader' | 'tts' | 'keyboard' | 'animation' | 'motion';
export type ChatTab = 'display' | 'media' | 'input';
export type VoiceVideoTabType = 'audio' | 'video';
export type PrivacySafetyTabType =
	| 'profile-privacy'
	| 'connections'
	| 'communication'
	| 'active-now'
	| 'sensitive-content'
	| 'data-export'
	| 'data-deletion';
export type AccountSecurityTabType = 'account' | 'security' | 'danger_zone';
export type NotificationsTabType = 'notifications' | 'mention-preference' | 'sounds' | 'text-to-speech' | 'push';
export type UserSettingsSubtabType = SettingsSectionId;
type UserSettingsTabCategories = 'user_settings' | 'billing' | 'app_settings' | 'developer';

export interface SettingsTab {
	type: UserSettingsTabType;
	category: UserSettingsTabCategories;
	label: string;
	icon: Icon;
	iconWeight?: IconWeight;
}

type SettingsTabLabel = MessageDescriptor | string;

interface SettingsTabDescriptor {
	type: UserSettingsTabType;
	category: UserSettingsTabCategories;
	label: SettingsTabLabel;
	icon: Icon;
	iconWeight?: IconWeight;
}

export function getCategoryLabel(category: UserSettingsTabCategories): React.ReactElement {
	switch (category) {
		case 'user_settings':
			return <Trans>Your account</Trans>;
		case 'billing':
			return <Trans>Billing</Trans>;
		case 'app_settings':
			return <Trans>App settings</Trans>;
		case 'developer':
			return <Trans>Developer</Trans>;
	}
}

const ALL_TABS_DESCRIPTORS: Array<SettingsTabDescriptor> = [
	{
		type: 'my_profile',
		category: 'user_settings',
		label: PROFILE_DESCRIPTOR,
		icon: UserIcon,
	},
	{
		type: 'account_security',
		category: 'user_settings',
		label: ACCOUNT_SECURITY_DESCRIPTOR,
		icon: ShieldIcon,
	},
	{
		type: 'privacy_safety',
		category: 'user_settings',
		label: PRIVACY_SAFETY_DESCRIPTOR,
		icon: EyeSlashIcon,
	},
	{
		type: 'linked_accounts',
		category: 'user_settings',
		label: CONNECTIONS_DESCRIPTOR,
		icon: UserListIcon,
	},
	{
		type: 'authorized_apps',
		category: 'user_settings',
		label: AUTHORIZED_APPS_DESCRIPTOR,
		icon: RobotIcon,
	},
	{
		type: 'blocked_users',
		category: 'user_settings',
		label: BLOCKED_USERS_DESCRIPTOR,
		icon: ProhibitIcon,
	},
	{
		type: 'devices',
		category: 'user_settings',
		label: LINKED_DEVICES_DESCRIPTOR,
		icon: DevicesIcon,
	},
	{
		type: 'plutonium',
		category: 'billing',
		label: PREMIUM_PRODUCT_FULL_NAME,
		icon: CrownIcon,
	},
	{
		type: 'gift_inventory',
		category: 'billing',
		label: GIFTS_AND_CODES_DESCRIPTOR,
		icon: GiftIcon,
	},
	{
		type: 'appearance',
		category: 'app_settings',
		label: LOOK_AND_FEEL_DESCRIPTOR,
		icon: PaintBrushIcon,
	},
	{
		type: 'notifications',
		category: 'app_settings',
		label: NOTIFICATIONS_DESCRIPTOR,
		icon: BellIcon,
	},
	{
		type: 'chat_settings',
		category: 'app_settings',
		label: MESSAGES_AND_MEDIA_DESCRIPTOR,
		icon: ChatCircleIcon,
	},
	{
		type: 'voice_video',
		category: 'app_settings',
		label: AUDIO_AND_VIDEO_DESCRIPTOR,
		icon: MicrophoneIcon,
	},
	{
		type: 'accessibility',
		category: 'app_settings',
		label: ACCESSIBILITY_DESCRIPTOR,
		icon: PersonSimpleCircleIcon,
	},
	{
		type: 'language',
		category: 'app_settings',
		label: LANGUAGE_AND_TIME_DESCRIPTOR,
		icon: TranslateIcon,
		iconWeight: 'bold',
	},
	{
		type: 'keybinds',
		category: 'app_settings',
		label: SHORTCUTS_DESCRIPTOR,
		icon: KeyboardIcon,
	},
	{
		type: 'desktop_settings',
		category: 'app_settings',
		label: DESKTOP_SETTINGS_DESCRIPTOR,
		icon: DesktopIcon,
	},
	{
		type: 'advanced_settings',
		category: 'app_settings',
		label: ADVANCED_SETTINGS_DESCRIPTOR,
		icon: GearIcon,
	},
	{
		type: 'applications',
		category: 'developer',
		label: APPLICATIONS_DESCRIPTOR,
		icon: CodeIcon,
		iconWeight: 'bold',
	},
	{
		type: 'embed_debugger',
		category: 'developer',
		label: EMBED_DEBUGGER_DESCRIPTOR,
		icon: CodeIcon,
		iconWeight: 'bold',
	},
	{
		type: 'component_gallery',
		category: 'developer',
		label: DESIGN_SYSTEM_DESCRIPTOR,
		icon: PaletteIcon,
	},
];
export const USER_SETTINGS_LABEL_DESCRIPTOR = msg({
	message: 'User settings',
	comment: 'Root label for the current user settings modal and settings search paths.',
});

export function getUserSettingsTabLabel(i18n: I18n, tabType: UserSettingsTabType): string {
	const tab = ALL_TABS_DESCRIPTORS.find((candidate) => candidate.type === tabType);
	if (!tab) return '';
	if (tab.type === 'desktop_settings') return getDesktopSettingsTabLabel(i18n);
	return typeof tab.label === 'string' ? tab.label : i18n._(tab.label);
}

export function getUserSettingsTabIconDescriptor(
	tabType: UserSettingsTabType,
): {icon: Icon; iconWeight?: IconWeight} | null {
	const tab = ALL_TABS_DESCRIPTORS.find((candidate) => candidate.type === tabType);
	return tab ? {icon: tab.icon, iconWeight: tab.iconWeight} : null;
}

export function getUserSettingsSectionLabel(
	i18n: I18n,
	sectionId: string,
	tabType?: UserSettingsTabType,
): string | null {
	if (tabType === 'advanced_settings' && isAdvancedSettingsCategorySectionId(sectionId)) {
		const tag = sectionId.slice('advanced-settings-'.length) as keyof typeof ADVANCED_SETTINGS_TAG_LABELS;
		const descriptor = ADVANCED_SETTINGS_TAG_LABELS[tag];
		if (descriptor) return i18n._(descriptor);
	}
	const section = tabType
		? registryGetAllSectionDefinitions().find(
				(candidate) => candidate.id === sectionId && candidate.tabType === tabType,
			)
		: registryGetSectionDefinition(sectionId);
	if (!section) return null;
	return i18n._(section.label);
}

export function formatUserSettingsPath(
	i18n: I18n,
	tabType: UserSettingsTabType,
	sectionId?: UserSettingsSubtabType,
): string {
	const parts = [i18n._(USER_SETTINGS_LABEL_DESCRIPTOR), getUserSettingsTabLabel(i18n, tabType)];
	if (sectionId) {
		const sectionLabel = getUserSettingsSectionLabel(i18n, sectionId, tabType);
		if (sectionLabel) parts.push(sectionLabel);
	}
	return parts.filter(Boolean).join(' > ');
}

export const getSettingsTabs = (i18n: I18n): Array<SettingsTab> => {
	const allTabs = ALL_TABS_DESCRIPTORS.map((tab) => ({
		...tab,
		label: getUserSettingsTabLabel(i18n, tab.type),
	}));
	const isSelfHosted = RuntimeConfig.isSelfHosted();
	const showClaimedAccountUi = shouldShowClaimedAccountUi();
	return allTabs.filter((tab) => {
		if (!showClaimedAccountUi && (tab.type === 'my_profile' || tab.type === 'linked_accounts')) {
			return false;
		}
		if (ACCOUNT_NESTED_TAB_TYPES.some((tabType) => tabType === tab.type)) {
			return false;
		}
		if (isSelfHosted && (tab.type === 'plutonium' || tab.type === 'gift_inventory')) {
			return false;
		}
		if (tab.type === 'desktop_settings' && !isDesktop()) {
			return false;
		}
		return true;
	});
};

export interface SettingsSubtab {
	type: UserSettingsSubtabType;
	parentTab: UserSettingsTabType;
	label: string;
}

function shouldShowClaimedAccountUi(): boolean {
	return Users.getCurrentUser()?.isClaimed() ?? true;
}

export function getDesktopSettingsTabLabel(i18n: I18n): string {
	switch (guessPlatform()) {
		case 'windows':
			return i18n._(WINDOWS_SETTINGS_DESCRIPTOR);
		case 'linux':
			return i18n._(LINUX_SETTINGS_DESCRIPTOR);
		case 'macos':
			return i18n._(MACOS_SETTINGS_DESCRIPTOR);
		default:
			return i18n._(DESKTOP_SETTINGS_FALLBACK_DESCRIPTOR);
	}
}

export const getSettingsSubtabs = (i18n: I18n): Array<SettingsSubtab> => {
	const showClaimedAccountUi = shouldShowClaimedAccountUi();
	return registryGetAllSectionDefinitions()
		.filter((section) => {
			if (!showClaimedAccountUi && (section.id === 'account' || section.id === 'security')) {
				return false;
			}
			if ('isVisible' in section && section.isVisible && !section.isVisible()) {
				return false;
			}
			return true;
		})
		.map((section) => ({
			type: section.id,
			parentTab: section.tabType,
			label: i18n._(section.label),
		}));
};

export function getSubtabsForTab(tabType: UserSettingsTabType, i18n: I18n): Array<SettingsSubtab> {
	return registryGetVisibleSectionsForTab(tabType).map((section) => ({
		type: section.id,
		parentTab: section.tabType,
		label: i18n._(section.label),
	}));
}

export function getSectionsForTab(tabType: UserSettingsTabType, i18n: I18n): Array<SettingsSectionConfig> {
	return registryGetSectionsForTab(tabType, i18n);
}

export function tabHasSections(tabType: UserSettingsTabType): boolean {
	return registryTabHasSections(tabType);
}

export function tabHasMultipleLinkableSections(tabType: UserSettingsTabType): boolean {
	return registryTabHasMultipleLinkableSections(tabType);
}

export function getSectionIdsForTab(tabType: UserSettingsTabType): Array<string> {
	return registryGetSectionIdsForTab(tabType);
}
