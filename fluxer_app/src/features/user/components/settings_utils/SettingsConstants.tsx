// SPDX-License-Identifier: AGPL-3.0-or-later

import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import PremiumState from '@app/features/premium/state/PremiumState';
import {
	hasPaymentHistory,
	hasSubscriptionToManage,
	holdsOperatorPass,
} from '@app/features/premium/utils/BillingTabVisibility';
import {isOperatorPassAvailable} from '@app/features/premium/utils/OperatorPassUtils';
import {getPremiumProductFullName, shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
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
	type SettingsSectionConfig,
	type SettingsSectionId,
	type UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {shouldShowClaimedAccountSettings} from '@app/features/user/components/settings_utils/SettingsVisibility';
import {
	ACCESSIBILITY_DESCRIPTOR,
	ACCOUNT_DESCRIPTOR as ACCOUNT_SECURITY_DESCRIPTOR,
	APPLICATIONS_DESCRIPTOR,
	AUTHORIZED_APPS_DESCRIPTOR,
	BLOCKED_USERS_DESCRIPTOR,
	DESKTOP_SETTINGS_FALLBACK_DESCRIPTOR as DESKTOP_SETTINGS_DESCRIPTOR,
	DESKTOP_SETTINGS_FALLBACK_DESCRIPTOR,
	EMBED_DEBUGGER_DESCRIPTOR,
	KEYBINDS_DESCRIPTOR,
	LANGUAGE_DESCRIPTOR as LANGUAGE_AND_TIME_DESCRIPTOR,
	DEVICES_DESCRIPTOR as LINKED_DEVICES_DESCRIPTOR,
	PRIVACY_DESCRIPTOR as PRIVACY_SAFETY_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import Users from '@app/features/user/state/Users';
import {
	GIFTS_DESCRIPTOR as GIFTS_AND_CODES_DESCRIPTOR,
	LOOK_AND_FEEL_DESCRIPTOR,
	MESSAGES_AND_MEDIA_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {
	ArrowsClockwiseIcon,
	AtomIcon,
	BellIcon,
	ChatCircleIcon,
	CodeIcon,
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
	ReceiptIcon,
	RobotIcon,
	ShieldIcon,
	SwatchesIcon,
	TicketIcon,
	TranslateIcon,
	UserIcon,
	UserListIcon,
} from '@phosphor-icons/react';
import type React from 'react';

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
const PROFILE_DESCRIPTOR = msg({
	message: 'Profile',
	comment: 'User settings tab for editing the current user profile.',
});

const SUBSCRIPTIONS_DESCRIPTOR = msg({
	message: 'Subscriptions',
	comment:
		'User settings tab under Billing where the user manages a Plutonium subscription, its renewal and cancellation.',
});
const PAYMENTS_DESCRIPTOR = msg({
	message: 'Payments',
	comment:
		'User settings tab under Billing that holds the payment methods on file, refunds and the receipts for past payments.',
});

const CONNECTIONS_DESCRIPTOR = msg({
	message: 'Connections',
	context: 'user-settings-tab',
	comment: 'User settings tab for linked external accounts.',
});

const AUDIO_AND_VIDEO_DESCRIPTOR = msg({
	message: 'Voice & video',
	comment: 'User settings tab for microphone, speaker, camera, and call stats.',
});
const NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Notifications',
	context: 'user-settings-tab',
	comment: 'User settings tab for notification and alert sound controls.',
});
const ADVANCED_SETTINGS_DESCRIPTOR = msg({
	message: 'Advanced',
	comment: 'User settings tab for power-user and experimental application preferences.',
});
const THEMES_DESCRIPTOR = msg({
	message: 'Themes',
	comment: 'User settings tab for the base theme, theme files, colors, fonts and quick CSS.',
});
const DESIGN_SYSTEM_DESCRIPTOR = msg({
	message: 'Design system',
	comment: 'Developer settings tab showing UI component examples.',
});
export type UserSettingsSubtabType = SettingsSectionId;
type UserSettingsTabCategories = 'user_settings' | 'billing' | 'app_settings' | 'developer';

export interface SettingsTab {
	type: UserSettingsTabType;
	category: UserSettingsTabCategories;
	label: string;
	icon: Icon;
	iconWeight?: IconWeight;
}

type SettingsTabLabel = MessageDescriptor | string | (() => string);

interface SettingsTabDescriptor {
	type: UserSettingsTabType;
	category: UserSettingsTabCategories;
	label: SettingsTabLabel;
	icon: Icon;
	iconWeight?: IconWeight;
	isVisible?: () => boolean;
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
		isVisible: shouldShowClaimedAccountSettings,
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
		isVisible: shouldShowClaimedAccountSettings,
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
		label: getPremiumProductFullName,
		icon: AtomIcon,
		isVisible: shouldShowPremiumFeatures,
	},
	{
		type: 'subscriptions',
		category: 'billing',
		label: SUBSCRIPTIONS_DESCRIPTOR,
		icon: ArrowsClockwiseIcon,
		isVisible: () => shouldShowPremiumFeatures() && hasSubscriptionToManage(PremiumState, Users.currentUser),
	},
	{
		type: 'operator_pass',
		category: 'billing',
		label: OPERATOR_PASS_NAME,
		icon: TicketIcon,
		isVisible: () => isOperatorPassAvailable() && holdsOperatorPass(Users.currentUser, OperatorPassHoldings.passes),
	},
	{
		type: 'gift_inventory',
		category: 'billing',
		label: GIFTS_AND_CODES_DESCRIPTOR,
		icon: GiftIcon,
		isVisible: shouldShowPremiumFeatures,
	},
	{
		type: 'payments',
		category: 'billing',
		label: PAYMENTS_DESCRIPTOR,
		icon: ReceiptIcon,
		isVisible: () => shouldShowPremiumFeatures() && hasPaymentHistory(PremiumState, Users.currentUser),
	},
	{
		type: 'appearance',
		category: 'app_settings',
		label: LOOK_AND_FEEL_DESCRIPTOR,
		icon: PaintBrushIcon,
	},
	{
		type: 'themes',
		category: 'app_settings',
		label: THEMES_DESCRIPTOR,
		icon: SwatchesIcon,
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
		label: KEYBINDS_DESCRIPTOR,
		icon: KeyboardIcon,
	},
	{
		type: 'desktop_settings',
		category: 'app_settings',
		label: DESKTOP_SETTINGS_DESCRIPTOR,
		icon: DesktopIcon,
		isVisible: isDesktop,
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
export const USER_SETTINGS_PRIVACY_REVIEW_NEEDED_DESCRIPTOR = msg({
	message: 'User settings, privacy review needed',
	comment: 'Accessible label for the user settings button while the user has not yet reviewed who can message them.',
});
export const PRIVACY_REVIEW_NEEDED_DESCRIPTOR = msg({
	message: 'Review needed',
	comment:
		'Screen reader text read after the Privacy item in the settings sidebar while the user has not yet reviewed who can message them. A red dot marks the item visually.',
});

export function getUserSettingsTabLabel(i18n: I18n, tabType: UserSettingsTabType): string {
	const tab = ALL_TABS_DESCRIPTORS.find((candidate) => candidate.type === tabType);
	if (!tab) return '';
	if (tab.type === 'desktop_settings') return getDesktopSettingsTabLabel(i18n);
	if (typeof tab.label === 'string') return tab.label;
	if (typeof tab.label === 'function') return tab.label();
	return i18n._(tab.label);
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

export const getSettingsTabs = (i18n: I18n): Array<SettingsTab> =>
	ALL_TABS_DESCRIPTORS.filter(
		(tab) => !ACCOUNT_NESTED_TAB_TYPES.some((tabType) => tabType === tab.type) && (tab.isVisible?.() ?? true),
	).map((tab) => ({
		type: tab.type,
		category: tab.category,
		label: getUserSettingsTabLabel(i18n, tab.type),
		icon: tab.icon,
		iconWeight: tab.iconWeight,
	}));

export interface SettingsSubtab {
	type: UserSettingsSubtabType;
	parentTab: UserSettingsTabType;
	label: string;
}

function getDesktopSettingsTabLabel(i18n: I18n): string {
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
	return registryGetAllSectionDefinitions()
		.filter((section) => !('isVisible' in section && section.isVisible && !section.isVisible()))
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

export function tabHasMultipleLinkableSections(tabType: UserSettingsTabType): boolean {
	return registryTabHasMultipleLinkableSections(tabType);
}

export function getSectionIdsForTab(tabType: UserSettingsTabType): Array<string> {
	return registryGetSectionIdsForTab(tabType);
}
