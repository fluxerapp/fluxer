/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {
	BellIcon,
	ChatCircleIcon,
	CodeIcon,
	CrownIcon,
	DevicesIcon,
	EyeSlashIcon,
	FlagIcon,
	FlaskIcon,
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
	StickerIcon,
	TicketIcon,
	TranslateIcon,
	UserIcon,
} from '@phosphor-icons/react';
import type React from 'react';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';

export type UserSettingsTabType =
	| 'my_profile'
	| 'account_security'
	| 'beta_codes'
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
	| 'advanced'
	| 'applications'
	| 'developer_options'
	| 'component_gallery'
	| 'language'
	| 'keybinds'
	| 'expression_packs'
	| 'feature_flags';

export type AppearanceTabType = 'theme' | 'messages' | 'interface' | 'favorites';
export type AccessibilityTabType = 'visual' | 'keyboard' | 'animation' | 'motion';
export type ChatTab = 'display' | 'media' | 'input' | 'interaction';
export type VoiceVideoTabType = 'voice' | 'video';
export type PrivacySafetyTabType = 'connections' | 'communication' | 'data-export' | 'data-deletion';
export type AccountSecurityTabType = 'account' | 'security' | 'danger_zone';
export type DevTab = 'general' | 'account_premium' | 'mocking' | 'nagbars' | 'tools' | 'typography';
export type ComponentGalleryTabType = 'buttons' | 'inputs' | 'selections' | 'overlays' | 'indicators' | 'markdown';

export type UserSettingsSubtabType =
	| AppearanceTabType
	| AccessibilityTabType
	| ChatTab
	| VoiceVideoTabType
	| PrivacySafetyTabType
	| AccountSecurityTabType
	| DevTab
	| ComponentGalleryTabType;

type UserSettingsTabCategories = 'user_settings' | 'app_settings' | 'developer' | 'staff_only';

export interface SettingsTab {
	type: UserSettingsTabType;
	category: UserSettingsTabCategories;
	label: string;
	icon: Icon;
	iconWeight?: IconWeight;
}

interface SettingsTabDescriptor {
	type: UserSettingsTabType;
	category: UserSettingsTabCategories;
	label: MessageDescriptor;
	icon: Icon;
	iconWeight?: IconWeight;
}

export function getCategoryLabel(category: UserSettingsTabCategories): React.ReactElement {
	switch (category) {
		case 'user_settings':
			return <Trans>Your Account</Trans>;
		case 'app_settings':
			return <Trans>Application</Trans>;
		case 'developer':
			return <Trans>Developer</Trans>;
		case 'staff_only':
			return <Trans>Staff-Only</Trans>;
	}
}

const ALL_TABS_DESCRIPTORS: Array<SettingsTabDescriptor> = [
	{
		type: 'my_profile',
		category: 'user_settings',
		label: msg`Profile`,
		icon: UserIcon,
	},
	{
		type: 'account_security',
		category: 'user_settings',
		label: msg`Security & Login`,
		icon: ShieldIcon,
	},
	{
		type: 'beta_codes',
		category: 'user_settings',
		label: msg`Early Access`,
		icon: TicketIcon,
	},
	{
		type: 'plutonium',
		category: 'user_settings',
		label: msg`Fluxer Plutonium`,
		icon: CrownIcon,
	},
	{
		type: 'gift_inventory',
		category: 'user_settings',
		label: msg`Gifts & Codes`,
		icon: GiftIcon,
	},
	{
		type: 'expression_packs',
		category: 'user_settings',
		label: msg`Expression Packs`,
		icon: StickerIcon,
	},
	{
		type: 'privacy_safety',
		category: 'user_settings',
		label: msg`Privacy Dashboard`,
		icon: EyeSlashIcon,
	},
	{
		type: 'authorized_apps',
		category: 'user_settings',
		label: msg`Authorized Apps`,
		icon: RobotIcon,
	},
	{
		type: 'blocked_users',
		category: 'user_settings',
		label: msg`Blocked Users`,
		icon: ProhibitIcon,
	},
	{
		type: 'devices',
		category: 'user_settings',
		label: msg`Linked Devices`,
		icon: DevicesIcon,
	},
	{
		type: 'appearance',
		category: 'app_settings',
		label: msg`Look & Feel`,
		icon: PaintBrushIcon,
	},
	{
		type: 'accessibility',
		category: 'app_settings',
		label: msg`Accessibility`,
		icon: PersonSimpleCircleIcon,
	},
	{
		type: 'chat_settings',
		category: 'app_settings',
		label: msg`Messages & Media`,
		icon: ChatCircleIcon,
	},
	{
		type: 'voice_video',
		category: 'app_settings',
		label: msg`Audio & Video`,
		icon: MicrophoneIcon,
	},
	{
		type: 'keybinds',
		category: 'app_settings',
		label: msg`Keybinds`,
		icon: KeyboardIcon,
	},
	{
		type: 'notifications',
		category: 'app_settings',
		label: msg`Sounds & Alerts`,
		icon: BellIcon,
	},
	{
		type: 'language',
		category: 'app_settings',
		label: msg`Languge & Time`,
		icon: TranslateIcon,
		iconWeight: 'bold',
	},
	{
		type: 'advanced',
		category: 'app_settings',
		label: msg`Advanced`,
		icon: FlaskIcon,
	},
	{
		type: 'applications',
		category: 'developer',
		label: msg`Applications`,
		icon: CodeIcon,
		iconWeight: 'bold',
	},
	{
		type: 'developer_options',
		category: 'staff_only',
		label: msg`Developer Tools`,
		icon: CodeIcon,
		iconWeight: 'bold',
	},
	{
		type: 'feature_flags',
		category: 'staff_only',
		label: msg`Feature Flags`,
		icon: FlagIcon,
	},
	{
		type: 'component_gallery',
		category: 'staff_only',
		label: msg`UI Kit`,
		icon: PaletteIcon,
	},
];

export const getSettingsTabs = (t: (msg: MessageDescriptor) => string): Array<SettingsTab> => {
	const allTabs = ALL_TABS_DESCRIPTORS.map((tab) => ({
		...tab,
		label: t(tab.label),
	}));

	return RuntimeConfigStore.isSelfHosted()
		? allTabs.filter((tab) => tab.type !== 'plutonium' && tab.type !== 'gift_inventory')
		: allTabs;
};

export interface SettingsSubtab {
	type: UserSettingsSubtabType;
	parentTab: UserSettingsTabType;
	label: string;
}

interface SettingsSubtabDescriptor {
	type: UserSettingsSubtabType;
	parentTab: UserSettingsTabType;
	label: MessageDescriptor;
}

const SETTINGS_SUBTABS_DESCRIPTORS: Array<SettingsSubtabDescriptor> = [
	{type: 'theme', parentTab: 'appearance', label: msg`Theme`},
	{type: 'messages', parentTab: 'appearance', label: msg`Messages`},
	{type: 'interface', parentTab: 'appearance', label: msg`Interface`},
	{type: 'favorites', parentTab: 'appearance', label: msg`Favorites`},

	{type: 'visual', parentTab: 'accessibility', label: msg`Visual`},
	{type: 'keyboard', parentTab: 'accessibility', label: msg`Keyboard`},
	{type: 'animation', parentTab: 'accessibility', label: msg`Animation`},
	{type: 'motion', parentTab: 'accessibility', label: msg`Motion`},

	{type: 'display', parentTab: 'chat_settings', label: msg`Display`},
	{type: 'media', parentTab: 'chat_settings', label: msg`Media`},
	{type: 'input', parentTab: 'chat_settings', label: msg`Input`},
	{type: 'interaction', parentTab: 'chat_settings', label: msg`Interaction`},

	{type: 'voice', parentTab: 'voice_video', label: msg`Audio`},
	{type: 'video', parentTab: 'voice_video', label: msg`Video`},

	{type: 'connections', parentTab: 'privacy_safety', label: msg`Connections`},
	{type: 'communication', parentTab: 'privacy_safety', label: msg`Communication`},

	{type: 'account', parentTab: 'account_security', label: msg`Account`},
	{type: 'security', parentTab: 'account_security', label: msg`Security`},
	{type: 'danger_zone', parentTab: 'account_security', label: msg`Danger Zone`},

	{type: 'general', parentTab: 'developer_options', label: msg`General`},
	{type: 'account_premium', parentTab: 'developer_options', label: msg`Account & Premium`},
	{type: 'mocking', parentTab: 'developer_options', label: msg`Mocking`},
	{type: 'nagbars', parentTab: 'developer_options', label: msg`Nagbars`},
	{type: 'tools', parentTab: 'developer_options', label: msg`Tools`},
	{type: 'typography', parentTab: 'developer_options', label: msg`Typography`},

	{type: 'buttons', parentTab: 'component_gallery', label: msg`Buttons`},
	{type: 'inputs', parentTab: 'component_gallery', label: msg`Inputs & Text`},
	{type: 'selections', parentTab: 'component_gallery', label: msg`Selections`},
	{type: 'overlays', parentTab: 'component_gallery', label: msg`Overlays & Menus`},
	{type: 'indicators', parentTab: 'component_gallery', label: msg`Indicators & Status`},
	{type: 'markdown', parentTab: 'component_gallery', label: msg`Markdown`},
];

export const getSettingsSubtabs = (t: (msg: MessageDescriptor) => string): Array<SettingsSubtab> => {
	return SETTINGS_SUBTABS_DESCRIPTORS.map((subtab) => ({
		...subtab,
		label: t(subtab.label),
	}));
};

export function getSubtabsForTab(
	tabType: UserSettingsTabType,
	t: (msg: MessageDescriptor) => string,
): Array<SettingsSubtab> {
	return getSettingsSubtabs(t).filter((subtab) => subtab.parentTab === tabType);
}

export interface SettingsSectionConfig {
	id: string;
	label: string;
	isAdvanced: boolean;
}

interface SettingsSectionConfigDescriptor {
	id: string;
	label: MessageDescriptor;
	isAdvanced: boolean;
}

const SETTINGS_SECTIONS_MAP_DESCRIPTORS: Partial<Record<UserSettingsTabType, Array<SettingsSectionConfigDescriptor>>> =
	{
		appearance: [
			{id: 'theme', label: msg`Theme`, isAdvanced: false},
			{id: 'messages', label: msg`Messages`, isAdvanced: false},
			{id: 'interface', label: msg`Interface`, isAdvanced: false},
			{id: 'favorites', label: msg`Favorites`, isAdvanced: true},
		],
		accessibility: [
			{id: 'visual', label: msg`Visual`, isAdvanced: false},
			{id: 'keyboard', label: msg`Keyboard`, isAdvanced: false},
			{id: 'animation', label: msg`Animation`, isAdvanced: false},
			{id: 'motion', label: msg`Motion`, isAdvanced: true},
		],
		chat_settings: [
			{id: 'display', label: msg`Display`, isAdvanced: false},
			{id: 'media', label: msg`Media`, isAdvanced: false},
			{id: 'input', label: msg`Input`, isAdvanced: false},
			{id: 'interaction', label: msg`Interaction`, isAdvanced: true},
		],
		voice_video: [
			{id: 'voice', label: msg`Audio`, isAdvanced: false},
			{id: 'video', label: msg`Video`, isAdvanced: false},
		],
		privacy_safety: [
			{id: 'connections', label: msg`Connections`, isAdvanced: false},
			{id: 'communication', label: msg`Communication`, isAdvanced: false},
			{id: 'data-export', label: msg`Data Export`, isAdvanced: true},
			{id: 'data-deletion', label: msg`Data Deletion`, isAdvanced: true},
		],
		account_security: [
			{id: 'account', label: msg`Account`, isAdvanced: false},
			{id: 'security', label: msg`Security`, isAdvanced: false},
			{id: 'danger_zone', label: msg`Danger Zone`, isAdvanced: false},
		],
		notifications: [
			{id: 'notifications', label: msg`Notifications`, isAdvanced: false},
			{id: 'sounds', label: msg`Sounds`, isAdvanced: false},
			{id: 'push', label: msg`Push Settings`, isAdvanced: false},
		],
		developer_options: [
			{id: 'general', label: msg`General`, isAdvanced: false},
			{id: 'account_premium', label: msg`Account & Premium`, isAdvanced: false},
			{id: 'mocking', label: msg`Mocking`, isAdvanced: false},
			{id: 'nagbars', label: msg`Nagbars`, isAdvanced: false},
			{id: 'tools', label: msg`Tools`, isAdvanced: false},
			{id: 'typography', label: msg`Typography`, isAdvanced: false},
		],
		component_gallery: [
			{id: 'buttons', label: msg`Buttons`, isAdvanced: false},
			{id: 'inputs', label: msg`Inputs & Text`, isAdvanced: false},
			{id: 'selections', label: msg`Selections`, isAdvanced: false},
			{id: 'overlays', label: msg`Overlays & Menus`, isAdvanced: false},
			{id: 'indicators', label: msg`Indicators & Status`, isAdvanced: false},
			{id: 'status', label: msg`Status Slate`, isAdvanced: false},
			{id: 'markdown', label: msg`Markdown`, isAdvanced: false},
		],
	};

export function getSectionsForTab(
	tabType: UserSettingsTabType,
	t: (msg: MessageDescriptor) => string,
): Array<SettingsSectionConfig> {
	const sections = SETTINGS_SECTIONS_MAP_DESCRIPTORS[tabType];
	if (!sections) return [];

	return sections.map((section) => ({
		...section,
		label: t(section.label),
	}));
}

export function tabHasSections(tabType: UserSettingsTabType): boolean {
	return SETTINGS_SECTIONS_MAP_DESCRIPTORS[tabType] !== undefined;
}

export function getSectionIdsForTab(tabType: UserSettingsTabType): Array<string> {
	const sections = SETTINGS_SECTIONS_MAP_DESCRIPTORS[tabType];
	if (!sections) return [];

	return sections.map((section) => section.id);
}
