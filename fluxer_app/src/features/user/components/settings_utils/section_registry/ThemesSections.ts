// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	COLORS_DESCRIPTOR,
	DEVICES_DESCRIPTOR,
	FONTS_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {DARK_MODE_DESCRIPTOR, SYSTEM_THEME_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

export const THIS_ACCOUNT_SECTION_DESCRIPTOR = msg({
	message: 'This account',
	comment:
		'Section title in Settings > Themes on the desktop app, for choosing whether the current account uses the theme shared by every account.',
});
export const BASE_THEME_SECTION_DESCRIPTOR = msg({
	message: 'Base theme',
	comment: 'Section title in Settings > Themes for picking dark, light, coal or the system theme.',
});
export const YOUR_THEMES_SECTION_DESCRIPTOR = msg({
	message: 'Your themes',
	comment: 'Section title in Settings > Themes listing the CSS theme files the user imported.',
});
export const COLORS_AND_FONTS_SECTION_DESCRIPTOR = msg({
	message: 'Colors and fonts',
	comment: 'Section title in Settings > Themes for changing single colors and fonts of the base theme.',
});
export const QUICK_CSS_SECTION_DESCRIPTOR = msg({
	message: 'Quick CSS',
	comment: 'Section title in Settings > Themes. Quick CSS is custom CSS the user writes to restyle the app.',
});
export const ASSETS_SECTION_DESCRIPTOR = msg({
	message: 'Assets',
	comment: 'Section title in Settings > Themes for images and fonts the user uploads to use in custom CSS.',
});
export const DESKTOP_WINDOW_SECTION_DESCRIPTOR = msg({
	message: 'Desktop window',
	comment: 'Section title in Settings > Themes on the desktop app for window transparency.',
});
export const SHARE_AND_BACK_UP_SECTION_DESCRIPTOR = msg({
	message: 'Share and back up',
	comment: 'Section title in Settings > Themes for sharing a theme, exporting and importing the theme library.',
});

const THEME_DESCRIPTOR = msg({
	message: 'Theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LIGHT_MODE_DESCRIPTOR = msg({
	message: 'Light mode',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DARK_DESCRIPTOR = msg({
	message: 'Dark',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LIGHT_DESCRIPTOR = msg({
	message: 'Light',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COAL_DESCRIPTOR = msg({
	message: 'Coal',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COLOR_SCHEME_DESCRIPTOR = msg({
	message: 'Color scheme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYNC_DESCRIPTOR = msg({
	message: 'Sync',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYNC_THEME_DESCRIPTOR = msg({
	message: 'Sync theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_SYNC_DESCRIPTOR = msg({
	message: 'Theme sync',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DESKTOP_APP_THEME_KEYWORD_DESCRIPTOR = msg({
	message: 'Desktop app theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACCOUNTS_DESCRIPTOR = msg({
	message: 'Accounts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CSS_DESCRIPTOR = msg({
	message: 'CSS',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STYLING_DESCRIPTOR = msg({
	message: 'Styling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_LIBRARY_DESCRIPTOR = msg({
	message: 'Theme library',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_FILES_DESCRIPTOR = msg({
	message: 'Theme files',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IMPORT_THEMES_DESCRIPTOR = msg({
	message: 'Import themes',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXPORT_THEMES_DESCRIPTOR = msg({
	message: 'Export themes',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_ASSETS_DESCRIPTOR = msg({
	message: 'Theme assets',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOCAL_FILES_DESCRIPTOR = msg({
	message: 'Local files',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_TOKENS_DESCRIPTOR = msg({
	message: 'Theme tokens',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const OVERRIDES_DESCRIPTOR = msg({
	message: 'Overrides',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRANSPARENCY_DESCRIPTOR = msg({
	message: 'Transparency',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHARE_THEME_KEYWORD_DESCRIPTOR = msg({
	message: 'Share theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BACKUP_DESCRIPTOR = msg({
	message: 'Backup',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RESET_THEME_DESCRIPTOR = msg({
	message: 'Reset theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

export const themesSections = [
	{
		id: 'this-account',
		tabType: 'themes',
		label: THIS_ACCOUNT_SECTION_DESCRIPTOR,
		keywords: [THEME_DESCRIPTOR, DESKTOP_APP_THEME_KEYWORD_DESCRIPTOR, ACCOUNTS_DESCRIPTOR, SYNC_DESCRIPTOR],
		isAdvanced: false,
		isVisible: isDesktop,
	},
	{
		id: 'base-theme',
		tabType: 'themes',
		label: BASE_THEME_SECTION_DESCRIPTOR,
		keywords: [
			THEME_DESCRIPTOR,
			DARK_MODE_DESCRIPTOR,
			LIGHT_MODE_DESCRIPTOR,
			DARK_DESCRIPTOR,
			LIGHT_DESCRIPTOR,
			COAL_DESCRIPTOR,
			COLOR_SCHEME_DESCRIPTOR,
			SYSTEM_THEME_DESCRIPTOR,
			SYNC_THEME_DESCRIPTOR,
			THEME_SYNC_DESCRIPTOR,
			DEVICES_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'your-themes',
		tabType: 'themes',
		label: YOUR_THEMES_SECTION_DESCRIPTOR,
		keywords: [THEME_LIBRARY_DESCRIPTOR, THEME_FILES_DESCRIPTOR, IMPORT_THEMES_DESCRIPTOR, CSS_DESCRIPTOR],
		isAdvanced: false,
	},
	{
		id: 'colors-and-fonts',
		tabType: 'themes',
		label: COLORS_AND_FONTS_SECTION_DESCRIPTOR,
		keywords: [COLORS_DESCRIPTOR, FONTS_DESCRIPTOR, THEME_TOKENS_DESCRIPTOR, OVERRIDES_DESCRIPTOR, STYLING_DESCRIPTOR],
		isAdvanced: false,
	},
	{
		id: 'assets',
		tabType: 'themes',
		label: ASSETS_SECTION_DESCRIPTOR,
		keywords: [THEME_ASSETS_DESCRIPTOR, LOCAL_FILES_DESCRIPTOR, FONTS_DESCRIPTOR],
		isAdvanced: false,
	},
	{
		id: 'transparent-window',
		tabType: 'themes',
		label: DESKTOP_WINDOW_SECTION_DESCRIPTOR,
		keywords: [TRANSPARENCY_DESCRIPTOR],
		isAdvanced: false,
		isVisible: isDesktop,
	},
	{
		id: 'share-and-back-up',
		tabType: 'themes',
		label: SHARE_AND_BACK_UP_SECTION_DESCRIPTOR,
		keywords: [SHARE_THEME_KEYWORD_DESCRIPTOR, EXPORT_THEMES_DESCRIPTOR, BACKUP_DESCRIPTOR, RESET_THEME_DESCRIPTOR],
		isAdvanced: false,
	},
	{
		id: 'quick-css',
		tabType: 'themes',
		label: QUICK_CSS_SECTION_DESCRIPTOR,
		keywords: [CSS_DESCRIPTOR, CUSTOM_DESCRIPTOR, STYLING_DESCRIPTOR, SYNC_DESCRIPTOR],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
