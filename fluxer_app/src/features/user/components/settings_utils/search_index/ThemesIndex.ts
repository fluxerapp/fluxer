// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	EXPORT_LIBRARY_DESCRIPTOR,
	IMPORT_LIBRARY_DESCRIPTOR,
	OPEN_QUICK_CSS_EDITOR_DESCRIPTOR,
	SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTOR,
	USE_DESKTOP_APP_THEME_DESCRIPTOR,
} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {msg} from '@lingui/core/macro';

const SAME_THEME_FOR_EVERY_ACCOUNT_DESCRIPTOR = msg({
	message: 'Use the same theme for every account in the desktop app',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});
const CSS_EDITOR_DESCRIPTOR = msg({
	message: 'CSS editor',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_CSS_DESCRIPTOR = msg({
	message: 'Custom CSS',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTOR = msg({
	message: 'Allow transparent desktop window',
	comment: 'Settings search entry label. Names the switch in Settings > Themes on the desktop app.',
});

export const themesIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'themes-use-desktop-app-theme',
		tabType: 'themes',
		sectionId: 'this-account',
		label: USE_DESKTOP_APP_THEME_DESCRIPTOR,
		description: SAME_THEME_FOR_EVERY_ACCOUNT_DESCRIPTOR,
		keywords: [USE_DESKTOP_APP_THEME_DESCRIPTOR],
		tags: ['appearance'],
		isVisible: isDesktop,
	},
	{
		id: 'themes-transparent-window',
		tabType: 'themes',
		sectionId: 'transparent-window',
		label: ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTOR,
		keywords: [],
		tags: ['appearance', 'desktop'],
		isVisible: isDesktop,
	},
	{
		id: 'themes-export-library',
		tabType: 'themes',
		sectionId: 'share-and-back-up',
		label: EXPORT_LIBRARY_DESCRIPTOR,
		keywords: [],
		tags: ['appearance'],
	},
	{
		id: 'themes-import-library',
		tabType: 'themes',
		sectionId: 'share-and-back-up',
		label: IMPORT_LIBRARY_DESCRIPTOR,
		keywords: [],
		tags: ['appearance'],
	},
	{
		id: 'themes-open-quick-css-editor',
		tabType: 'themes',
		sectionId: 'quick-css',
		label: OPEN_QUICK_CSS_EDITOR_DESCRIPTOR,
		keywords: [CSS_EDITOR_DESCRIPTOR, CUSTOM_CSS_DESCRIPTOR],
		tags: ['appearance'],
		isVisible: () => !MobileLayout.enabled,
	},
	{
		id: 'themes-sync-custom-css',
		tabType: 'themes',
		sectionId: 'quick-css',
		label: SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTOR,
		keywords: [CUSTOM_CSS_DESCRIPTOR],
		tags: ['appearance'],
	},
];
