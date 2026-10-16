// SPDX-License-Identifier: AGPL-3.0-or-later

import {isNativeWindows} from '@app/features/ui/utils/NativeUtils';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	FONTS_DESCRIPTOR,
	USE_FLUXER_SANS_ON_WINDOWS_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const UNREAD_BADGE_CUSTOMIZATION_DESCRIPTOR = msg({
	message: 'Unread badge customization',
	comment: 'Settings search entry label for opting into experimental per-community unread badge controls.',
});
const UNREAD_BADGES_DESCRIPTOR = msg({
	message: 'Unread badges',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMMUNITY_BADGES_DESCRIPTOR = msg({
	message: 'Community badges',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANNEL_BADGES_DESCRIPTOR = msg({
	message: 'Channel badges',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXPERIMENTAL_DESCRIPTOR = msg({
	message: 'Experimental',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const OPT_IN_TO_EXPERIMENTAL_UNREAD_BADGE_CONTROLS_DESCRIPTOR = msg({
	message: 'Opt into experimental per-community and per-channel unread badge controls',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});

const EMOJI_AND_STICKER_CLONING_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Emoji and sticker cloning shortcuts',
	comment: 'Settings search entry label for opting into the emoji and sticker cloning shortcuts.',
});
const CLONE_EMOJI_DESCRIPTOR = msg({
	message: 'Clone emoji',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CLONE_STICKER_DESCRIPTOR = msg({
	message: 'Clone sticker',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COPY_EMOJI_DESCRIPTOR = msg({
	message: 'Copy emoji',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COPY_STICKER_DESCRIPTOR = msg({
	message: 'Copy sticker',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHOW_A_ONE_CLICK_SHORTCUT_FOR_COPYING_EXPRESSIONS_DESCRIPTOR = msg({
	message: 'Show a one-click shortcut for copying custom emojis and stickers from communities that allow it',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});

const SEGOE_UI_DESCRIPTOR = msg({
	message: 'Segoe UI',
	comment: 'Settings search synonym. Segoe UI is the name of the Windows system font and must not be translated.',
});
const SHOW_TEXT_IN_THE_PREVIOUS_FONT_DESCRIPTOR = msg({
	message: 'Show text in Fluxer Sans, the previous default font, instead of the Windows system font',
	comment:
		'Settings search entry description. One-line summary of what the setting controls. Fluxer Sans is a font name and must not be translated.',
});

export const advancedSettingsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'advanced-windows-fluxer-sans',
		tabType: 'advanced_settings',
		label: USE_FLUXER_SANS_ON_WINDOWS_DESCRIPTOR,
		keywords: [FONTS_DESCRIPTOR, SEGOE_UI_DESCRIPTOR],
		description: SHOW_TEXT_IN_THE_PREVIOUS_FONT_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance'],
		isVisible: () => isNativeWindows(),
	},
	{
		id: 'advanced-unread-badge-customization',
		tabType: 'advanced_settings',
		label: UNREAD_BADGE_CUSTOMIZATION_DESCRIPTOR,
		keywords: [
			UNREAD_BADGES_DESCRIPTOR,
			COMMUNITY_BADGES_DESCRIPTOR,
			CHANNEL_BADGES_DESCRIPTOR,
			EXPERIMENTAL_DESCRIPTOR,
		],
		description: OPT_IN_TO_EXPERIMENTAL_UNREAD_BADGE_CONTROLS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['notifications'],
		addedAt: '2026-06-04T00:00:00.000Z',
		badges: ['experimental'],
	},
	{
		id: 'advanced-expression-clone-shortcuts',
		tabType: 'advanced_settings',
		label: EMOJI_AND_STICKER_CLONING_SHORTCUTS_DESCRIPTOR,
		keywords: [
			CLONE_EMOJI_DESCRIPTOR,
			CLONE_STICKER_DESCRIPTOR,
			COPY_EMOJI_DESCRIPTOR,
			COPY_STICKER_DESCRIPTOR,
			EXPERIMENTAL_DESCRIPTOR,
		],
		description: SHOW_A_ONE_CLICK_SHORTCUT_FOR_COPYING_EXPRESSIONS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
		addedAt: '2026-09-11T00:00:00.000Z',
		badges: ['experimental'],
	},
];
