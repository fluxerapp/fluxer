// SPDX-License-Identifier: AGPL-3.0-or-later

import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {shouldShowClaimedAccountSettings} from '@app/features/user/components/settings_utils/search_index/SearchIndexHelpers';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	BACKGROUND_DESCRIPTOR,
	PREMIUM_PRODUCT_NAME_KEYWORD,
} from '@app/features/user/components/settings_utils/search_index/SharedDescriptors';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';

const AVATAR_DESCRIPTOR = msg({
	message: 'Avatar',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const PROFILE_PICTURE_DESCRIPTOR = msg({
	message: 'Profile picture',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PHOTO_DESCRIPTOR = msg({
	message: 'Photo',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IMAGE_DESCRIPTOR = msg({
	message: 'Image',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PICTURE_DESCRIPTOR = msg({
	message: 'Picture',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_YOUR_PROFILE_PICTURE_DESCRIPTOR = msg({
	message: 'Change your profile picture',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const BANNER_DESCRIPTOR = msg({
	message: 'Banner',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const COVER_DESCRIPTOR = msg({
	message: 'Cover',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HEADER_DESCRIPTOR = msg({
	message: 'Header',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOMIZE_YOUR_PROFILE_BANNER_DESCRIPTOR = msg({
	message: 'Customize your profile banner',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const USERNAME_DESCRIPTOR = msg({
	message: 'Username',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISPLAY_NAME_DESCRIPTOR = msg({
	message: 'Display name',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const USER_DESCRIPTOR = msg({
	message: 'User',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HANDLE_DESCRIPTOR = msg({
	message: 'Handle',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TAG_DESCRIPTOR = msg({
	message: 'Tag',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_YOUR_USERNAME_DESCRIPTOR = msg({
	message: 'Change your username',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const GLOBAL_NAME_DESCRIPTOR = msg({
	message: 'Global name',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PROFILE_NAME_DESCRIPTOR = msg({
	message: 'Profile name',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_THE_NAME_SHOWN_ON_YOUR_GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'Change the name shown on your global profile',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const PROFILE_TYPE_DESCRIPTOR = msg({
	message: 'Profile type',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'Global profile',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Community profile',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PER_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Per-community profile',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SWITCH_BETWEEN_YOUR_GLOBAL_PROFILE_AND_PER_COMMUNITY_DESCRIPTOR = msg({
	message: 'Switch between your global profile and per-community profiles',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const COMMUNITY_NICKNAME_DESCRIPTOR = msg({
	message: 'Community nickname',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const NICKNAME_DESCRIPTOR = msg({
	message: 'Nickname',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PROFILE_NICKNAME_DESCRIPTOR = msg({
	message: 'Profile nickname',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANGE_YOUR_NICKNAME_FOR_A_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Change your nickname for a community profile',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const BIO_DESCRIPTOR = msg({
	message: 'Bio',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const ABOUT_ME_DESCRIPTOR = msg({
	message: 'About me',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DESCRIPTION_DESCRIPTOR = msg({
	message: 'Description',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BIOGRAPHY_DESCRIPTOR = msg({
	message: 'Biography',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ABOUT_DESCRIPTOR = msg({
	message: 'About',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EDIT_YOUR_PROFILE_BIO_DESCRIPTOR = msg({
	message: 'Edit your profile bio',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const ACCENT_COLOR_DESCRIPTOR = msg({
	message: 'Accent color',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const ACCENT_DESCRIPTOR = msg({
	message: 'Accent',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COLOR_DESCRIPTOR = msg({
	message: 'Color',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_COLOR_DESCRIPTOR = msg({
	message: 'Theme color',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PROFILE_COLOR_DESCRIPTOR = msg({
	message: 'Profile color',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_YOUR_PROFILE_ACCENT_COLOR_DESCRIPTOR = msg({
	message: 'Choose your profile accent color',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const PRONOUNS_DESCRIPTOR = msg({
	message: 'Pronouns',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const IDENTITY_DESCRIPTOR = msg({
	message: 'Identity',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HE_DESCRIPTOR = msg({
	message: 'He',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHE_DESCRIPTOR = msg({
	message: 'She',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEY_DESCRIPTOR = msg({
	message: 'They',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SET_YOUR_PRONOUNS_DESCRIPTOR = msg({
	message: 'Set your pronouns',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const BADGE_DESCRIPTOR = msg({
	message: 'Badge',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const PREMIUM_BADGE_DESCRIPTOR = msg({
	message: 'Premium badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIGURE_YOUR_PROFILE_BADGE_DESCRIPTOR = msg({
	message: 'Configure your profile badge',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

function shouldShowPremiumBadgeSettings(): boolean {
	return (
		shouldShowClaimedAccountSettings() && shouldShowPremiumFeatures() && (Users.getCurrentUser()?.isPremium() ?? false)
	);
}

export const myProfileIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'profile-avatar',
		tabType: 'my_profile',
		label: AVATAR_DESCRIPTOR,
		keywords: [AVATAR_DESCRIPTOR, PROFILE_PICTURE_DESCRIPTOR, PHOTO_DESCRIPTOR, IMAGE_DESCRIPTOR, PICTURE_DESCRIPTOR],
		description: CHANGE_YOUR_PROFILE_PICTURE_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-banner',
		tabType: 'my_profile',
		label: BANNER_DESCRIPTOR,
		keywords: [BANNER_DESCRIPTOR, COVER_DESCRIPTOR, HEADER_DESCRIPTOR, BACKGROUND_DESCRIPTOR],
		description: CUSTOMIZE_YOUR_PROFILE_BANNER_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-username',
		tabType: 'my_profile',
		label: USERNAME_DESCRIPTOR,
		keywords: [
			USERNAME_DESCRIPTOR,
			NAME_DESCRIPTOR,
			DISPLAY_NAME_DESCRIPTOR,
			USER_DESCRIPTOR,
			HANDLE_DESCRIPTOR,
			TAG_DESCRIPTOR,
		],
		description: CHANGE_YOUR_USERNAME_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-display-name',
		tabType: 'my_profile',
		label: DISPLAY_NAME_DESCRIPTOR,
		keywords: [DISPLAY_NAME_DESCRIPTOR, GLOBAL_NAME_DESCRIPTOR, PROFILE_NAME_DESCRIPTOR, NAME_DESCRIPTOR],
		description: CHANGE_THE_NAME_SHOWN_ON_YOUR_GLOBAL_PROFILE_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-type',
		tabType: 'my_profile',
		label: PROFILE_TYPE_DESCRIPTOR,
		keywords: [
			PROFILE_TYPE_DESCRIPTOR,
			GLOBAL_PROFILE_DESCRIPTOR,
			COMMUNITY_PROFILE_DESCRIPTOR,
			PER_COMMUNITY_PROFILE_DESCRIPTOR,
		],
		description: SWITCH_BETWEEN_YOUR_GLOBAL_PROFILE_AND_PER_COMMUNITY_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-community-nickname',
		tabType: 'my_profile',
		label: COMMUNITY_NICKNAME_DESCRIPTOR,
		keywords: [
			NICKNAME_DESCRIPTOR,
			COMMUNITY_NICKNAME_DESCRIPTOR,
			PROFILE_NICKNAME_DESCRIPTOR,
			PER_COMMUNITY_PROFILE_DESCRIPTOR,
		],
		description: CHANGE_YOUR_NICKNAME_FOR_A_COMMUNITY_PROFILE_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-bio',
		tabType: 'my_profile',
		label: BIO_DESCRIPTOR,
		keywords: [ABOUT_ME_DESCRIPTOR, DESCRIPTION_DESCRIPTOR, BIOGRAPHY_DESCRIPTOR, ABOUT_DESCRIPTOR],
		description: EDIT_YOUR_PROFILE_BIO_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-accent-color',
		tabType: 'my_profile',
		label: ACCENT_COLOR_DESCRIPTOR,
		keywords: [ACCENT_DESCRIPTOR, COLOR_DESCRIPTOR, THEME_COLOR_DESCRIPTOR, PROFILE_COLOR_DESCRIPTOR],
		description: CHOOSE_YOUR_PROFILE_ACCENT_COLOR_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-pronouns',
		tabType: 'my_profile',
		label: PRONOUNS_DESCRIPTOR,
		keywords: [PRONOUNS_DESCRIPTOR, IDENTITY_DESCRIPTOR, HE_DESCRIPTOR, SHE_DESCRIPTOR, THEY_DESCRIPTOR],
		description: SET_YOUR_PRONOUNS_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
	{
		id: 'profile-badge',
		tabType: 'my_profile',
		label: BADGE_DESCRIPTOR,
		keywords: [BADGE_DESCRIPTOR, PREMIUM_BADGE_DESCRIPTOR, PREMIUM_PRODUCT_NAME_KEYWORD],
		description: CONFIGURE_YOUR_PROFILE_BADGE_DESCRIPTOR,
		isVisible: shouldShowPremiumBadgeSettings,
	},
];
