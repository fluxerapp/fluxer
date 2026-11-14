// SPDX-License-Identifier: AGPL-3.0-or-later

import {ReverseImageSearchMenuItems} from '@app/features/ui/action_menu/items/ReverseImageSearchMenuItems';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const COPY_AVATAR_URL_DESCRIPTOR = msg({
	message: 'Copy avatar URL',
	comment: 'Action that copies the selected user avatar URL.',
});
const OPEN_AVATAR_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open avatar in browser',
	comment: 'Action that opens the selected user avatar in an external browser.',
});
const REVERSE_SEARCH_AVATAR_DESCRIPTOR = msg({
	message: 'Reverse search avatar',
	comment: 'Submenu label that lists reverse-image-search providers for the selected user avatar.',
});
const COPY_BANNER_URL_DESCRIPTOR = msg({
	message: 'Copy banner URL',
	comment: 'Action that copies the selected user banner URL.',
});
const OPEN_BANNER_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open banner in browser',
	comment: 'Action that opens the selected user banner in an external browser.',
});
const REVERSE_SEARCH_BANNER_DESCRIPTOR = msg({
	message: 'Reverse search banner',
	comment: 'Submenu label that lists reverse-image-search providers for the selected user banner.',
});

interface UserImageMenuItemsProps {
	user: User;
	profile?: Profile | null;
	profileContext?: ProfileDisplayUtils.ProfileDisplayContext;
	previewOverrides?: ProfileDisplayUtils.ProfilePreviewOverrides;
	onClose: () => void;
	variant?: 'all' | 'avatar' | 'banner';
}

interface UserImageMenuUrlOptions {
	user: User;
	profile?: Profile | null;
	profileContext?: ProfileDisplayUtils.ProfileDisplayContext;
	previewOverrides?: ProfileDisplayUtils.ProfilePreviewOverrides;
}

function getUserImageMenuProfileContext({
	user,
	profile,
	profileContext,
}: UserImageMenuUrlOptions): ProfileDisplayUtils.ProfileDisplayContext {
	return profileContext ?? {user, profile};
}

export function getUserMenuAvatarUrl(options: UserImageMenuUrlOptions): string | null {
	return ProfileDisplayUtils.getProfileAvatarMenuUrl(getUserImageMenuProfileContext(options), options.previewOverrides);
}

export function getUserMenuBannerUrl(options: UserImageMenuUrlOptions): string | null {
	return ProfileDisplayUtils.getProfileBannerUrl(getUserImageMenuProfileContext(options), options.previewOverrides);
}

export const UserImageMenuItems: React.FC<UserImageMenuItemsProps> = observer(
	({user, profile, profileContext, previewOverrides, onClose, variant = 'all'}) => {
		const {i18n} = useLingui();
		const menuUrlOptions = {user, profile, profileContext, previewOverrides};
		const avatarUrl = getUserMenuAvatarUrl(menuUrlOptions);
		const bannerUrl = getUserMenuBannerUrl(menuUrlOptions);
		const showAvatar = variant !== 'banner';
		const showBanner = variant !== 'avatar';
		return (
			<>
				{showAvatar && avatarUrl && (
					<ReverseImageSearchMenuItems
						imageUrl={avatarUrl}
						onClose={onClose}
						includeCopyAndOpen
						copyLabel={i18n._(COPY_AVATAR_URL_DESCRIPTOR)}
						openLabel={i18n._(OPEN_AVATAR_IN_BROWSER_DESCRIPTOR)}
						defaultLabel={i18n._(REVERSE_SEARCH_AVATAR_DESCRIPTOR)}
						data-flx="ui.action-menu.items.user-image-menu-items.reverse-image-search-menu-items"
					/>
				)}
				{showBanner && bannerUrl && (
					<ReverseImageSearchMenuItems
						imageUrl={bannerUrl}
						onClose={onClose}
						includeCopyAndOpen
						copyLabel={i18n._(COPY_BANNER_URL_DESCRIPTOR)}
						openLabel={i18n._(OPEN_BANNER_IN_BROWSER_DESCRIPTOR)}
						defaultLabel={i18n._(REVERSE_SEARCH_BANNER_DESCRIPTOR)}
						data-flx="ui.action-menu.items.user-image-menu-items.reverse-image-search-menu-items--2"
					/>
				)}
			</>
		);
	},
);
