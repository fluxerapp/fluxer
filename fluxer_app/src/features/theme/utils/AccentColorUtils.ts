// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import type {User} from '@app/features/user/models/User';
import {getDefaultAvatarPrimaryColor} from '@app/features/user/utils/AvatarUtils';
import {DEFAULT_ACCENT_COLOR} from '@fluxer/constants/src/AppConstants';

type RawAccentColor = number | null | undefined;

function getAccentColorHex(rawAccentColor?: RawAccentColor): string | null {
	if (rawAccentColor == null) {
		return null;
	}
	return ColorUtils.int2hex(rawAccentColor);
}

export function getUserAccentColor(
	user: User | null | undefined,
	profileAccentColor?: RawAccentColor,
	fallback = DEFAULT_ACCENT_COLOR,
): string {
	const profileColor = getAccentColorHex(profileAccentColor);
	if (profileColor) {
		return profileColor;
	}
	if (user && typeof user.avatarColor === 'number') {
		return ColorUtils.int2hex(user.avatarColor);
	}
	if (user && !user.avatar) {
		return ColorUtils.int2hex(getDefaultAvatarPrimaryColor(user.id));
	}
	return fallback;
}
