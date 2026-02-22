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

import type {UserRecord} from '@app/records/UserRecord';
import {getDefaultAvatarPrimaryColor} from '@app/utils/AvatarUtils';
import * as ColorUtils from '@app/utils/ColorUtils';
import {DEFAULT_ACCENT_COLOR} from '@fluxer/constants/src/AppConstants';

type RawAccentColor = number | null | undefined;

export function getAccentColorHex(rawAccentColor?: RawAccentColor): string | null {
	if (rawAccentColor == null) {
		return null;
	}

	return ColorUtils.int2hex(rawAccentColor);
}

export function getAccentColor(rawAccentColor?: RawAccentColor, fallback = DEFAULT_ACCENT_COLOR): string {
	return getAccentColorHex(rawAccentColor) ?? fallback;
}

export function getUserAccentColor(
	user: UserRecord | null | undefined,
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
