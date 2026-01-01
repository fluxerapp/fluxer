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

import type {UserRecord} from '~/records/UserRecord';
import {getDefaultAvatarPrimaryColor} from '~/utils/AvatarUtils';
import {int2hex} from '~/utils/ColorUtils';

const toHex = (value: string | number | null | undefined): string | null => {
	if (value == null) return null;
	if (typeof value === 'number') return int2hex(value);
	return value;
};

export const getPlaceholderAvatarColor = (user: UserRecord | null | undefined, fallback: string): string => {
	if (!user) return fallback;
	if (typeof user.avatarColor === 'number') return int2hex(user.avatarColor);
	if (!user.avatar) {
		return int2hex(getDefaultAvatarPrimaryColor(user.id));
	}
	const accentColor = toHex(user.accentColor);
	if (accentColor) return accentColor;
	return fallback;
};
