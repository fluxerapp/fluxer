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

import {getBestContrastColor} from '~/utils/ColorUtils';

const DEFAULT_NOTCH_COLOR = 'rgba(255, 255, 255, 0.6)';
const DARK_NOTCH_COLOR = 'rgba(0, 0, 0, 0.4)';
const LIGHT_NOTCH_COLOR = 'rgba(255, 255, 255, 0.7)';

export const getContrastingNotchColor = (bannerColor?: number | null, hasBanner?: boolean): string => {
	if (!hasBanner || bannerColor == null) {
		return DEFAULT_NOTCH_COLOR;
	}

	return getBestContrastColor(bannerColor) === 'black' ? DARK_NOTCH_COLOR : LIGHT_NOTCH_COLOR;
};
