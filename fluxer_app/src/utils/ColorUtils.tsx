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

const pad2 = (s: string) => (s.length === 1 ? `0${s}` : s);

export const int2hex = (colorInt: number) => {
	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;
	return `#${pad2(r.toString(16))}${pad2(g.toString(16))}${pad2(b.toString(16))}`;
};

export const int2rgba = (colorInt: number, alpha?: number) => {
	if (alpha == null) {
		alpha = ((colorInt >> 24) & 0xff) / 255;
	}

	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;

	return `rgba(${r}, ${g}, ${b}, ${alpha})`;
};

export const int2rgb = (colorInt: number) => {
	if (colorInt === 0) {
		return 'rgb(219, 222, 225)';
	}
	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;
	return `rgb(${r}, ${g}, ${b})`;
};

export const getBestContrastColor = (colorInt: number): 'black' | 'white' => {
	if (colorInt === 0) {
		return 'black';
	}

	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;

	const rsRGB = r / 255;
	const gsRGB = g / 255;
	const bsRGB = b / 255;

	const rLinear = rsRGB <= 0.03928 ? rsRGB / 12.92 : ((rsRGB + 0.055) / 1.055) ** 2.4;
	const gLinear = gsRGB <= 0.03928 ? gsRGB / 12.92 : ((gsRGB + 0.055) / 1.055) ** 2.4;
	const bLinear = bsRGB <= 0.03928 ? bsRGB / 12.92 : ((bsRGB + 0.055) / 1.055) ** 2.4;

	const luminance = 0.2126 * rLinear + 0.7152 * gLinear + 0.0722 * bLinear;

	return luminance > 0.5 ? 'black' : 'white';
};
