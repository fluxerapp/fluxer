// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Color} from 'react-aria-components';

interface RectLike {
	left: number;
	top: number;
	width: number;
	height: number;
}

export interface UnitPoint {
	x: number;
	y: number;
}

function clampUnit(value: number): number {
	if (!Number.isFinite(value)) return 0;
	return Math.max(0, Math.min(1, value));
}

export function getUnitPointFromClientPosition(clientX: number, clientY: number, rect: RectLike): UnitPoint | null {
	if (rect.width <= 0 || rect.height <= 0) return null;
	return {
		x: clampUnit((clientX - rect.left) / rect.width),
		y: clampUnit((clientY - rect.top) / rect.height),
	};
}

export function getUnitValueFromClientX(clientX: number, rect: Pick<RectLike, 'left' | 'width'>): number | null {
	if (rect.width <= 0) return null;
	return clampUnit((clientX - rect.left) / rect.width);
}

export function getColorWithSaturationBrightness(color: Color, point: UnitPoint): Color {
	return color.withChannelValue('saturation', point.x * 100).withChannelValue('brightness', (1 - point.y) * 100);
}

export function getColorWithHue(color: Color, unitValue: number): Color {
	return color.withChannelValue('hue', clampUnit(unitValue) * 360);
}

function getColorHex(color: Color): string {
	return color.toString('hex').toUpperCase();
}

export function shouldSyncPickerColorFromProp(propColor: Color, pickerColor: Color): boolean {
	return getColorHex(propColor) !== getColorHex(pickerColor);
}
