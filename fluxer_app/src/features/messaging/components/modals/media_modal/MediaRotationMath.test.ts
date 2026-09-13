// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getNearestDefaultRotationDegrees,
	isDefaultRotationDegrees,
	isSidewaysRotationDegrees,
	normalizeRotationDegrees,
	rotateAnticlockwiseDegrees,
	rotateClockwiseDegrees,
} from '@app/features/messaging/components/modals/media_modal/MediaRotationMath';
import {describe, expect, it} from 'vitest';

describe('MediaRotationMath', () => {
	it('keeps clockwise animation continuous through a full turn', () => {
		let rotation = 0;
		rotation = rotateClockwiseDegrees(rotation);
		expect(rotation).toBe(90);
		rotation = rotateClockwiseDegrees(rotation);
		expect(rotation).toBe(180);
		rotation = rotateClockwiseDegrees(rotation);
		expect(rotation).toBe(270);
		rotation = rotateClockwiseDegrees(rotation);
		expect(rotation).toBe(360);
	});

	it('keeps anticlockwise animation continuous from the default orientation', () => {
		expect(rotateAnticlockwiseDegrees(0)).toBe(-90);
		expect(normalizeRotationDegrees(-90)).toBe(270);
	});

	it('normalizes orientation for layout and default-state decisions', () => {
		expect(isSidewaysRotationDegrees(-90)).toBe(true);
		expect(isSidewaysRotationDegrees(450)).toBe(true);
		expect(isDefaultRotationDegrees(360)).toBe(true);
		expect(isDefaultRotationDegrees(-360)).toBe(true);
	});

	it('resets to the nearest default-equivalent angle', () => {
		expect(getNearestDefaultRotationDegrees(270)).toBe(360);
		expect(getNearestDefaultRotationDegrees(-270)).toBe(-360);
		expect(getNearestDefaultRotationDegrees(450)).toBe(360);
		expect(getNearestDefaultRotationDegrees(360)).toBe(360);
	});
});
