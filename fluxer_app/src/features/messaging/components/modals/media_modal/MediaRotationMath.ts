// SPDX-License-Identifier: AGPL-3.0-or-later

const FULL_ROTATION_DEGREES = 360;
const ROTATION_STEP_DEGREES = 90;

function normalizeRotationDegrees(degrees: number): number {
	const normalized = degrees % FULL_ROTATION_DEGREES;
	if (Object.is(normalized, -0)) return 0;
	return normalized < 0 ? normalized + FULL_ROTATION_DEGREES : normalized;
}

export function rotateClockwiseDegrees(degrees: number): number {
	return degrees + ROTATION_STEP_DEGREES;
}

export function rotateAnticlockwiseDegrees(degrees: number): number {
	return degrees - ROTATION_STEP_DEGREES;
}

export function isDefaultRotationDegrees(degrees: number): boolean {
	return normalizeRotationDegrees(degrees) === 0;
}

export function isSidewaysRotationDegrees(degrees: number): boolean {
	const normalized = normalizeRotationDegrees(degrees);
	return normalized === ROTATION_STEP_DEGREES || normalized === FULL_ROTATION_DEGREES - ROTATION_STEP_DEGREES;
}

export function getNearestDefaultRotationDegrees(degrees: number): number {
	const normalized = normalizeRotationDegrees(degrees);
	if (normalized === 0) return degrees;
	if (normalized <= FULL_ROTATION_DEGREES / 2) return degrees - normalized;
	return degrees + (FULL_ROTATION_DEGREES - normalized);
}
