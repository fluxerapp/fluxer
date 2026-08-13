// SPDX-License-Identifier: AGPL-3.0-or-later

export const REM_BASE_PX = 16;

class NonFinitePixelValueError extends Error {
	public constructor(value: number) {
		super(`Pixel value must be finite: ${value}`);
		this.name = 'NonFinitePixelValueError';
	}
}

export function remFromPx(px: number): `${number}rem` {
	if (!Number.isFinite(px)) {
		throw new NonFinitePixelValueError(px);
	}
	const rounded = Math.round((px / REM_BASE_PX) * 1e5) / 1e5;
	return `${rounded}rem`;
}

export function getRemScaleForDocument(ownerDocument: Document | null): number {
	if (ownerDocument == null) {
		return 1;
	}
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) {
		return 1;
	}
	const rootFontSize = Number.parseFloat(ownerWindow.getComputedStyle(ownerDocument.documentElement).fontSize);
	if (!Number.isFinite(rootFontSize) || rootFontSize <= 0) {
		return 1;
	}
	return rootFontSize / REM_BASE_PX;
}
