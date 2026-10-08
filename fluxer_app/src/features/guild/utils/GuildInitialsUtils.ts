// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';

const GUILD_ICON_INITIALS_MAX_LENGTH = 4;
const MEASURE_FONT_SIZES_PX = [10, 12, 14, 16, 20, 24];
const MEASURE_FONT_WEIGHT = 600;

export const GuildInitialsLength = Object.freeze({
	SHORT: 'short',
	MEDIUM: 'medium',
} as const);

export type GuildInitialsLength = (typeof GuildInitialsLength)[keyof typeof GuildInitialsLength];

export type GuildInitialsFitStyle = React.CSSProperties & {
	'--initials-width-em'?: string;
};

const measuredWidthsEm = new Map<string, number>();
let measureContext: OffscreenCanvasRenderingContext2D | null | undefined;

export function getInitialsFromName(name: string): string {
	const words = name.split(/\s+/u).filter(Boolean);
	return words
		.map((word) => Array.from(word)[0])
		.filter(Boolean)
		.join('');
}

export function truncateInitials(initials: string, maxLength: number): string {
	if (maxLength <= 0) return '';
	return Array.from(initials).slice(0, maxLength).join('');
}

export function getGuildIconDisplayInitials(initials: string): string {
	return truncateInitials(initials, GUILD_ICON_INITIALS_MAX_LENGTH);
}

export function getInitialsLength(displayInitials: string): GuildInitialsLength {
	return Array.from(displayInitials).length <= 2 ? GuildInitialsLength.SHORT : GuildInitialsLength.MEDIUM;
}

function getMeasureContext(): OffscreenCanvasRenderingContext2D | null {
	if (measureContext !== undefined) return measureContext;
	if (typeof document === 'undefined' || typeof OffscreenCanvas === 'undefined') {
		measureContext = null;
		return measureContext;
	}
	measureContext = new OffscreenCanvas(1, 1).getContext('2d');
	document.fonts?.addEventListener('loadingdone', () => measuredWidthsEm.clear());
	return measureContext;
}

function measureInitialsWidthEm(initials: string): number | null {
	const context = getMeasureContext();
	if (context == null || document.body == null) return null;
	const fontFamily = getComputedStyle(document.body).fontFamily;
	const key = `${fontFamily}\u0000${initials}`;
	const cached = measuredWidthsEm.get(key);
	if (cached !== undefined) return cached;
	let widthEm = 0;
	for (const fontSize of MEASURE_FONT_SIZES_PX) {
		context.font = `${MEASURE_FONT_WEIGHT} ${fontSize}px ${fontFamily}`;
		widthEm = Math.max(widthEm, context.measureText(initials).width / fontSize);
	}
	measuredWidthsEm.set(key, widthEm);
	return widthEm;
}

export function getGuildInitialsFitStyle(displayInitials: string): GuildInitialsFitStyle | undefined {
	if (!displayInitials) return undefined;
	const widthEm = measureInitialsWidthEm(displayInitials);
	if (widthEm == null || widthEm <= 0) return undefined;
	return {'--initials-width-em': widthEm.toFixed(3)};
}
