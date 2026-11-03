// SPDX-License-Identifier: AGPL-3.0-or-later

const POSSESSIVE_SUFFIX = /['’]s(?=\s)/gu;
const WORD_RUN = /([\p{L}\p{N}_]\p{M}*)[\p{L}\p{N}\p{M}_‌‍]*/gu;
const WHITESPACE = /\s+/gu;
const GUILD_ACRONYM_REFERENCE_ICON_SIZE_PX = 48;
const GUILD_ACRONYM_FONT_SIZES_PX = [18, 18, 16, 14, 12, 10];
const GUILD_ACRONYM_FIT_PADDING_PX = 4;
const GUILD_ACRONYM_FIT_WIDTH_RATIO =
	(GUILD_ACRONYM_REFERENCE_ICON_SIZE_PX - GUILD_ACRONYM_FIT_PADDING_PX * 2) / GUILD_ACRONYM_REFERENCE_ICON_SIZE_PX;
const GUILD_ACRONYM_MEASURE_FONT_SIZE_PX = 100;
const GUILD_ACRONYM_WIDTH_CACHE_CAPACITY = 512;

const acronymWidthCache = new Map<string, number>();
let measureContext: CanvasRenderingContext2D | null | undefined;

export const GUILD_ACRONYM_FALLBACK = '?';

const graphemeSegmenter =
	typeof Intl !== 'undefined' && typeof Intl.Segmenter === 'function'
		? new Intl.Segmenter(undefined, {granularity: 'grapheme'})
		: null;

function firstGrapheme(wordRun: string, leadingCharacterWithMarks: string): string {
	if (graphemeSegmenter == null) return leadingCharacterWithMarks;
	for (const {segment} of graphemeSegmenter.segment(wordRun)) {
		return segment;
	}
	return leadingCharacterWithMarks;
}

export function getGuildAcronym(name: string): string {
	return name.replace(POSSESSIVE_SUFFIX, '').replace(WORD_RUN, firstGrapheme).replace(WHITESPACE, '');
}

function getGuildAcronymLength(acronym: string): number {
	if (graphemeSegmenter == null) return Array.from(acronym).length;
	let length = 0;
	for (const _segment of graphemeSegmenter.segment(acronym)) {
		length += 1;
	}
	return length;
}

function getGuildAcronymFontScale(acronymLength: number): number {
	const index = Math.min(Math.max(acronymLength, 1), GUILD_ACRONYM_FONT_SIZES_PX.length) - 1;
	return GUILD_ACRONYM_FONT_SIZES_PX[index] / GUILD_ACRONYM_REFERENCE_ICON_SIZE_PX;
}

function getMeasureContext(): CanvasRenderingContext2D | null {
	if (measureContext !== undefined) return measureContext;
	if (typeof document === 'undefined' || document.body == null) return null;
	const context = document.createElement('canvas').getContext('2d');
	if (context != null) {
		context.font = `500 ${GUILD_ACRONYM_MEASURE_FONT_SIZE_PX}px ${getComputedStyle(document.body).fontFamily}`;
	}
	measureContext = context;
	return context;
}

function getGuildAcronymWidthPerEm(acronym: string): number | null {
	const cached = acronymWidthCache.get(acronym);
	if (cached !== undefined) return cached;
	const context = getMeasureContext();
	if (context == null) return null;
	const widthPerEm = context.measureText(acronym).width / GUILD_ACRONYM_MEASURE_FONT_SIZE_PX;
	if (acronymWidthCache.size >= GUILD_ACRONYM_WIDTH_CACHE_CAPACITY) acronymWidthCache.clear();
	acronymWidthCache.set(acronym, widthPerEm);
	return widthPerEm;
}

function fitGuildAcronymFontScale(acronymLength: number, widthPerEm: number | null): number {
	const scale = getGuildAcronymFontScale(acronymLength);
	if (widthPerEm == null || widthPerEm <= 0) return scale;
	const maxScale = GUILD_ACRONYM_FIT_WIDTH_RATIO / widthPerEm;
	if (scale <= maxScale) return scale;
	for (const fontSize of GUILD_ACRONYM_FONT_SIZES_PX) {
		const candidate = fontSize / GUILD_ACRONYM_REFERENCE_ICON_SIZE_PX;
		if (candidate < scale && candidate <= maxScale) return candidate;
	}
	return maxScale;
}

export function getFittedGuildAcronymFontScale(acronym: string): number {
	return fitGuildAcronymFontScale(getGuildAcronymLength(acronym), getGuildAcronymWidthPerEm(acronym));
}
