// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	COLOR_FAMILIES,
	COLOR_SCALES,
	type ColorTone,
	getThemePalette,
	scaleStopLightness,
} from '@app/features/theme/utils/ColorSystemPalette';

export const INSTANCE_BRAND_VARIABLES = [
	'--instance-brand-primary',
	'--instance-brand-secondary',
	'--instance-brand-primary-shade',
	'--instance-brand-primary-light',
	'--instance-brand-primary-foreground',
	'--instance-brand-primary-fill',
	'--instance-brand-primary-light-fill',
	'--instance-text-on-brand-primary',
	'--instance-text-on-brand-primary-light',
	'--instance-brand-tint',
	'--instance-brand-tint-hover',
	'--instance-brand-tint-border',
	'--instance-text-on-brand-tint',
] as const;

type InstanceBrandVariable = (typeof INSTANCE_BRAND_VARIABLES)[number];

export const MIN_TEXT_CONTRAST = 4.5;
const TARGET_CONTRAST = MIN_TEXT_CONTRAST + 0.05;
export const FOREGROUND_SELF_TINT_RATIO = 15;
export const FOREGROUND_BRAND_TINT_RATIO = 22;

type Rgb = readonly [number, number, number];

interface Color {
	css: string;
	rgb: Rgb;
}

interface Hsl {
	hue: number;
	saturation: number;
	lightness: number;
}

interface OnColor {
	text: Color;
	fill: Color;
	dark: boolean;
}

const HEX_COLOR_PATTERN = /^#[0-9a-f]{6}$/;
const WHITE: Rgb = [255, 255, 255];
const BLACK: Rgb = [0, 0, 0];

function round2(value: number): number {
	return Math.round(value * 100) / 100;
}

function parseHexColor(value: string): Rgb | null {
	if (!HEX_COLOR_PATTERN.test(value)) return null;
	const digits = value.slice(1);
	return [
		Number.parseInt(digits.slice(0, 2), 16),
		Number.parseInt(digits.slice(2, 4), 16),
		Number.parseInt(digits.slice(4, 6), 16),
	];
}

function rgbToHsl([red, green, blue]: Rgb): Hsl {
	const r = red / 255;
	const g = green / 255;
	const b = blue / 255;
	const max = Math.max(r, g, b);
	const min = Math.min(r, g, b);
	const lightness = (max + min) / 2;
	const delta = max - min;
	if (delta === 0) return {hue: 0, saturation: 0, lightness: lightness * 100};
	const saturation = delta / (1 - Math.abs(2 * lightness - 1));
	let hue: number;
	if (max === r) {
		hue = ((g - b) / delta) % 6;
	} else if (max === g) {
		hue = (b - r) / delta + 2;
	} else {
		hue = (r - g) / delta + 4;
	}
	return {hue: (hue * 60 + 360) % 360, saturation: saturation * 100, lightness: lightness * 100};
}

export function hslToRgb(hue: number, saturation: number, lightness: number): Rgb {
	const s = Math.min(100, Math.max(0, saturation)) / 100;
	const l = Math.min(100, Math.max(0, lightness)) / 100;
	const chroma = (1 - Math.abs(2 * l - 1)) * s;
	const h = (((hue % 360) + 360) % 360) / 60;
	const x = chroma * (1 - Math.abs((h % 2) - 1));
	const m = l - chroma / 2;
	const [r, g, b] =
		h < 1
			? [chroma, x, 0]
			: h < 2
				? [x, chroma, 0]
				: h < 3
					? [0, chroma, x]
					: h < 4
						? [0, x, chroma]
						: h < 5
							? [x, 0, chroma]
							: [chroma, 0, x];
	return [(r + m) * 255, (g + m) * 255, (b + m) * 255];
}

export function mixRgb(first: Rgb, second: Rgb, firstPercent: number): Rgb {
	const ratio = firstPercent / 100;
	return [
		first[0] * ratio + second[0] * (1 - ratio),
		first[1] * ratio + second[1] * (1 - ratio),
		first[2] * ratio + second[2] * (1 - ratio),
	];
}

function relativeLuminance(rgb: Rgb): number {
	const linear = (channel: number) => {
		const value = Math.round(channel) / 255;
		return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
	};
	return 0.2126 * linear(rgb[0]) + 0.7152 * linear(rgb[1]) + 0.0722 * linear(rgb[2]);
}

export function contrastRatio(first: Rgb, second: Rgb): number {
	const a = relativeLuminance(first);
	const b = relativeLuminance(second);
	return (Math.max(a, b) + 0.05) / (Math.min(a, b) + 0.05);
}

function hslColor({hue, saturation, lightness}: Hsl): Color {
	const h = round2(hue);
	const s = round2(saturation);
	const l = round2(lightness);
	return {css: `hsl(${h}, ${s}%, ${l}%)`, rgb: hslToRgb(h, s, l)};
}

function rgbColor(rgb: Rgb): Color {
	const [r, g, b] = rgb.map(round2);
	return {css: `rgb(${r}, ${g}, ${b})`, rgb: [r, g, b]};
}

function resolveTone(tone: ColorTone, saturationFactor: number): Rgb {
	const family = tone.family ? COLOR_FAMILIES[tone.family] : undefined;
	const hue = round2(tone.hue ?? family?.hue ?? 0);
	const saturation = round2(tone.saturation ?? family?.saturation ?? 0);
	const useFactor = tone.useSaturationFactor ?? family?.useSaturationFactor ?? false;
	return hslToRgb(hue, useFactor ? saturation * saturationFactor : saturation, round2(tone.lightness ?? 0));
}

export function resolveThemeSurface(theme: string, name: string, saturationFactor: number): Rgb {
	const palette = getThemePalette(theme);
	const scale = COLOR_SCALES[palette.surfaceScale];
	const target = palette.surfaceAliases[name] ?? name;
	const index = scale.stops.findIndex((stop) => stop.name === target);
	if (index === -1) throw new Error(`${target} is not a stop of ${palette.surfaceScale}`);
	return resolveTone({family: scale.family, lightness: scaleStopLightness(scale, index)}, saturationFactor);
}

export function resolveThemeSurfaces(theme: string, saturationFactor: number): Array<Rgb> {
	const palette = getThemePalette(theme);
	const scale = COLOR_SCALES[palette.surfaceScale];
	const overlay = resolveTone(palette.selectedOverlay, saturationFactor);
	const overlayPercent = (palette.selectedOverlay.alpha ?? 1) * 100;
	const stops = scale.stops.map((_, index) =>
		resolveTone({family: scale.family, lightness: scaleStopLightness(scale, index)}, saturationFactor),
	);
	return [...stops, ...stops.map((stop) => mixRgb(overlay, stop, overlayPercent))];
}

function isLightTheme(theme: string, saturationFactor: number): boolean {
	return relativeLuminance(resolveThemeSurface(theme, '--background-primary', saturationFactor)) > 0.5;
}

function passes(foreground: Rgb, backgrounds: ReadonlyArray<Rgb>): boolean {
	return backgrounds.every((background) => contrastRatio(foreground, background) >= TARGET_CONTRAST);
}

function shiftLightness(start: Hsl, lighter: boolean, accept: (color: Color) => boolean): Color {
	const initial = hslColor(start);
	if (accept(initial)) return initial;
	const step = lighter ? 1 : -1;
	const limit = lighter ? 10000 : 0;
	let failing = Math.round(start.lightness * 100);
	let passing = limit;
	for (let value = failing + step * 100; lighter ? value < limit : value > limit; value += step * 100) {
		if (accept(hslColor({...start, lightness: value / 100}))) {
			passing = value;
			break;
		}
		failing = value;
	}
	while (Math.abs(passing - failing) > 1) {
		const middle = Math.round((passing + failing) / 2);
		if (accept(hslColor({...start, lightness: middle / 100}))) {
			passing = middle;
		} else {
			failing = middle;
		}
	}
	return hslColor({...start, lightness: passing / 100});
}

function neutral(lightness: number): Color {
	return hslColor({hue: 0, saturation: 0, lightness});
}

function pickOnColor(background: Rgb): OnColor {
	const light = {text: neutral(98), fill: neutral(100), dark: false};
	const dark = {text: neutral(8), fill: neutral(8), dark: true};
	const best = contrastRatio(light.text.rgb, background) >= contrastRatio(dark.text.rgb, background) ? light : dark;
	if (contrastRatio(best.text.rgb, background) >= TARGET_CONTRAST) return best;
	const pure = rgbColor(contrastRatio(WHITE, background) >= contrastRatio(BLACK, background) ? WHITE : BLACK);
	return {text: pure, fill: pure, dark: pure.rgb[0] === 0};
}

function readableOnSurfaces(start: Hsl, theme: string, saturationFactor: number, primary: Rgb): Color {
	const surfaces = resolveThemeSurfaces(theme, saturationFactor);
	const stops = surfaces.slice(0, surfaces.length / 2);
	const brandTints = stops.map((stop) => mixRgb(primary, stop, FOREGROUND_BRAND_TINT_RATIO));
	return shiftLightness(start, !isLightTheme(theme, saturationFactor), (color) => {
		const selfTints = stops.map((stop) => mixRgb(color.rgb, stop, FOREGROUND_SELF_TINT_RATIO));
		return passes(color.rgb, surfaces) && passes(color.rgb, selfTints) && passes(color.rgb, brandTints);
	});
}

function largestPassingRatio(text: Rgb, primary: Rgb, surface: Rgb, ratio: number): number {
	for (let candidate = ratio; candidate >= 0; candidate--) {
		if (contrastRatio(text, mixRgb(primary, surface, candidate)) >= TARGET_CONTRAST) return candidate;
	}
	return -1;
}

function brandTint(
	brand: Hsl,
	primary: Rgb,
	theme: string,
	saturationFactor: number,
): {rest: Color; hover: Color; text: Color} {
	const tint = getThemePalette(theme).brandTint;
	const surface = resolveThemeSurface(theme, tint.surface, saturationFactor);
	if (isLightTheme(theme, saturationFactor)) {
		const rest = rgbColor(mixRgb(primary, surface, tint.rest));
		const hover = rgbColor(mixRgb(primary, surface, tint.hover));
		const text = shiftLightness(brand, false, (color) => passes(color.rgb, [rest.rgb, hover.rgb]));
		return {rest, hover, text};
	}
	const options = [neutral(98), neutral(8), rgbColor(WHITE), rgbColor(BLACK)].map((text) => {
		const hover = largestPassingRatio(text.rgb, primary, surface, tint.hover);
		const rest = Math.min(largestPassingRatio(text.rgb, primary, surface, tint.rest), hover);
		return {text, rest, hover};
	});
	const best =
		options.find((option) => option.rest === tint.rest && option.hover === tint.hover) ??
		options.reduce((current, option) => (option.rest + option.hover > current.rest + current.hover ? option : current));
	return {
		rest: rgbColor(mixRgb(primary, surface, best.rest)),
		hover: rgbColor(mixRgb(primary, surface, best.hover)),
		text: best.text,
	};
}

export function getInstanceBrandVariables(
	themeColor: string,
	theme: string,
	saturationFactor: number,
): Record<InstanceBrandVariable, string> | null {
	const rgb = parseHexColor(themeColor);
	if (rgb === null) return null;
	const base = rgbToHsl(rgb);
	const brand = {...base, saturation: base.saturation * saturationFactor};
	const primary = hslColor(brand);
	const onPrimary = pickOnColor(primary.rgb);
	const secondary = shiftLightness(
		{hue: brand.hue, saturation: brand.saturation * (6 / 7), lightness: brand.lightness * 0.89},
		onPrimary.dark,
		(color) => passes(onPrimary.text.rgb, [color.rgb]) && passes(onPrimary.fill.rgb, [color.rgb]),
	);
	const shade = rgbColor(onPrimary.dark ? WHITE : BLACK);
	const foreground = readableOnSurfaces(brand, theme, saturationFactor, primary.rgb);
	const light = readableOnSurfaces(
		{...brand, lightness: brand.lightness + (100 - brand.lightness) * 0.65},
		theme,
		saturationFactor,
		primary.rgb,
	);
	const onLight = pickOnColor(light.rgb);
	const tint = brandTint(brand, primary.rgb, theme, saturationFactor);
	return {
		'--instance-brand-primary': primary.css,
		'--instance-brand-secondary': secondary.css,
		'--instance-brand-primary-shade': shade.css,
		'--instance-brand-primary-light': light.css,
		'--instance-brand-primary-foreground': foreground.css,
		'--instance-brand-primary-fill': onPrimary.fill.css,
		'--instance-brand-primary-light-fill': onLight.fill.css,
		'--instance-text-on-brand-primary': onPrimary.text.css,
		'--instance-text-on-brand-primary-light': onLight.text.css,
		'--instance-brand-tint': tint.rest.css,
		'--instance-brand-tint-hover': tint.hover.css,
		'--instance-brand-tint-border': foreground.css,
		'--instance-text-on-brand-tint': tint.text.css,
	};
}
