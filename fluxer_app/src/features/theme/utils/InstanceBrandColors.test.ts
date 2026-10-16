// SPDX-License-Identifier: AGPL-3.0-or-later

import {BRAND_PRESSED_RATIO, THEME_PALETTES} from '@app/features/theme/utils/ColorSystemPalette';
import {
	contrastRatio,
	FOREGROUND_BRAND_TINT_RATIO,
	FOREGROUND_SELF_TINT_RATIO,
	getInstanceBrandVariables,
	hslToRgb,
	MIN_TEXT_CONTRAST,
	mixRgb,
	resolveThemeSurface,
	resolveThemeSurfaces,
} from '@app/features/theme/utils/InstanceBrandColors';
import {describe, expect, test} from 'vitest';

type Rgb = readonly [number, number, number];

const MIN_UI_CONTRAST = 3;
const THEMES = Object.keys(THEME_PALETTES);
const SATURATION_FACTORS = [1, 0.5, 0];
const NAMED_COLORS = [
	'#ffffff',
	'#f0f0f0',
	'#ffff00',
	'#00ff00',
	'#00ffff',
	'#ff00ff',
	'#ff0000',
	'#0000ff',
	'#e53935',
	'#4641d9',
	'#7f7f7f',
	'#000001',
	'#0b1a3a',
	'#ffd1dc',
];

function hex(rgb: Rgb): string {
	return `#${rgb.map((channel) => Math.round(channel).toString(16).padStart(2, '0')).join('')}`;
}

function gridColors(): Array<string> {
	const colors: Array<string> = [];
	for (let hue = 0; hue < 360; hue += 30) {
		for (const saturation of [100, 35]) {
			for (const lightness of [3, 15, 30, 50, 70, 85, 97]) {
				colors.push(hex(hslToRgb(hue, saturation, lightness)));
			}
		}
	}
	return colors;
}

function parseColor(value: string): Rgb {
	const hsl = /^hsl\(([\d.]+), ([\d.]+)%, ([\d.]+)%\)$/.exec(value);
	if (hsl) return hslToRgb(Number(hsl[1]), Number(hsl[2]), Number(hsl[3]));
	const rgb = /^rgb\(([\d.]+), ([\d.]+), ([\d.]+)\)$/.exec(value);
	if (rgb) return [Number(rgb[1]), Number(rgb[2]), Number(rgb[3])];
	throw new Error(`Unparseable colour ${value}`);
}

function minContrast(foreground: Rgb, backgrounds: ReadonlyArray<Rgb>): number {
	return Math.min(...backgrounds.map((background) => contrastRatio(foreground, background)));
}

function sameColor(first: Rgb, second: Rgb): boolean {
	return first.every((channel, index) => Math.abs(channel - second[index]) < 0.01);
}

const COLORS = [...NAMED_COLORS, ...gridColors()];
const CASES = THEMES.flatMap((theme) =>
	SATURATION_FACTORS.flatMap((saturationFactor) => COLORS.map((color) => ({theme, saturationFactor, color}))),
);

describe('getInstanceBrandVariables', () => {
	test('rejects anything but a lowercase six digit hex colour', () => {
		expect(getInstanceBrandVariables('#FFF', 'dark', 1)).toBeNull();
		expect(getInstanceBrandVariables('red', 'dark', 1)).toBeNull();
	});

	test('keeps the operator colour as the solid fill', () => {
		expect(getInstanceBrandVariables('#ffff00', 'light', 1)?.['--instance-brand-primary']).toBe('hsl(60, 100%, 50%)');
		expect(getInstanceBrandVariables('#e53935', 'dark', 0.5)?.['--instance-brand-primary']).toBe(
			'hsl(1.36, 38.6%, 55.29%)',
		);
	});

	test('treats an unknown theme like the dark root palette', () => {
		expect(getInstanceBrandVariables('#ffff00', 'unknown', 1)).toEqual(getInstanceBrandVariables('#ffff00', 'dark', 1));
	});

	test.each(CASES)(
		'$color in $theme at saturation $saturationFactor meets contrast minimums',
		({theme, saturationFactor, color}) => {
			const variables = getInstanceBrandVariables(color, theme, saturationFactor);
			expect(variables).not.toBeNull();
			const v = Object.fromEntries(
				Object.entries(variables!).map(([name, value]) => [name.replace('--instance-', ''), parseColor(value)]),
			);
			const surfaces = resolveThemeSurfaces(theme, saturationFactor);
			const stops = surfaces.slice(0, surfaces.length / 2);
			const pressed = mixRgb(v['brand-primary'], v['brand-primary-shade'], BRAND_PRESSED_RATIO);
			const solidFills = [v['brand-primary'], v['brand-secondary'], pressed];
			expect(minContrast(v['text-on-brand-primary'], solidFills)).toBeGreaterThanOrEqual(MIN_TEXT_CONTRAST);
			expect(minContrast(v['brand-primary-fill'], solidFills)).toBeGreaterThanOrEqual(MIN_TEXT_CONTRAST);
			expect(contrastRatio(v['text-on-brand-primary-light'], v['brand-primary-light'])).toBeGreaterThanOrEqual(
				MIN_TEXT_CONTRAST,
			);
			expect(contrastRatio(v['brand-primary-light-fill'], v['brand-primary-light'])).toBeGreaterThanOrEqual(
				MIN_TEXT_CONTRAST,
			);
			for (const name of ['brand-primary-foreground', 'brand-primary-light']) {
				const foreground = v[name];
				const tinted = [
					...stops.map((stop) => mixRgb(foreground, stop, FOREGROUND_SELF_TINT_RATIO)),
					...stops.map((stop) => mixRgb(v['brand-primary'], stop, FOREGROUND_BRAND_TINT_RATIO)),
				];
				expect(minContrast(foreground, surfaces)).toBeGreaterThanOrEqual(MIN_TEXT_CONTRAST);
				expect(minContrast(foreground, tinted)).toBeGreaterThanOrEqual(MIN_TEXT_CONTRAST);
			}
			expect(minContrast(v['brand-tint-border'], surfaces)).toBeGreaterThanOrEqual(MIN_UI_CONTRAST);
			expect(minContrast(v['text-on-brand-tint'], [v['brand-tint'], v['brand-tint-hover']])).toBeGreaterThanOrEqual(
				MIN_TEXT_CONTRAST,
			);
			const tint = THEME_PALETTES[theme as keyof typeof THEME_PALETTES].brandTint;
			const tintSurface = resolveThemeSurface(theme, tint.surface, saturationFactor);
			const matchingRatio = (target: Rgb, max: number) =>
				Array.from({length: max + 1}, (_, ratio) => max - ratio).find((ratio) =>
					sameColor(target, mixRgb(v['brand-primary'], tintSurface, ratio)),
				);
			expect(matchingRatio(v['brand-tint'], tint.rest)).toBeDefined();
			expect(matchingRatio(v['brand-tint-hover'], tint.hover)).toBeDefined();
		},
	);

	test('keeps the stylesheet tint ratios whenever the text already passes', () => {
		const variables = getInstanceBrandVariables('#4641d9', 'dark', 1)!;
		const surface = resolveThemeSurface('dark', THEME_PALETTES.dark.brandTint.surface, 1);
		const primary = parseColor(variables['--instance-brand-primary']);
		expect(sameColor(parseColor(variables['--instance-brand-tint']), mixRgb(primary, surface, 36))).toBe(true);
		expect(sameColor(parseColor(variables['--instance-brand-tint-hover']), mixRgb(primary, surface, 48))).toBe(true);
	});

	test('reads light text on the dark reaction tint even when the brand needs dark text', () => {
		const variables = getInstanceBrandVariables('#e53935', 'dark', 1)!;
		expect(variables['--instance-text-on-brand-primary']).toBe('rgb(0, 0, 0)');
		expect(variables['--instance-text-on-brand-tint']).toBe('hsl(0, 0%, 98%)');
	});
});
