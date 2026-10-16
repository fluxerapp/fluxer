// SPDX-License-Identifier: AGPL-3.0-or-later

import {ThemeTypes} from '@fluxer/constants/src/UserConstants';

export interface ColorFamily {
	hue: number;
	saturation: number;
	useSaturationFactor: boolean;
}

interface ScaleStop {
	name: string;
	position?: number;
}

export interface Scale {
	family: string;
	range: [number, number];
	curve: 'linear' | 'easeIn' | 'easeOut' | 'easeInOut';
	stops: Array<ScaleStop>;
}

export interface ColorTone {
	family?: string;
	hue?: number;
	saturation?: number;
	lightness?: number;
	alpha?: number;
	useSaturationFactor?: boolean;
}

export type ColorSystemTheme =
	| typeof ThemeTypes.DARK
	| typeof ThemeTypes.DARK_LEGACY
	| typeof ThemeTypes.COAL
	| typeof ThemeTypes.LIGHT;

interface BrandTint {
	surface: string;
	rest: number;
	hover: number;
}

interface ThemePalette {
	surfaceScale: string;
	surfaceAliases: Record<string, string>;
	selectedOverlay: ColorTone;
	brandTint: BrandTint;
}

export const COLOR_FAMILIES: Record<string, ColorFamily> = {
	neutralDark: {hue: 258, saturation: 10, useSaturationFactor: true},
	neutralLight: {hue: 220, saturation: 10, useSaturationFactor: true},
	brand: {hue: 242, saturation: 70, useSaturationFactor: true},
	link: {hue: 198, saturation: 92, useSaturationFactor: true},
	accentPurple: {hue: 278, saturation: 85, useSaturationFactor: true},
	statusOnline: {hue: 152, saturation: 72, useSaturationFactor: true},
	legacyDark: {hue: 220, saturation: 13, useSaturationFactor: true},
	legacyLink: {hue: 210, saturation: 100, useSaturationFactor: true},
	legacyAccentPurple: {hue: 270, saturation: 80, useSaturationFactor: true},
	legacyStatusOnline: {hue: 142, saturation: 76, useSaturationFactor: true},
	statusIdle: {hue: 45, saturation: 93, useSaturationFactor: true},
	statusDnd: {hue: 0, saturation: 84, useSaturationFactor: true},
	statusOffline: {hue: 218, saturation: 11, useSaturationFactor: true},
	statusDanger: {hue: 1, saturation: 77, useSaturationFactor: true},
	textCode: {hue: 340, saturation: 50, useSaturationFactor: true},
	brandIcon: {hue: 38, saturation: 92, useSaturationFactor: true},
};

export const COLOR_SCALES: Record<string, Scale> = {
	darkSurface: {
		family: 'neutralDark',
		range: [5, 24],
		curve: 'easeOut',
		stops: [
			{name: '--background-primary', position: 0},
			{name: '--background-secondary', position: 0.16},
			{name: '--background-secondary-lighter', position: 0.22},
			{name: '--background-secondary-alt', position: 0.28},
			{name: '--background-tertiary', position: 0.4},
			{name: '--background-channel-header', position: 0.34},
			{name: '--guild-list-foreground', position: 0.38},
			{name: '--background-header-secondary', position: 0.5},
			{name: '--background-header-primary', position: 0.5},
			{name: '--background-textarea', position: 0.3},
			{name: '--background-header-primary-hover', position: 0.85},
		],
	},
	coalSurface: {
		family: 'neutralDark',
		range: [1, 12],
		curve: 'easeOut',
		stops: [
			{name: '--background-primary', position: 0},
			{name: '--background-secondary', position: 0.16},
			{name: '--background-secondary-alt', position: 0.28},
			{name: '--background-tertiary', position: 0.4},
			{name: '--background-channel-header', position: 0.34},
			{name: '--guild-list-foreground', position: 0.38},
			{name: '--background-header-secondary', position: 0.5},
			{name: '--background-header-primary', position: 0.5},
			{name: '--background-textarea', position: 0.3},
			{name: '--background-header-primary-hover', position: 0.85},
		],
	},
	darkText: {
		family: 'neutralDark',
		range: [60, 96],
		curve: 'easeInOut',
		stops: [
			{name: '--text-tertiary-secondary', position: 0},
			{name: '--text-tertiary-muted', position: 0.2},
			{name: '--text-tertiary', position: 0.38},
			{name: '--text-primary-muted', position: 0.55},
			{name: '--text-chat-muted', position: 0.55},
			{name: '--text-secondary', position: 0.72},
			{name: '--text-chat', position: 0.82},
			{name: '--text-primary', position: 1},
		],
	},
	lightSurface: {
		family: 'neutralLight',
		range: [86, 98.5],
		curve: 'easeIn',
		stops: [
			{name: '--background-header-primary-hover', position: 0},
			{name: '--background-header-primary', position: 0.12},
			{name: '--background-header-secondary', position: 0.2},
			{name: '--guild-list-foreground', position: 0.35},
			{name: '--background-tertiary', position: 0.42},
			{name: '--background-channel-header', position: 0.5},
			{name: '--background-secondary-alt', position: 0.63},
			{name: '--background-secondary', position: 0.74},
			{name: '--background-secondary-lighter', position: 0.83},
			{name: '--background-textarea', position: 0.88},
			{name: '--background-primary', position: 1},
		],
	},
	lightText: {
		family: 'neutralLight',
		range: [15, 54],
		curve: 'easeOut',
		stops: [
			{name: '--text-primary', position: 0},
			{name: '--text-chat', position: 0.08},
			{name: '--text-secondary', position: 0.28},
			{name: '--text-chat-muted', position: 0.45},
			{name: '--text-primary-muted', position: 0.45},
			{name: '--text-tertiary', position: 0.6},
			{name: '--text-tertiary-secondary', position: 0.75},
			{name: '--text-tertiary-muted', position: 0.85},
		],
	},
	legacyDarkSurface: {
		family: 'legacyDark',
		range: [5, 26],
		curve: 'easeOut',
		stops: [
			{name: '--background-primary', position: 0},
			{name: '--background-secondary', position: 0.16},
			{name: '--background-secondary-lighter', position: 0.22},
			{name: '--background-secondary-alt', position: 0.28},
			{name: '--background-tertiary', position: 0.4},
			{name: '--background-channel-header', position: 0.34},
			{name: '--guild-list-foreground', position: 0.38},
			{name: '--background-header-secondary', position: 0.5},
			{name: '--background-header-primary', position: 0.5},
			{name: '--background-textarea', position: 0.3},
			{name: '--background-header-primary-hover', position: 0.85},
		],
	},
	legacyDarkText: {
		family: 'legacyDark',
		range: [52, 96],
		curve: 'easeInOut',
		stops: [
			{name: '--text-tertiary-secondary', position: 0},
			{name: '--text-tertiary-muted', position: 0.2},
			{name: '--text-tertiary', position: 0.38},
			{name: '--text-primary-muted', position: 0.55},
			{name: '--text-chat-muted', position: 0.55},
			{name: '--text-secondary', position: 0.72},
			{name: '--text-chat', position: 0.82},
			{name: '--text-primary', position: 1},
		],
	},
};

const DARK_BRAND_TINT: BrandTint = {surface: '--background-secondary', rest: 36, hover: 48};

export const THEME_PALETTES: Record<ColorSystemTheme, ThemePalette> = {
	[ThemeTypes.DARK]: {
		surfaceScale: 'darkSurface',
		surfaceAliases: {},
		selectedOverlay: {family: 'neutralDark', lightness: 100, alpha: 0.1},
		brandTint: DARK_BRAND_TINT,
	},
	[ThemeTypes.COAL]: {
		surfaceScale: 'coalSurface',
		surfaceAliases: {
			'--background-secondary': '--background-primary',
			'--background-secondary-lighter': '--background-primary',
		},
		selectedOverlay: {family: 'neutralDark', lightness: 100, alpha: 0.08},
		brandTint: DARK_BRAND_TINT,
	},
	[ThemeTypes.DARK_LEGACY]: {
		surfaceScale: 'legacyDarkSurface',
		surfaceAliases: {},
		selectedOverlay: {family: 'legacyDark', lightness: 100, alpha: 0.1},
		brandTint: DARK_BRAND_TINT,
	},
	[ThemeTypes.LIGHT]: {
		surfaceScale: 'lightSurface',
		surfaceAliases: {},
		selectedOverlay: {family: 'neutralLight', saturation: 10, lightness: 10, alpha: 0.1},
		brandTint: {surface: '--background-primary', rest: 12, hover: 18},
	},
};

export const BRAND_PRESSED_RATIO = 90;

export function getThemePalette(theme: string): ThemePalette {
	return THEME_PALETTES[theme as ColorSystemTheme] ?? THEME_PALETTES[ThemeTypes.DARK];
}

export function brandTintMix(tint: BrandTint, state: 'rest' | 'hover'): string {
	const ratio = tint[state];
	return `color-mix(in srgb, var(--brand-primary) ${ratio}%, var(${tint.surface}) ${100 - ratio}%)`;
}

function clamp01(value: number): number {
	return Math.min(1, Math.max(0, value));
}

function applyCurve(curve: Scale['curve'], t: number): number {
	switch (curve) {
		case 'easeIn':
			return t * t;
		case 'easeOut':
			return 1 - (1 - t) * (1 - t);
		case 'easeInOut':
			if (t < 0.5) {
				return 2 * t * t;
			}
			return 1 - 2 * (1 - t) * (1 - t);
		default:
			return t;
	}
}

export function scaleStopLightness(scale: Scale, index: number): number {
	const lastIndex = Math.max(scale.stops.length - 1, 1);
	const stop = scale.stops[index];
	const position = stop.position !== undefined ? clamp01(stop.position) : index / lastIndex;
	const eased = applyCurve(scale.curve, position);
	const lightness = scale.range[0] + (scale.range[1] - scale.range[0]) * eased;
	return Math.round(lightness * 1000) / 1000;
}
