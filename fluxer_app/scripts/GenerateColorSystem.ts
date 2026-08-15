// SPDX-License-Identifier: AGPL-3.0-or-later

import {mkdirSync, writeFileSync} from 'node:fs';
import {dirname, join, relative} from 'node:path';

interface ColorFamily {
	hue: number;
	saturation: number;
	useSaturationFactor: boolean;
}

interface ScaleStop {
	name: string;
	position?: number;
}

interface Scale {
	family: string;
	range: [number, number];
	curve: 'linear' | 'easeIn' | 'easeOut' | 'easeInOut';
	stops: Array<ScaleStop>;
}

interface TokenDef {
	name?: string;
	scale?: string;
	value?: string;
	family?: string;
	hue?: number;
	saturation?: number;
	lightness?: number;
	alpha?: number;
	useSaturationFactor?: boolean;
}

interface Config {
	families: Record<string, ColorFamily>;
	scales: Record<string, Scale>;
	tokens: {
		root: Array<TokenDef>;
		light: Array<TokenDef>;
		coal: Array<TokenDef>;
		darkLegacy: Array<TokenDef>;
	};
}

const DARK_CODE_TOKENS: Array<TokenDef> = [
	{
		name: '--code-text',
		value: 'color-mix(in srgb, var(--text-secondary) 82%, hsl(340, calc(50% * var(--saturation-factor)), 90%) 18%)',
	},
	{name: '--text-code', value: 'var(--code-text)'},
	{name: '--code-muted', value: 'color-mix(in srgb, var(--text-code) 42%, var(--text-tertiary) 58%)'},
	{
		name: '--code-inline-bg',
		value: 'color-mix(in srgb, var(--background-secondary-alt) 88%, var(--background-tertiary) 12%)',
	},
	{name: '--bg-code', value: 'var(--code-inline-bg)'},
	{
		name: '--code-block-bg',
		value: 'color-mix(in srgb, var(--background-secondary-alt) 92%, var(--background-primary) 8%)',
	},
	{name: '--bg-code-block', value: 'var(--code-block-bg)'},
	{name: '--code-block-border', value: 'color-mix(in srgb, var(--border-color) 90%, var(--text-code) 10%)'},
	{name: '--code-block-highlight', value: 'color-mix(in srgb, var(--text-primary) 10%, transparent)'},
	{name: '--ansi-inverse-text', value: 'var(--code-block-bg)'},
	{name: '--ansi-inverse-bg', value: 'var(--text-code)'},
	{name: '--ansi-fg-black', value: 'color-mix(in srgb, hsl(0, 0%, 12%) 72%, var(--text-tertiary) 28%)'},
	{
		name: '--ansi-fg-red',
		value: 'color-mix(in srgb, hsl(0, calc(88% * var(--saturation-factor)), 62%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-green',
		value: 'color-mix(in srgb, hsl(99, calc(29% * var(--saturation-factor)), 47%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-yellow',
		value: 'color-mix(in srgb, hsl(41, calc(53% * var(--saturation-factor)), 67%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-blue',
		value: 'color-mix(in srgb, hsl(207, calc(61% * var(--saturation-factor)), 59%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-magenta',
		value: 'color-mix(in srgb, hsl(305, calc(35% * var(--saturation-factor)), 65%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-cyan',
		value: 'color-mix(in srgb, hsl(168, calc(53% * var(--saturation-factor)), 55%) 92%, var(--text-code) 8%)',
	},
	{name: '--ansi-fg-white', value: 'color-mix(in srgb, hsl(0, 0%, 83%) 88%, var(--text-secondary) 12%)'},
	{name: '--ansi-fg-bright-black', value: 'color-mix(in srgb, hsl(0, 0%, 50%) 86%, var(--text-tertiary) 14%)'},
	{
		name: '--ansi-fg-bright-red',
		value: 'color-mix(in srgb, hsl(0, calc(88% * var(--saturation-factor)), 62%) 94%, var(--text-primary) 6%)',
	},
	{
		name: '--ansi-fg-bright-green',
		value: 'color-mix(in srgb, hsl(99, calc(29% * var(--saturation-factor)), 47%) 88%, var(--text-primary) 12%)',
	},
	{
		name: '--ansi-fg-bright-yellow',
		value: 'color-mix(in srgb, hsl(41, calc(53% * var(--saturation-factor)), 67%) 94%, var(--text-primary) 6%)',
	},
	{
		name: '--ansi-fg-bright-blue',
		value: 'color-mix(in srgb, hsl(207, calc(61% * var(--saturation-factor)), 59%) 94%, var(--text-primary) 6%)',
	},
	{
		name: '--ansi-fg-bright-magenta',
		value: 'color-mix(in srgb, hsl(305, calc(35% * var(--saturation-factor)), 65%) 94%, var(--text-primary) 6%)',
	},
	{
		name: '--ansi-fg-bright-cyan',
		value: 'color-mix(in srgb, hsl(168, calc(53% * var(--saturation-factor)), 55%) 94%, var(--text-primary) 6%)',
	},
	{name: '--ansi-fg-bright-white', value: 'var(--text-primary)'},
	{name: '--ansi-bg-black', value: 'color-mix(in srgb, hsl(0, 0%, 12%) 88%, var(--code-block-bg) 12%)'},
	{
		name: '--ansi-bg-red',
		value: 'color-mix(in srgb, hsl(0, calc(88% * var(--saturation-factor)), 62%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-green',
		value: 'color-mix(in srgb, hsl(99, calc(29% * var(--saturation-factor)), 47%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-yellow',
		value: 'color-mix(in srgb, hsl(41, calc(53% * var(--saturation-factor)), 67%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-blue',
		value: 'color-mix(in srgb, hsl(207, calc(61% * var(--saturation-factor)), 59%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-magenta',
		value: 'color-mix(in srgb, hsl(305, calc(35% * var(--saturation-factor)), 65%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-cyan',
		value: 'color-mix(in srgb, hsl(168, calc(53% * var(--saturation-factor)), 55%) 88%, var(--code-block-bg) 12%)',
	},
	{name: '--ansi-bg-white', value: 'color-mix(in srgb, hsl(0, 0%, 83%) 88%, var(--code-block-bg) 12%)'},
	{name: '--ansi-bg-bright-black', value: 'color-mix(in srgb, hsl(0, 0%, 50%) 88%, var(--code-block-bg) 12%)'},
	{
		name: '--ansi-bg-bright-red',
		value: 'color-mix(in srgb, hsl(0, calc(88% * var(--saturation-factor)), 62%) 92%, var(--code-block-bg) 8%)',
	},
	{
		name: '--ansi-bg-bright-green',
		value: 'color-mix(in srgb, hsl(99, calc(29% * var(--saturation-factor)), 47%) 92%, var(--code-block-bg) 8%)',
	},
	{
		name: '--ansi-bg-bright-yellow',
		value: 'color-mix(in srgb, hsl(41, calc(53% * var(--saturation-factor)), 67%) 92%, var(--code-block-bg) 8%)',
	},
	{
		name: '--ansi-bg-bright-blue',
		value: 'color-mix(in srgb, hsl(207, calc(61% * var(--saturation-factor)), 59%) 92%, var(--code-block-bg) 8%)',
	},
	{
		name: '--ansi-bg-bright-magenta',
		value: 'color-mix(in srgb, hsl(305, calc(35% * var(--saturation-factor)), 65%) 92%, var(--code-block-bg) 8%)',
	},
	{
		name: '--ansi-bg-bright-cyan',
		value: 'color-mix(in srgb, hsl(168, calc(53% * var(--saturation-factor)), 55%) 92%, var(--code-block-bg) 8%)',
	},
	{name: '--ansi-bg-bright-white', value: 'color-mix(in srgb, hsl(0, 0%, 100%) 92%, var(--code-block-bg) 8%)'},
];
const LIGHT_CODE_TOKENS: Array<TokenDef> = [
	{
		name: '--code-text',
		value: 'color-mix(in srgb, var(--text-secondary) 72%, hsl(340, calc(50% * var(--saturation-factor)), 38%) 28%)',
	},
	{name: '--text-code', value: 'var(--code-text)'},
	{name: '--code-muted', value: 'color-mix(in srgb, var(--text-code) 34%, var(--text-tertiary) 66%)'},
	{name: '--code-inline-bg', value: 'color-mix(in srgb, var(--background-secondary-alt) 92%, var(--text-code) 8%)'},
	{name: '--bg-code', value: 'var(--code-inline-bg)'},
	{
		name: '--code-block-bg',
		value: 'color-mix(in srgb, var(--background-primary) 88%, var(--background-secondary) 12%)',
	},
	{name: '--bg-code-block', value: 'var(--code-block-bg)'},
	{name: '--code-block-border', value: 'color-mix(in srgb, var(--border-color) 92%, var(--text-code) 8%)'},
	{name: '--code-block-highlight', value: 'color-mix(in srgb, hsl(0, 0%, 100%) 82%, transparent)'},
	{name: '--ansi-inverse-text', value: 'var(--code-block-bg)'},
	{name: '--ansi-inverse-bg', value: 'var(--text-code)'},
	{name: '--ansi-fg-black', value: 'var(--text-primary)'},
	{
		name: '--ansi-fg-red',
		value: 'color-mix(in srgb, hsl(0, calc(62% * var(--saturation-factor)), 50%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-green',
		value: 'color-mix(in srgb, hsl(120, calc(100% * var(--saturation-factor)), 25%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-yellow',
		value: 'color-mix(in srgb, hsl(37, calc(52% * var(--saturation-factor)), 31%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-blue',
		value: 'color-mix(in srgb, hsl(211, calc(95% * var(--saturation-factor)), 33%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-magenta',
		value: 'color-mix(in srgb, hsl(288, calc(100% * var(--saturation-factor)), 43%) 92%, var(--text-code) 8%)',
	},
	{
		name: '--ansi-fg-cyan',
		value: 'color-mix(in srgb, hsl(192, calc(95% * var(--saturation-factor)), 38%) 92%, var(--text-code) 8%)',
	},
	{name: '--ansi-fg-white', value: 'color-mix(in srgb, hsl(0, 0%, 33%) 88%, var(--text-secondary) 12%)'},
	{name: '--ansi-fg-bright-black', value: 'color-mix(in srgb, hsl(0, 0%, 40%) 88%, var(--text-tertiary) 12%)'},
	{
		name: '--ansi-fg-bright-red',
		value: 'color-mix(in srgb, hsl(0, calc(62% * var(--saturation-factor)), 50%) 92%, var(--text-primary) 8%)',
	},
	{
		name: '--ansi-fg-bright-green',
		value: 'color-mix(in srgb, hsl(120, calc(82% * var(--saturation-factor)), 44%) 90%, var(--text-primary) 10%)',
	},
	{
		name: '--ansi-fg-bright-yellow',
		value: 'color-mix(in srgb, hsl(62, calc(100% * var(--saturation-factor)), 36%) 90%, var(--text-primary) 10%)',
	},
	{
		name: '--ansi-fg-bright-blue',
		value: 'color-mix(in srgb, hsl(211, calc(95% * var(--saturation-factor)), 33%) 92%, var(--text-primary) 8%)',
	},
	{
		name: '--ansi-fg-bright-magenta',
		value: 'color-mix(in srgb, hsl(300, calc(95% * var(--saturation-factor)), 38%) 92%, var(--text-primary) 8%)',
	},
	{
		name: '--ansi-fg-bright-cyan',
		value: 'color-mix(in srgb, hsl(192, calc(95% * var(--saturation-factor)), 38%) 92%, var(--text-primary) 8%)',
	},
	{name: '--ansi-fg-bright-white', value: 'color-mix(in srgb, hsl(0, 0%, 65%) 86%, var(--text-primary) 14%)'},
	{name: '--ansi-bg-black', value: 'color-mix(in srgb, hsl(0, 0%, 0%) 86%, var(--code-block-bg) 14%)'},
	{
		name: '--ansi-bg-red',
		value: 'color-mix(in srgb, hsl(0, calc(62% * var(--saturation-factor)), 50%) 86%, var(--code-block-bg) 14%)',
	},
	{
		name: '--ansi-bg-green',
		value: 'color-mix(in srgb, hsl(120, calc(100% * var(--saturation-factor)), 25%) 86%, var(--code-block-bg) 14%)',
	},
	{
		name: '--ansi-bg-yellow',
		value: 'color-mix(in srgb, hsl(37, calc(52% * var(--saturation-factor)), 31%) 86%, var(--code-block-bg) 14%)',
	},
	{
		name: '--ansi-bg-blue',
		value: 'color-mix(in srgb, hsl(211, calc(95% * var(--saturation-factor)), 33%) 86%, var(--code-block-bg) 14%)',
	},
	{
		name: '--ansi-bg-magenta',
		value: 'color-mix(in srgb, hsl(288, calc(100% * var(--saturation-factor)), 43%) 86%, var(--code-block-bg) 14%)',
	},
	{
		name: '--ansi-bg-cyan',
		value: 'color-mix(in srgb, hsl(192, calc(95% * var(--saturation-factor)), 38%) 86%, var(--code-block-bg) 14%)',
	},
	{name: '--ansi-bg-white', value: 'color-mix(in srgb, hsl(0, 0%, 90%) 86%, var(--code-block-bg) 14%)'},
	{name: '--ansi-bg-bright-black', value: 'color-mix(in srgb, hsl(0, 0%, 40%) 88%, var(--code-block-bg) 12%)'},
	{
		name: '--ansi-bg-bright-red',
		value: 'color-mix(in srgb, hsl(0, calc(62% * var(--saturation-factor)), 50%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-bright-green',
		value: 'color-mix(in srgb, hsl(120, calc(82% * var(--saturation-factor)), 44%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-bright-yellow',
		value: 'color-mix(in srgb, hsl(62, calc(100% * var(--saturation-factor)), 36%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-bright-blue',
		value: 'color-mix(in srgb, hsl(211, calc(95% * var(--saturation-factor)), 33%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-bright-magenta',
		value: 'color-mix(in srgb, hsl(300, calc(95% * var(--saturation-factor)), 38%) 88%, var(--code-block-bg) 12%)',
	},
	{
		name: '--ansi-bg-bright-cyan',
		value: 'color-mix(in srgb, hsl(192, calc(95% * var(--saturation-factor)), 38%) 88%, var(--code-block-bg) 12%)',
	},
	{name: '--ansi-bg-bright-white', value: 'color-mix(in srgb, hsl(0, 0%, 90%) 90%, var(--code-block-bg) 10%)'},
];
const CONFIG: Config = {
	families: {
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
	},
	scales: {
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
	},
	tokens: {
		root: [
			{scale: 'darkSurface'},
			{scale: 'darkText'},
			{
				name: '--panel-control-bg',
				value: `color-mix(
in srgb,
var(--background-secondary-alt) 90%,
hsl(258, calc(10% * var(--saturation-factor)), 2%) 10%
)`,
			},
			{name: '--panel-control-border', family: 'neutralDark', saturation: 30, lightness: 65, alpha: 0.45},
			{name: '--panel-control-divider', family: 'neutralDark', saturation: 30, lightness: 55, alpha: 0.35},
			{name: '--panel-control-highlight', value: 'hsla(0, 0%, 100%, 0.04)'},
			{name: '--background-modifier-hover', family: 'neutralDark', lightness: 100, alpha: 0.05},
			{name: '--background-modifier-selected', family: 'neutralDark', lightness: 100, alpha: 0.1},
			{name: '--background-modifier-accent', family: 'neutralDark', saturation: 13, lightness: 80, alpha: 0.15},
			{name: '--background-modifier-accent-focus', family: 'neutralDark', saturation: 13, lightness: 80, alpha: 0.22},
			{name: '--control-button-normal-bg', value: 'transparent'},
			{name: '--control-button-normal-text', value: 'var(--text-primary-muted)'},
			{name: '--control-button-hover-bg', family: 'neutralDark', lightness: 22},
			{name: '--control-button-hover-text', value: 'var(--text-primary)'},
			{name: '--control-button-active-bg', family: 'neutralDark', lightness: 24},
			{name: '--control-button-active-text', value: 'var(--text-primary)'},
			{name: '--control-button-danger-text', hue: 1, saturation: 77, useSaturationFactor: true, lightness: 60},
			{name: '--control-button-danger-hover-bg', hue: 1, saturation: 77, useSaturationFactor: true, lightness: 20},
			{name: '--brand-primary', family: 'brand', lightness: 55},
			{name: '--brand-secondary', family: 'brand', saturation: 60, lightness: 49},
			{name: '--brand-primary-light', family: 'brand', saturation: 100, lightness: 84},
			{name: '--brand-primary-fill', hue: 0, saturation: 0, lightness: 100},
			{name: '--status-online', family: 'statusOnline', lightness: 40},
			{name: '--status-idle', family: 'statusIdle', lightness: 50},
			{name: '--status-dnd', family: 'statusDnd', lightness: 60},
			{name: '--status-offline', family: 'statusOffline', lightness: 65},
			{name: '--status-danger', family: 'statusDanger', lightness: 55},
			{name: '--status-warning', value: 'var(--status-idle)'},
			{name: '--text-warning', family: 'statusIdle', lightness: 55},
			{name: '--plutonium', value: 'var(--brand-primary)'},
			{name: '--plutonium-hover', value: 'var(--brand-secondary)'},
			{name: '--plutonium-text', value: 'var(--text-on-brand-primary)'},
			{name: '--plutonium-icon', family: 'brandIcon', lightness: 50},
			{name: '--invite-verified-icon-color', value: 'var(--text-on-brand-primary)'},
			{name: '--text-link', family: 'link', lightness: 70},
			{name: '--text-on-brand-primary', hue: 0, saturation: 0, lightness: 98},
			...DARK_CODE_TOKENS,
			{name: '--text-selection', hue: 198, saturation: 92, useSaturationFactor: true, lightness: 70, alpha: 0.35},
			{name: '--markup-mention-text', value: 'var(--text-link)'},
			{name: '--markup-mention-fill', value: 'color-mix(in srgb, var(--text-link) 20%, transparent)'},
			{name: '--markup-mention-border', family: 'link', lightness: 70, alpha: 0.3},
			{name: '--markup-jump-link-text', value: 'var(--text-link)'},
			{name: '--markup-jump-link-fill', value: 'color-mix(in srgb, var(--text-link) 12%, transparent)'},
			{name: '--markup-jump-link-hover-fill', value: 'color-mix(in srgb, var(--text-link) 20%, transparent)'},
			{name: '--markup-everyone-text', hue: 250, saturation: 80, useSaturationFactor: true, lightness: 75},
			{
				name: '--markup-everyone-fill',
				value: 'color-mix(in srgb, hsl(250, calc(80% * var(--saturation-factor)), 75%) 18%, transparent)',
			},
			{
				name: '--markup-everyone-border',
				hue: 250,
				saturation: 80,
				useSaturationFactor: true,
				lightness: 75,
				alpha: 0.3,
			},
			{name: '--markup-here-text', hue: 45, saturation: 90, useSaturationFactor: true, lightness: 70},
			{
				name: '--markup-here-fill',
				value: 'color-mix(in srgb, hsl(45, calc(90% * var(--saturation-factor)), 70%) 18%, transparent)',
			},
			{name: '--markup-here-border', hue: 45, saturation: 90, useSaturationFactor: true, lightness: 70, alpha: 0.3},
			{name: '--markup-interactive-hover-text', value: 'var(--text-link)'},
			{name: '--markup-interactive-hover-fill', value: 'color-mix(in srgb, var(--text-link) 30%, transparent)'},
			{
				name: '--interactive-muted',
				value: `color-mix(
in oklab,
hsl(228, calc(10% * var(--saturation-factor)), 35%) 100%,
hsl(245, calc(100% * var(--saturation-factor)), 80%) 40%
)`,
			},
			{
				name: '--interactive-active',
				value: `color-mix(
in oklab,
hsl(0, calc(0% * var(--saturation-factor)), 100%) 100%,
hsl(245, calc(100% * var(--saturation-factor)), 80%) 40%
)`,
			},
			{name: '--button-primary-fill', hue: 139, saturation: 55, useSaturationFactor: true, lightness: 44},
			{name: '--button-primary-active-fill', hue: 136, saturation: 60, useSaturationFactor: true, lightness: 38},
			{name: '--button-primary-text', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-secondary-fill', hue: 0, saturation: 0, lightness: 100, alpha: 0.1, useSaturationFactor: false},
			{
				name: '--button-secondary-active-fill',
				hue: 0,
				saturation: 0,
				lightness: 100,
				alpha: 0.15,
				useSaturationFactor: false,
			},
			{name: '--button-secondary-text', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-secondary-active-text', value: 'var(--button-secondary-text)'},
			{name: '--button-danger-fill', hue: 359, saturation: 70, useSaturationFactor: true, lightness: 54},
			{name: '--button-danger-active-fill', hue: 359, saturation: 65, useSaturationFactor: true, lightness: 45},
			{name: '--button-danger-text', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-danger-outline-border', value: '1px solid hsl(359, calc(70% * var(--saturation-factor)), 54%)'},
			{name: '--button-danger-outline-text', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-danger-outline-active-fill', hue: 359, saturation: 65, useSaturationFactor: true, lightness: 48},
			{name: '--button-danger-outline-active-border', value: 'transparent'},
			{name: '--button-ghost-text', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-inverted-fill', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-inverted-text', hue: 0, saturation: 0, lightness: 0},
			{name: '--button-outline-border', value: '1px solid hsla(0, 0%, 100%, 0.3)'},
			{name: '--button-outline-text', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-outline-active-fill', value: 'hsla(0, 0%, 100%, 0.15)'},
			{name: '--button-outline-active-border', value: '1px solid hsla(0, 0%, 100%, 0.4)'},
			{name: '--theme-border', value: 'transparent'},
			{name: '--theme-border-width', value: '0px'},
			{name: '--bg-primary', value: 'var(--background-primary)'},
			{name: '--bg-secondary', value: 'var(--background-secondary)'},
			{name: '--bg-tertiary', value: 'var(--background-tertiary)'},
			{name: '--bg-hover', value: 'var(--background-modifier-hover)'},
			{name: '--bg-active', value: 'var(--background-modifier-selected)'},
			{name: '--bg-blockquote', value: 'var(--background-secondary-alt)'},
			{name: '--bg-table-header', value: 'var(--background-tertiary)'},
			{name: '--bg-table-row-odd', value: 'var(--background-primary)'},
			{name: '--bg-table-row-even', value: 'var(--background-secondary)'},
			{name: '--border-color', family: 'neutralDark', lightness: 50, alpha: 0.2},
			{name: '--border-color-hover', family: 'neutralDark', lightness: 50, alpha: 0.3},
			{name: '--border-color-focus', hue: 198, saturation: 92, useSaturationFactor: true, lightness: 70, alpha: 0.45},
			{name: '--accent-primary', value: 'var(--brand-primary)'},
			{name: '--accent-success', value: 'var(--status-online)'},
			{name: '--accent-warning', value: 'var(--status-idle)'},
			{name: '--accent-danger', value: 'var(--status-dnd)'},
			{name: '--accent-info', value: 'var(--text-link)'},
			{name: '--accent-purple', family: 'accentPurple', lightness: 65},
			{name: '--alert-note-color', family: 'link', lightness: 70},
			{name: '--alert-tip-color', family: 'statusOnline', lightness: 45},
			{name: '--alert-important-color', family: 'accentPurple', lightness: 65},
			{name: '--alert-warning-color', family: 'statusIdle', lightness: 55},
			{name: '--alert-caution-color', hue: 359, saturation: 75, useSaturationFactor: true, lightness: 60},
			{name: '--shadow-sm', value: '0 1px 2px rgba(0, 0, 0, 0.1)'},
			{name: '--shadow-md', value: '0 2px 4px rgba(0, 0, 0, 0.15), 0 1px 2px rgba(0, 0, 0, 0.1)'},
			{name: '--shadow-lg', value: '0 4px 8px rgba(0, 0, 0, 0.15), 0 2px 4px rgba(0, 0, 0, 0.1)'},
			{name: '--shadow-xl', value: '0 10px 20px rgba(0, 0, 0, 0.15), 0 4px 8px rgba(0, 0, 0, 0.1)'},
			{name: '--transition-fast', value: '100ms ease'},
			{name: '--transition-normal', value: '200ms ease'},
			{name: '--transition-slow', value: '300ms ease'},
			{name: '--spoiler-overlay-color', value: 'rgba(0, 0, 0, 0.2)'},
			{name: '--spoiler-overlay-hover-color', value: 'rgba(0, 0, 0, 0.3)'},
			{name: '--scrollbar-thumb-bg', value: 'rgba(121, 122, 124, 0.4)'},
			{name: '--scrollbar-thumb-bg-hover', value: 'rgba(121, 122, 124, 0.7)'},
			{name: '--scrollbar-track-bg', value: 'transparent'},
			{
				name: '--user-area-divider-color',
				value: 'color-mix(in srgb, var(--background-modifier-hover) 70%, transparent)',
			},
		],
		light: [
			{scale: 'lightSurface'},
			{scale: 'lightText'},
			{name: '--panel-control-bg', value: 'color-mix(in srgb, var(--background-secondary) 65%, hsl(0, 0%, 100%) 35%)'},
			{name: '--panel-control-border', family: 'neutralLight', saturation: 25, lightness: 45, alpha: 0.25},
			{name: '--panel-control-divider', family: 'neutralLight', saturation: 30, lightness: 35, alpha: 0.2},
			{name: '--panel-control-highlight', value: 'hsla(0, 0%, 100%, 0.65)'},
			{name: '--background-modifier-hover', family: 'neutralLight', saturation: 10, lightness: 10, alpha: 0.05},
			{name: '--background-modifier-selected', family: 'neutralLight', saturation: 10, lightness: 10, alpha: 0.1},
			{name: '--background-modifier-accent', family: 'neutralLight', saturation: 10, lightness: 40, alpha: 0.22},
			{name: '--background-modifier-accent-focus', family: 'neutralLight', saturation: 10, lightness: 40, alpha: 0.32},
			{name: '--control-button-normal-bg', value: 'transparent'},
			{name: '--control-button-normal-text', family: 'neutralLight', lightness: 50},
			{name: '--control-button-hover-bg', family: 'neutralLight', lightness: 88},
			{name: '--control-button-hover-text', family: 'neutralLight', lightness: 20},
			{name: '--control-button-active-bg', family: 'neutralLight', lightness: 85},
			{name: '--control-button-active-text', family: 'neutralLight', lightness: 15},
			{name: '--control-button-danger-text', hue: 359, saturation: 70, useSaturationFactor: true, lightness: 50},
			{name: '--control-button-danger-hover-bg', hue: 359, saturation: 70, useSaturationFactor: true, lightness: 95},
			{name: '--text-link', family: 'link', lightness: 45},
			...LIGHT_CODE_TOKENS,
			{name: '--text-selection', hue: 210, saturation: 90, useSaturationFactor: true, lightness: 50, alpha: 0.2},
			{name: '--markup-mention-border', family: 'link', lightness: 45, alpha: 0.4},
			{name: '--markup-jump-link-fill', value: 'color-mix(in srgb, var(--text-link) 8%, transparent)'},
			{name: '--markup-everyone-text', hue: 250, saturation: 70, useSaturationFactor: true, lightness: 45},
			{
				name: '--markup-everyone-fill',
				value: 'color-mix(in srgb, hsl(250, calc(70% * var(--saturation-factor)), 45%) 12%, transparent)',
			},
			{
				name: '--markup-everyone-border',
				hue: 250,
				saturation: 70,
				useSaturationFactor: true,
				lightness: 45,
				alpha: 0.4,
			},
			{name: '--markup-here-text', hue: 40, saturation: 85, useSaturationFactor: true, lightness: 40},
			{
				name: '--markup-here-fill',
				value: 'color-mix(in srgb, hsl(40, calc(85% * var(--saturation-factor)), 40%) 12%, transparent)',
			},
			{name: '--markup-here-border', hue: 40, saturation: 85, useSaturationFactor: true, lightness: 40, alpha: 0.4},
			{name: '--status-online', family: 'statusOnline', saturation: 70, lightness: 40},
			{name: '--status-idle', family: 'statusIdle', saturation: 90, lightness: 45},
			{name: '--status-dnd', hue: 359, saturation: 70, useSaturationFactor: true, lightness: 50},
			{name: '--status-offline', family: 'statusOffline', hue: 210, saturation: 10, lightness: 55},
			{name: '--plutonium', value: 'var(--brand-primary)'},
			{name: '--plutonium-hover', value: 'var(--brand-secondary)'},
			{name: '--plutonium-text', value: 'var(--text-on-brand-primary)'},
			{name: '--plutonium-icon', family: 'brandIcon', lightness: 45},
			{name: '--invite-verified-icon-color', value: 'var(--brand-primary)'},
			{name: '--border-color', family: 'neutralLight', lightness: 40, alpha: 0.15},
			{name: '--border-color-hover', family: 'neutralLight', lightness: 40, alpha: 0.25},
			{name: '--border-color-focus', hue: 210, saturation: 90, useSaturationFactor: true, lightness: 50, alpha: 0.4},
			{name: '--bg-primary', value: 'var(--background-primary)'},
			{name: '--bg-secondary', value: 'var(--background-secondary)'},
			{name: '--bg-tertiary', value: 'var(--background-tertiary)'},
			{name: '--bg-hover', value: 'var(--background-modifier-hover)'},
			{name: '--bg-active', value: 'var(--background-modifier-selected)'},
			{name: '--bg-blockquote', value: 'var(--background-secondary-alt)'},
			{name: '--bg-table-header', value: 'var(--background-tertiary)'},
			{name: '--bg-table-row-odd', value: 'var(--background-primary)'},
			{name: '--bg-table-row-even', value: 'var(--background-secondary)'},
			{name: '--alert-note-color', family: 'link', lightness: 45},
			{name: '--alert-tip-color', hue: 150, saturation: 80, useSaturationFactor: true, lightness: 35},
			{name: '--alert-important-color', family: 'accentPurple', lightness: 50},
			{name: '--alert-warning-color', family: 'statusIdle', saturation: 90, lightness: 45},
			{name: '--alert-caution-color', hue: 358, saturation: 80, useSaturationFactor: true, lightness: 50},
			{name: '--spoiler-overlay-color', value: 'rgba(0, 0, 0, 0.1)'},
			{name: '--spoiler-overlay-hover-color', value: 'rgba(0, 0, 0, 0.15)'},
			{name: '--button-secondary-fill', family: 'neutralLight', saturation: 10, lightness: 10, alpha: 0.1},
			{name: '--button-secondary-active-fill', family: 'neutralLight', saturation: 10, lightness: 10, alpha: 0.15},
			{name: '--button-secondary-text', family: 'neutralLight', lightness: 15},
			{name: '--button-secondary-active-text', family: 'neutralLight', lightness: 10},
			{name: '--button-ghost-text', family: 'neutralLight', lightness: 20},
			{name: '--button-inverted-fill', hue: 0, saturation: 0, lightness: 100},
			{name: '--button-inverted-text', hue: 0, saturation: 0, lightness: 10},
			{name: '--button-outline-border', value: '1px solid hsla(220, calc(10% * var(--saturation-factor)), 40%, 0.3)'},
			{name: '--button-outline-text', family: 'neutralLight', lightness: 20},
			{name: '--button-outline-active-fill', family: 'neutralLight', saturation: 10, lightness: 10, alpha: 0.1},
			{
				name: '--button-outline-active-border',
				value: '1px solid hsla(220, calc(10% * var(--saturation-factor)), 40%, 0.5)',
			},
			{name: '--button-danger-outline-border', value: '1px solid hsl(359, calc(70% * var(--saturation-factor)), 50%)'},
			{name: '--button-danger-outline-text', hue: 359, saturation: 70, useSaturationFactor: true, lightness: 45},
			{name: '--button-danger-outline-active-fill', hue: 359, saturation: 70, useSaturationFactor: true, lightness: 50},
			{name: '--user-area-divider-color', family: 'neutralLight', lightness: 40, alpha: 0.2},
		],
		coal: [
			{scale: 'coalSurface'},
			{name: '--background-secondary', value: 'var(--background-primary)'},
			{name: '--background-secondary-lighter', value: 'var(--background-primary)'},
			{
				name: '--panel-control-bg',
				value: `color-mix(
in srgb,
var(--background-primary) 90%,
hsl(258, calc(10% * var(--saturation-factor)), 0%) 10%
)`,
			},
			{name: '--panel-control-border', family: 'neutralDark', saturation: 20, lightness: 30, alpha: 0.35},
			{name: '--panel-control-divider', family: 'neutralDark', saturation: 20, lightness: 25, alpha: 0.28},
			{name: '--panel-control-highlight', value: 'hsla(0, 0%, 100%, 0.06)'},
			{name: '--background-modifier-hover', family: 'neutralDark', lightness: 100, alpha: 0.04},
			{name: '--background-modifier-selected', family: 'neutralDark', lightness: 100, alpha: 0.08},
			{name: '--background-modifier-accent', family: 'neutralDark', saturation: 10, lightness: 65, alpha: 0.18},
			{name: '--background-modifier-accent-focus', family: 'neutralDark', saturation: 10, lightness: 70, alpha: 0.26},
			{name: '--control-button-normal-bg', value: 'transparent'},
			{name: '--control-button-normal-text', value: 'var(--text-primary-muted)'},
			{name: '--control-button-hover-bg', family: 'neutralDark', lightness: 12},
			{name: '--control-button-hover-text', value: 'var(--text-primary)'},
			{name: '--control-button-active-bg', family: 'neutralDark', lightness: 14},
			{name: '--control-button-active-text', value: 'var(--text-primary)'},
			{name: '--scrollbar-thumb-bg', value: 'rgba(160, 160, 160, 0.35)'},
			{name: '--scrollbar-thumb-bg-hover', value: 'rgba(200, 200, 200, 0.55)'},
			{name: '--scrollbar-track-bg', value: 'rgba(0, 0, 0, 0.45)'},
			{name: '--spoiler-overlay-color', value: 'hsla(0, 0%, 100%, 0.08)'},
			{name: '--spoiler-overlay-hover-color', value: 'hsla(0, 0%, 100%, 0.14)'},
			{name: '--bg-primary', value: 'var(--background-primary)'},
			{name: '--bg-secondary', value: 'var(--background-secondary)'},
			{name: '--bg-tertiary', value: 'var(--background-tertiary)'},
			{name: '--bg-hover', value: 'var(--background-modifier-hover)'},
			{name: '--bg-active', value: 'var(--background-modifier-selected)'},
			...DARK_CODE_TOKENS,
			{name: '--code-inline-bg', value: 'color-mix(in srgb, var(--background-tertiary) 86%, var(--text-code) 14%)'},
			{name: '--bg-code', value: 'var(--code-inline-bg)'},
			{name: '--code-block-bg', value: 'color-mix(in srgb, var(--background-secondary-alt) 84%, hsl(0, 0%, 0%) 16%)'},
			{name: '--bg-code-block', value: 'var(--code-block-bg)'},
			{name: '--code-block-border', value: 'color-mix(in srgb, var(--border-color) 84%, var(--text-code) 16%)'},
			{name: '--bg-blockquote', value: 'var(--background-secondary)'},
			{name: '--bg-table-header', value: 'var(--background-tertiary)'},
			{name: '--bg-table-row-odd', value: 'var(--background-primary)'},
			{name: '--bg-table-row-even', value: 'var(--background-secondary)'},
			{name: '--button-secondary-fill', value: 'hsla(0, 0%, 100%, 0.04)'},
			{name: '--button-secondary-active-fill', value: 'hsla(0, 0%, 100%, 0.07)'},
			{name: '--button-secondary-text', value: 'var(--text-primary)'},
			{name: '--button-secondary-active-text', value: 'var(--text-primary)'},
			{name: '--button-outline-border', value: '1px solid hsla(0, 0%, 100%, 0.08)'},
			{name: '--button-outline-active-fill', value: 'hsla(0, 0%, 100%, 0.12)'},
			{name: '--button-outline-active-border', value: '1px solid hsla(0, 0%, 100%, 0.16)'},
			{
				name: '--user-area-divider-color',
				value: 'color-mix(in srgb, var(--background-modifier-hover) 80%, transparent)',
			},
		],
		darkLegacy: [
			{scale: 'legacyDarkSurface'},
			{scale: 'legacyDarkText'},
			{
				name: '--panel-control-bg',
				value: `color-mix(
in srgb,
var(--background-secondary-alt) 90%,
hsl(220, calc(13% * var(--saturation-factor)), 2%) 10%
)`,
			},
			{name: '--panel-control-border', family: 'legacyDark', saturation: 30, lightness: 65, alpha: 0.45},
			{name: '--panel-control-divider', family: 'legacyDark', saturation: 30, lightness: 55, alpha: 0.35},
			{name: '--background-modifier-hover', family: 'legacyDark', lightness: 100, alpha: 0.05},
			{name: '--background-modifier-selected', family: 'legacyDark', lightness: 100, alpha: 0.1},
			{name: '--background-modifier-accent', family: 'legacyDark', saturation: 13, lightness: 80, alpha: 0.15},
			{name: '--background-modifier-accent-focus', family: 'legacyDark', saturation: 13, lightness: 80, alpha: 0.22},
			{name: '--control-button-hover-bg', family: 'legacyDark', lightness: 22},
			{name: '--control-button-active-bg', family: 'legacyDark', lightness: 24},
			{name: '--status-online', family: 'legacyStatusOnline', lightness: 40},
			{name: '--text-link', family: 'legacyLink', lightness: 70},
			{name: '--text-selection', hue: 210, saturation: 90, useSaturationFactor: true, lightness: 70, alpha: 0.35},
			{name: '--markup-mention-border', family: 'legacyLink', lightness: 70, alpha: 0.3},
			...DARK_CODE_TOKENS,
			{
				name: '--code-inline-bg',
				value: 'color-mix(in srgb, var(--background-secondary-alt) 82%, var(--text-code) 18%)',
			},
			{name: '--bg-code', value: 'var(--code-inline-bg)'},
			{
				name: '--code-block-bg',
				value: 'color-mix(in srgb, var(--background-secondary-alt) 88%, var(--background-primary) 12%)',
			},
			{name: '--bg-code-block', value: 'var(--code-block-bg)'},
			{name: '--border-color', family: 'legacyDark', lightness: 50, alpha: 0.2},
			{name: '--border-color-hover', family: 'legacyDark', lightness: 50, alpha: 0.3},
			{name: '--border-color-focus', hue: 210, saturation: 90, useSaturationFactor: true, lightness: 70, alpha: 0.45},
			{name: '--accent-purple', family: 'legacyAccentPurple', lightness: 65},
			{name: '--alert-note-color', family: 'legacyLink', lightness: 70},
			{name: '--alert-tip-color', family: 'legacyStatusOnline', lightness: 45},
			{name: '--alert-important-color', family: 'legacyAccentPurple', lightness: 65},
		],
	},
};

interface OutputToken {
	type: 'tone' | 'literal';
	name: string;
	family?: string;
	hue?: number;
	saturation?: number;
	lightness?: number;
	alpha?: number;
	useSaturationFactor?: boolean;
	value?: string;
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

function buildScaleTokens(scale: Scale): Array<OutputToken> {
	const lastIndex = Math.max(scale.stops.length - 1, 1);
	const tokens: Array<OutputToken> = [];
	for (let i = 0; i < scale.stops.length; i++) {
		const stop = scale.stops[i];
		let pos: number;
		if (stop.position !== undefined) {
			pos = clamp01(stop.position);
		} else {
			pos = i / lastIndex;
		}
		const eased = applyCurve(scale.curve, pos);
		let lightness = scale.range[0] + (scale.range[1] - scale.range[0]) * eased;
		lightness = Math.round(lightness * 1000) / 1000;
		tokens.push({
			type: 'tone',
			name: stop.name,
			family: scale.family,
			lightness,
		});
	}
	return tokens;
}

function expandTokens(defs: Array<TokenDef>, scales: Record<string, Scale>): Array<OutputToken> {
	const tokens: Array<OutputToken> = [];
	for (const def of defs) {
		if (def.scale) {
			const scale = scales[def.scale];
			if (!scale) {
				console.warn(`Warning: unknown scale "${def.scale}"`);
				continue;
			}
			tokens.push(...buildScaleTokens(scale));
			continue;
		}
		if (def.value !== undefined) {
			tokens.push({
				type: 'literal',
				name: def.name!,
				value: def.value.trim(),
			});
		} else {
			tokens.push({
				type: 'tone',
				name: def.name!,
				family: def.family,
				hue: def.hue,
				saturation: def.saturation,
				lightness: def.lightness,
				alpha: def.alpha,
				useSaturationFactor: def.useSaturationFactor,
			});
		}
	}
	return tokens;
}

function formatNumber(value: number): string {
	if (value === Math.floor(value)) {
		return String(Math.floor(value));
	}
	let s = value.toFixed(2);
	s = s.replace(/\.?0+$/, '');
	return s;
}

function formatTone(token: OutputToken, families: Record<string, ColorFamily>): string {
	const family = token.family ? families[token.family] : undefined;
	let hue = 0;
	let saturation = 0;
	let lightness = 0;
	let useFactor = false;
	if (token.hue !== undefined) {
		hue = token.hue;
	} else if (family) {
		hue = family.hue;
	}
	if (token.saturation !== undefined) {
		saturation = token.saturation;
	} else if (family) {
		saturation = family.saturation;
	}
	if (token.lightness !== undefined) {
		lightness = token.lightness;
	}
	if (token.useSaturationFactor !== undefined) {
		useFactor = token.useSaturationFactor;
	} else if (family) {
		useFactor = family.useSaturationFactor;
	}
	let satStr: string;
	if (useFactor) {
		satStr = `calc(${formatNumber(saturation)}% * var(--saturation-factor))`;
	} else {
		satStr = `${formatNumber(saturation)}%`;
	}
	if (token.alpha === undefined) {
		return `hsl(${formatNumber(hue)}, ${satStr}, ${formatNumber(lightness)}%)`;
	}
	return `hsla(${formatNumber(hue)}, ${satStr}, ${formatNumber(lightness)}%, ${formatNumber(token.alpha)})`;
}

function formatValue(token: OutputToken, families: Record<string, ColorFamily>): string {
	if (token.type === 'tone') {
		return formatTone(token, families);
	}
	return token.value!.trim();
}

function renderBlock(selector: string, tokens: Array<OutputToken>, families: Record<string, ColorFamily>): string {
	const lines: Array<string> = [];
	for (const token of tokens) {
		lines.push(`\t${token.name}: ${formatValue(token, families)};`);
	}
	return `${selector} {\n${lines.join('\n')}\n}`;
}

function generateCSS(
	cfg: Config,
	rootTokens: Array<OutputToken>,
	lightTokens: Array<OutputToken>,
	coalTokens: Array<OutputToken>,
	darkLegacyTokens: Array<OutputToken>,
): string {
	const blocks = [
		renderBlock(':root', rootTokens, cfg.families),
		renderBlock('.theme-light', lightTokens, cfg.families),
		renderBlock('.theme-coal', coalTokens, cfg.families),
		renderBlock('.theme-dark_legacy', darkLegacyTokens, cfg.families),
	];
	return `/* SPDX-License-Identifier: AGPL-3.0-or-later */\n\n${blocks.join('\n\n')}\n`;
}

function main() {
	const scriptDir = import.meta.dirname;
	const appDir = join(scriptDir, '..');
	const rootTokens = expandTokens(CONFIG.tokens.root, CONFIG.scales);
	const lightTokens = expandTokens(CONFIG.tokens.light, CONFIG.scales);
	const coalTokens = expandTokens(CONFIG.tokens.coal, CONFIG.scales);
	const darkLegacyTokens = expandTokens(CONFIG.tokens.darkLegacy, CONFIG.scales);
	const cssPath = join(appDir, 'src', 'features', 'theme', 'styles', 'generated', 'color-system.css');
	mkdirSync(dirname(cssPath), {recursive: true});
	const css = generateCSS(CONFIG, rootTokens, lightTokens, coalTokens, darkLegacyTokens);
	writeFileSync(cssPath, css);
	const relCSS = relative(appDir, cssPath);
	console.log(`Wrote ${relCSS}`);
}

main();
