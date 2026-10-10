// SPDX-License-Identifier: AGPL-3.0-or-later

export const INSTANCE_BRAND_VARIABLES = [
	'--instance-brand-primary',
	'--instance-brand-secondary',
	'--instance-brand-primary-light',
	'--instance-brand-primary-fill',
	'--instance-text-on-brand-primary',
] as const;

type InstanceBrandVariable = (typeof INSTANCE_BRAND_VARIABLES)[number];

const HEX_COLOR_PATTERN = /^#(?:[0-9a-f]{3}|[0-9a-f]{6})$/i;

function parseHexColor(value: string): [number, number, number] | null {
	const trimmed = value.trim();
	if (!HEX_COLOR_PATTERN.test(trimmed)) return null;
	const digits =
		trimmed.length === 4
			? trimmed
					.slice(1)
					.split('')
					.map((digit) => digit + digit)
					.join('')
			: trimmed.slice(1);
	return [
		Number.parseInt(digits.slice(0, 2), 16),
		Number.parseInt(digits.slice(2, 4), 16),
		Number.parseInt(digits.slice(4, 6), 16),
	];
}

function rgbToHsl(red: number, green: number, blue: number): [number, number, number] {
	const r = red / 255;
	const g = green / 255;
	const b = blue / 255;
	const max = Math.max(r, g, b);
	const min = Math.min(r, g, b);
	const lightness = (max + min) / 2;
	const delta = max - min;
	if (delta === 0) return [0, 0, lightness * 100];
	const saturation = delta / (1 - Math.abs(2 * lightness - 1));
	let hue: number;
	if (max === r) {
		hue = ((g - b) / delta) % 6;
	} else if (max === g) {
		hue = (b - r) / delta + 2;
	} else {
		hue = (r - g) / delta + 4;
	}
	return [(hue * 60 + 360) % 360, saturation * 100, lightness * 100];
}

function relativeLuminance(red: number, green: number, blue: number): number {
	const linear = (channel: number) => {
		const value = channel / 255;
		return value <= 0.03928 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
	};
	return 0.2126 * linear(red) + 0.7152 * linear(green) + 0.0722 * linear(blue);
}

function formatNumber(value: number): string {
	return String(Math.round(value * 100) / 100);
}

function hsl(hue: number, saturation: number, lightness: number): string {
	return `hsl(${formatNumber(hue)}, calc(${formatNumber(saturation)}% * var(--saturation-factor)), ${formatNumber(lightness)}%)`;
}

export function getInstanceBrandVariables(themeColor: string): Record<InstanceBrandVariable, string> | null {
	const rgb = parseHexColor(themeColor);
	if (rgb === null) return null;
	const [hue, saturation, lightness] = rgbToHsl(...rgb);
	const luminance = relativeLuminance(...rgb);
	const prefersDarkText = (luminance + 0.05) / 0.05 > 1.05 / (luminance + 0.05);
	return {
		'--instance-brand-primary': hsl(hue, saturation, lightness),
		'--instance-brand-secondary': hsl(hue, saturation * (6 / 7), lightness * 0.89),
		'--instance-brand-primary-light': hsl(hue, saturation, lightness + (100 - lightness) * 0.65),
		'--instance-brand-primary-fill': prefersDarkText ? 'hsl(0, 0%, 8%)' : 'hsl(0, 0%, 100%)',
		'--instance-text-on-brand-primary': prefersDarkText ? 'hsl(0, 0%, 8%)' : 'hsl(0, 0%, 98%)',
	};
}
