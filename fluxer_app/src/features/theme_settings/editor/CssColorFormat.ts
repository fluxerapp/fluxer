// SPDX-License-Identifier: AGPL-3.0-or-later

export interface RgbaColor {
	r: number;
	g: number;
	b: number;
	a: number;
}

type CssColorFormat = 'hex' | 'rgb' | 'hsl' | 'hwb' | 'oklch' | 'oklab';

const LEGACY_RGB_PATTERN = /^rgba?\(\s*([\d.]+),\s*([\d.]+),\s*([\d.]+)(?:,\s*([\d.]+))?\s*\)$/;
const ALPHA_PATTERN = /\/\s*([\d.]+%?)\s*\)$/;

let probe: HTMLElement | null = null;
let canvasContext: CanvasRenderingContext2D | null = null;

function readComputedColor(value: string): string | null {
	if (!probe) {
		probe = document.createElement('span');
		probe.setAttribute('aria-hidden', 'true');
		probe.style.setProperty('display', 'none', 'important');
	}
	if (!probe.isConnected) document.documentElement.appendChild(probe);
	probe.style.removeProperty('color');
	probe.style.setProperty('color', value, 'important');
	if (probe.style.getPropertyValue('color') === '') return null;
	return getComputedStyle(probe).color;
}

function readCanvasColor(value: string): RgbaColor | null {
	if (!canvasContext) {
		const canvas = document.createElement('canvas');
		canvas.width = 1;
		canvas.height = 1;
		canvasContext = canvas.getContext('2d', {willReadFrequently: true});
	}
	if (!canvasContext) return null;
	canvasContext.clearRect(0, 0, 1, 1);
	canvasContext.fillStyle = '#000';
	canvasContext.fillStyle = value;
	canvasContext.fillRect(0, 0, 1, 1);
	const [r, g, b] = canvasContext.getImageData(0, 0, 1, 1).data;
	return {r, g, b, a: 1};
}

function parseAlpha(text: string): number {
	return text.endsWith('%') ? Number.parseFloat(text) / 100 : Number.parseFloat(text);
}

export function parseCssColor(value: string): RgbaColor | null {
	const computed = readComputedColor(value.trim());
	if (computed === null) return null;
	const legacy = LEGACY_RGB_PATTERN.exec(computed);
	if (legacy) {
		return {
			r: Number(legacy[1]),
			g: Number(legacy[2]),
			b: Number(legacy[3]),
			a: legacy[4] === undefined ? 1 : Number(legacy[4]),
		};
	}
	const alphaMatch = ALPHA_PATTERN.exec(computed);
	const opaque = alphaMatch ? computed.replace(ALPHA_PATTERN, ')') : computed;
	const color = readCanvasColor(opaque);
	if (!color) return null;
	return {...color, a: alphaMatch ? parseAlpha(alphaMatch[1]) : 1};
}

function detectFormat(original: string): CssColorFormat {
	const value = original.trim().toLowerCase();
	if (value.startsWith('#')) return 'hex';
	const name = /^([a-z]+)\(/.exec(value)?.[1];
	if (name === 'rgb' || name === 'rgba') return 'rgb';
	if (name === 'hsl' || name === 'hsla') return 'hsl';
	if (name === 'hwb') return 'hwb';
	if (name === 'oklch') return 'oklch';
	if (name === 'oklab') return 'oklab';
	return 'hex';
}

function round(value: number, digits: number): string {
	const factor = 10 ** digits;
	return String(Math.round(value * factor) / factor);
}

function toHex(channel: number): string {
	return Math.round(Math.min(255, Math.max(0, channel)))
		.toString(16)
		.padStart(2, '0');
}

function formatHex({r, g, b, a}: RgbaColor): string {
	const alpha = a < 1 ? toHex(a * 255) : '';
	return `#${toHex(r)}${toHex(g)}${toHex(b)}${alpha}`;
}

function rgbToHsl({r, g, b}: RgbaColor): [number, number, number] {
	const red = r / 255;
	const green = g / 255;
	const blue = b / 255;
	const max = Math.max(red, green, blue);
	const min = Math.min(red, green, blue);
	const lightness = (max + min) / 2;
	const delta = max - min;
	if (delta === 0) return [0, 0, lightness * 100];
	const saturation = delta / (1 - Math.abs(2 * lightness - 1));
	let hue: number;
	if (max === red) hue = ((green - blue) / delta) % 6;
	else if (max === green) hue = (blue - red) / delta + 2;
	else hue = (red - green) / delta + 4;
	return [(hue * 60 + 360) % 360, saturation * 100, lightness * 100];
}

function rgbToHwb(color: RgbaColor): [number, number, number] {
	const [hue] = rgbToHsl(color);
	const whiteness = Math.min(color.r, color.g, color.b) / 255;
	const blackness = 1 - Math.max(color.r, color.g, color.b) / 255;
	return [hue, whiteness * 100, blackness * 100];
}

function linearize(channel: number): number {
	const value = channel / 255;
	return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
}

function rgbToOklab({r, g, b}: RgbaColor): [number, number, number] {
	const red = linearize(r);
	const green = linearize(g);
	const blue = linearize(b);
	const l = Math.cbrt(0.4122214708 * red + 0.5363325363 * green + 0.0514459929 * blue);
	const m = Math.cbrt(0.2119034982 * red + 0.6806995451 * green + 0.1073969566 * blue);
	const s = Math.cbrt(0.0883024619 * red + 0.2817188376 * green + 0.6299787005 * blue);
	return [
		0.2104542553 * l + 0.793617785 * m - 0.0040720468 * s,
		1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s,
		0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s,
	];
}

function alphaSuffix(a: number, legacy: boolean): string {
	if (a >= 1) return '';
	return legacy ? `, ${round(a, 3)}` : ` / ${round(a * 100, 1)}%`;
}

export function formatCssColor(color: RgbaColor, original: string): string {
	const legacy = original.includes(',');
	switch (detectFormat(original)) {
		case 'rgb': {
			const channels = [color.r, color.g, color.b].map((channel) => String(Math.round(channel)));
			if (legacy) return `${color.a < 1 ? 'rgba' : 'rgb'}(${channels.join(', ')}${alphaSuffix(color.a, true)})`;
			return `rgb(${channels.join(' ')}${alphaSuffix(color.a, false)})`;
		}
		case 'hsl': {
			const [h, s, l] = rgbToHsl(color);
			if (legacy) {
				return `${color.a < 1 ? 'hsla' : 'hsl'}(${round(h, 1)}, ${round(s, 1)}%, ${round(l, 1)}%${alphaSuffix(color.a, true)})`;
			}
			return `hsl(${round(h, 1)} ${round(s, 1)}% ${round(l, 1)}%${alphaSuffix(color.a, false)})`;
		}
		case 'hwb': {
			const [h, w, b] = rgbToHwb(color);
			return `hwb(${round(h, 1)} ${round(w, 1)}% ${round(b, 1)}%${alphaSuffix(color.a, false)})`;
		}
		case 'oklch': {
			const [l, a, b] = rgbToOklab(color);
			const chroma = Math.hypot(a, b);
			const hue = chroma < 0.0001 ? 0 : (Math.atan2(b, a) * (180 / Math.PI) + 360) % 360;
			return `oklch(${round(l * 100, 1)}% ${round(chroma, 3)} ${round(hue, 1)}${alphaSuffix(color.a, false)})`;
		}
		case 'oklab': {
			const [l, a, b] = rgbToOklab(color);
			return `oklab(${round(l * 100, 1)}% ${round(a, 3)} ${round(b, 3)}${alphaSuffix(color.a, false)})`;
		}
		default:
			return formatHex(color);
	}
}

export function toHexColor(value: string): string | null {
	const color = parseCssColor(value);
	return color ? formatHex(color) : null;
}
