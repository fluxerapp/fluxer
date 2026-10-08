// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ThemeLibraryTheme} from '@app/features/theme/state/ThemeLibrary';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';

function escapeRegExp(value: string): string {
	return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

export function extractThemeVariableOverrides(css: string): Record<string, string> {
	const overrides: Record<string, string> = {};
	const variablePattern = /--([a-zA-Z0-9_-]+)\s*:\s*([^;]+);/g;
	let match: RegExpExecArray | null;
	while ((match = variablePattern.exec(css)) !== null) {
		const variableName = `--${match[1] as string}`;
		const value = match[2] as string;
		overrides[variableName] = value.trim();
	}
	return overrides;
}

function removeEmptyRootBlocks(css: string): string {
	return css
		.replace(/:root\s*\{\s*\}/g, '')
		.replace(/\n{3,}/g, '\n\n')
		.trim();
}

export function updateCssForVariable(css: string, variableName: string, newValue: string | null): string {
	const variableNamePattern = escapeRegExp(variableName);
	const propertyPattern = new RegExp(`(--${variableNamePattern.replace(/^--/, '')}\\s*:[^;]*;)`);
	if (newValue === null) {
		return removeEmptyRootBlocks(css.replace(propertyPattern, ''));
	}
	if (propertyPattern.test(css)) {
		return css.replace(propertyPattern, `${variableName}: ${newValue};`);
	}
	const trimmedCss = removeEmptyRootBlocks(css);
	const prefix = trimmedCss.length > 0 && !trimmedCss.endsWith('\n') ? '\n' : '';
	return `${trimmedCss}${prefix}:root { ${variableName}: ${newValue}; }\n`;
}

function clampByte(value: number): number {
	return Math.max(0, Math.min(255, Math.round(value)));
}

function numberToHex(value: number): string {
	return `#${(value >>> 0).toString(16).padStart(6, '0').slice(-6)}`.toUpperCase();
}

export function stripCssColorPriority(color: string): string {
	return color
		.trim()
		.replace(/\s*!important\s*$/i, '')
		.trim();
}

function parseHexColor(color: string): number | null {
	const trimmed = color.trim();
	const hexMatch = /^#([0-9A-Fa-f]{3,4}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$/.exec(trimmed);
	if (!hexMatch) return null;
	const value = hexMatch[1] as string;
	if (value.length === 3 || value.length === 4) {
		const [red = '0', green = '0', blue = '0'] = value;
		return Number.parseInt(`${red}${red}${green}${green}${blue}${blue}`, 16) >>> 0;
	}
	return Number.parseInt(value.slice(0, 6), 16) >>> 0;
}

function parseCssNumberComponent(value: string, scale: number): number | null {
	const trimmed = value.trim();
	if (!trimmed) return null;
	if (trimmed.endsWith('%')) {
		const percentage = Number.parseFloat(trimmed.slice(0, -1));
		return Number.isFinite(percentage) ? (percentage / 100) * scale : null;
	}
	const number = Number.parseFloat(trimmed);
	return Number.isFinite(number) ? number : null;
}

function parseRgbColor(color: string): number | null {
	const match = /^rgba?\((.*)\)$/i.exec(color.trim());
	if (!match) return null;
	const body = (match[1] ?? '').split('/')[0]?.trim() ?? '';
	const parts = body.includes(',') ? body.split(',') : body.split(/\s+/);
	if (parts.length < 3) return null;
	const red = parseCssNumberComponent(parts[0] ?? '', 255);
	const green = parseCssNumberComponent(parts[1] ?? '', 255);
	const blue = parseCssNumberComponent(parts[2] ?? '', 255);
	if (red === null || green === null || blue === null) return null;
	return (clampByte(red) << 16) | (clampByte(green) << 8) | clampByte(blue);
}

function parseSrgbColor(color: string): number | null {
	const match = /^color\(\s*srgb\s+(.*)\)$/i.exec(color.trim());
	if (!match) return null;
	const body = (match[1] ?? '').split('/')[0]?.trim() ?? '';
	const parts = body.split(/\s+/);
	if (parts.length < 3) return null;
	const red = parseCssNumberComponent(parts[0] ?? '', 1);
	const green = parseCssNumberComponent(parts[1] ?? '', 1);
	const blue = parseCssNumberComponent(parts[2] ?? '', 1);
	if (red === null || green === null || blue === null) return null;
	return (clampByte(red * 255) << 16) | (clampByte(green * 255) << 8) | clampByte(blue * 255);
}

function parseNormalizedCssColor(color: string): number | null {
	return parseHexColor(color) ?? parseRgbColor(color) ?? parseSrgbColor(color);
}

function resolveCssColorWithElement(color: string): string | null {
	if (typeof document === 'undefined' || typeof window === 'undefined') return null;
	if (typeof window.getComputedStyle !== 'function') return null;
	const container = document.body ?? document.documentElement;
	if (!container) return null;
	const probe = document.createElement('span');
	probe.style.position = 'absolute';
	probe.style.pointerEvents = 'none';
	probe.style.visibility = 'hidden';
	probe.style.backgroundColor = color;
	if (!probe.style.backgroundColor) return null;
	container.appendChild(probe);
	try {
		const resolved = window.getComputedStyle(probe).backgroundColor;
		return resolved.trim().length > 0 ? resolved : null;
	} finally {
		probe.remove();
	}
}

function resolveCssColorWithCanvas(color: string): string | null {
	if (typeof document === 'undefined') return null;
	const canvas = document.createElement('canvas');
	const context = canvas.getContext('2d');
	if (!context) return null;
	try {
		context.fillStyle = '#010203';
		context.fillStyle = color;
		const parsedFromFirstSentinel = String(context.fillStyle);
		context.fillStyle = '#040506';
		context.fillStyle = color;
		const parsedFromSecondSentinel = String(context.fillStyle);
		if (parsedFromFirstSentinel === '#010203' && parsedFromSecondSentinel === '#040506') {
			return null;
		}
		return parsedFromSecondSentinel;
	} catch {
		return null;
	}
}

function cssColorStringToNumber(color: string): number | null {
	const normalizedColor = stripCssColorPriority(color);
	const direct = parseNormalizedCssColor(normalizedColor);
	if (direct !== null) return direct;
	const resolved = resolveCssColorWithElement(normalizedColor);
	if (resolved !== null) {
		const parsed = parseNormalizedCssColor(resolved);
		if (parsed !== null) return parsed;
	}
	const canvasResolved = resolveCssColorWithCanvas(normalizedColor);
	return canvasResolved !== null ? parseNormalizedCssColor(canvasResolved) : null;
}

export function cssColorStringToHex(color: string): string | null {
	const parsed = cssColorStringToNumber(color);
	return parsed !== null ? numberToHex(parsed) : null;
}

export function downloadTextFile(text: string, fileName: string, mimeType: string): void {
	const blob = new Blob([text], {type: mimeType});
	const url = URL.createObjectURL(blob);
	const anchor = document.createElement('a');
	anchor.href = url;
	anchor.download = fileName;
	anchor.rel = 'noopener';
	document.body.appendChild(anchor);
	anchor.click();
	anchor.remove();
	URL.revokeObjectURL(url);
}

export function readFileText(file: File): Promise<string> {
	return new Promise((resolve, reject) => {
		const reader = new FileReader();
		reader.onerror = () => reject(reader.error ?? new Error(`Failed to read ${file.name}`));
		reader.onload = () => resolve(String(reader.result ?? ''));
		reader.readAsText(file);
	});
}

export async function copyText(text: string): Promise<void> {
	const electronApi = getElectronAPI();
	if (electronApi?.clipboardWriteText) {
		await electronApi.clipboardWriteText(text);
		return;
	}
	await navigator.clipboard.writeText(text);
}

export function parseTagInput(value: string): Array<string> {
	return value
		.split(',')
		.map((tag) => tag.trim())
		.filter(Boolean);
}

export function createThemeExportFileName(theme: ThemeLibraryTheme): string {
	return theme.fileName.toLowerCase().endsWith('.css') ? theme.fileName : `${theme.fileName}.css`;
}
