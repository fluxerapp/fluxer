// SPDX-License-Identifier: AGPL-3.0-or-later

type DesktopSourceType = 'screen' | 'window';

const MAX_DISPLAY_MEDIA_REQUEST_ID_LENGTH = 128;
const MAX_DESKTOP_SOURCE_ID_LENGTH = 256;

export function normalizeDesktopSourceTypes(value: unknown): Array<DesktopSourceType> {
	if (!Array.isArray(value)) {
		return ['screen', 'window'];
	}
	const output: Array<DesktopSourceType> = [];
	for (const entry of value) {
		if (entry !== 'screen' && entry !== 'window') continue;
		if (!output.includes(entry)) output.push(entry);
	}
	return output.length > 0 ? output : ['screen', 'window'];
}

export function isValidDisplayMediaRequestId(value: unknown): value is string {
	return typeof value === 'string' && value.length > 0 && value.length <= MAX_DISPLAY_MEDIA_REQUEST_ID_LENGTH;
}

export function isValidDesktopSourceId(value: unknown): value is string {
	if (typeof value !== 'string' || value.length === 0 || value.length > MAX_DESKTOP_SOURCE_ID_LENGTH) {
		return false;
	}
	if (value.includes('\0') || /[\r\n]/.test(value)) {
		return false;
	}
	return value.length > 'screen:'.length && (value.startsWith('screen:') || value.startsWith('window:'));
}

export function isListOnlyDesktopSourcesOption(value: unknown): boolean {
	if (typeof value !== 'object' || value === null) return false;
	return (value as {listOnly?: unknown}).listOnly === true;
}
