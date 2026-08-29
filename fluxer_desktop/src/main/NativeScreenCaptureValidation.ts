// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NativeScreenCaptureSourceKind, NativeScreenCaptureStartOptions} from '@electron/common/Types';

const MIN_SCREEN_DIM = 16;
const MAX_SCREEN_DIM = 8192;
const MIN_SCREEN_FRAME_RATE = 1;
const MAX_SCREEN_FRAME_RATE = 240;
const MAX_CAPTURE_ID_LEN = 128;
const MIN_CAPTURE_RECT_DIM = 1;

function isPlainObject(value: unknown): value is Record<string, unknown> {
	if (!value || typeof value !== 'object' || Array.isArray(value)) return false;
	const proto = Object.getPrototypeOf(value);
	return proto === Object.prototype || proto === null;
}

function isValidSourceKind(value: unknown): value is NativeScreenCaptureSourceKind {
	return value === 'screen' || value === 'window' || value === 'game';
}

const MAX_SOURCE_ID_LEN = 256;

function getDesktopSourceKind(value: string): NativeScreenCaptureSourceKind | null {
	const match = /^(window|screen):(?:[0-9]+|0x[0-9a-fA-F]+):(?:0|1)$/.exec(value);
	if (!match) return null;
	return match[1] === 'window' ? 'window' : 'screen';
}

function isValidSourceId(value: unknown, sourceKind: NativeScreenCaptureSourceKind): value is string {
	if (typeof value !== 'string') return false;
	if (value.length === 0 || value.length > MAX_SOURCE_ID_LEN) return false;
	if (/^[0-9]+$/.test(value)) return true;
	const desktopSourceKind = getDesktopSourceKind(value);
	if (sourceKind === 'game') {
		return desktopSourceKind === 'screen' || desktopSourceKind === 'window';
	}
	return desktopSourceKind === sourceKind;
}

function isValidCaptureRect(value: unknown): boolean {
	if (!isPlainObject(value)) return false;
	for (const key of ['x', 'y', 'width', 'height'] as const) {
		const next = value[key];
		if (typeof next !== 'number' || !Number.isSafeInteger(next)) return false;
	}
	const rect = value as {x: number; y: number; width: number; height: number};
	return (
		rect.width >= MIN_CAPTURE_RECT_DIM &&
		rect.height >= MIN_CAPTURE_RECT_DIM &&
		rect.width <= MAX_SCREEN_DIM &&
		rect.height <= MAX_SCREEN_DIM &&
		Math.abs(rect.x) <= MAX_SCREEN_DIM &&
		Math.abs(rect.y) <= MAX_SCREEN_DIM
	);
}

export function isValidStartOptions(options: unknown): options is NativeScreenCaptureStartOptions {
	if (!isPlainObject(options)) return false;
	if (!isValidSourceKind(options.sourceKind)) return false;
	if (!isValidSourceId(options.sourceId, options.sourceKind)) return false;
	if (
		options.width !== undefined &&
		(typeof options.width !== 'number' ||
			!Number.isSafeInteger(options.width) ||
			options.width < 0 ||
			options.width > MAX_SCREEN_DIM)
	) {
		return false;
	}
	if (
		options.height !== undefined &&
		(typeof options.height !== 'number' ||
			!Number.isSafeInteger(options.height) ||
			options.height < 0 ||
			options.height > MAX_SCREEN_DIM)
	) {
		return false;
	}
	if (
		options.frameRate !== undefined &&
		(typeof options.frameRate !== 'number' ||
			!Number.isFinite(options.frameRate) ||
			options.frameRate < MIN_SCREEN_FRAME_RATE ||
			options.frameRate > MAX_SCREEN_FRAME_RATE)
	) {
		return false;
	}
	if (
		options.captureId !== undefined &&
		(typeof options.captureId !== 'string' ||
			options.captureId.trim().length === 0 ||
			options.captureId.length > MAX_CAPTURE_ID_LEN)
	) {
		return false;
	}
	if (options.colorRange !== undefined && options.colorRange !== 'full' && options.colorRange !== 'limited') {
		return false;
	}
	if (options.colorSpace !== undefined && options.colorSpace !== 'rec709' && options.colorSpace !== 'srgb') {
		return false;
	}
	if (options.showCursorClicks !== undefined && typeof options.showCursorClicks !== 'boolean') {
		return false;
	}
	if (options.captureRect !== undefined && !isValidCaptureRect(options.captureRect)) {
		return false;
	}
	if (options.nativeFrameSinkRequired !== true) {
		return false;
	}
	return true;
}

export function normalizeScreenCaptureDimension(value: number | undefined): number | undefined {
	if (value === undefined || value <= 0) return undefined;
	let normalized = Math.max(MIN_SCREEN_DIM, Math.min(MAX_SCREEN_DIM, value));
	if (normalized % 2 !== 0) {
		normalized = normalized < MAX_SCREEN_DIM ? normalized + 1 : normalized - 1;
	}
	return normalized;
}
