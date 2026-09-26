// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import type {DesktopSource, NativeScreenCaptureSource, NativeScreenCaptureSourceKind} from '@app/types/electron.d';
import type {AppWindowIcon} from '@phosphor-icons/react';

export const logger = new Logger('ScreenSharePickerModal');

export type ScreenSharePickerTab = 'apps' | 'displays' | 'devices';

export const DESKTOP_SOURCE_PRELOAD_TTL_MS = 2_500;
export const DESKTOP_SOURCE_LIST_POLL_INTERVAL_MS = 1_000;
export const THUMBNAIL_REFRESH_DEBOUNCE_MS = 750;
export const NATIVE_DISPLAY_SELECTION_ID = '__native_display__';
export const LINUX_GAME_CAPTURE_SELECTION_ID = '__linux_game_capture__';

export interface PickerCard {
	id: string;
	title: string;
	thumbnailSrc?: string;
	badgeSrc?: string;
	placeholderIcon: typeof AppWindowIcon;
}

export interface ScreenSharePickerModalProps {
	initialDesktopSources?: Array<DesktopSource>;
	initialDesktopSourcesError?: boolean;
	initialDesktopSourcesSkippedForPermission?: boolean;
	displayShareEnvironment: DisplayShareEnvironment;
	initialTab?: ScreenSharePickerTab;
	mode?: 'start' | 'switch';
}

export interface ScreenSharePickerPreload {
	desktopSources: Array<DesktopSource>;
	desktopSourcesError?: boolean;
	desktopSourcesSkippedForPermission?: boolean;
	displayShareEnvironment: DisplayShareEnvironment;
}

export function isUsableImageDataUrl(value?: string | null): value is string {
	if (!value) {
		return false;
	}
	const trimmedValue = value.trim();
	if (!trimmedValue.startsWith('data:image/')) {
		return false;
	}
	const base64MarkerIndex = trimmedValue.indexOf('base64,');
	return base64MarkerIndex >= 0 && trimmedValue.length > base64MarkerIndex + 'base64,'.length;
}

export function normaliseDesktopSource(source: DesktopSource): DesktopSource {
	return {
		...source,
		thumbnailDataUrl: isUsableImageDataUrl(source.thumbnailDataUrl) ? source.thumbnailDataUrl : undefined,
		appIconDataUrl: isUsableImageDataUrl(source.appIconDataUrl) ? source.appIconDataUrl : undefined,
	};
}

export function desktopSourceHasThumbnail(source: DesktopSource): boolean {
	return isUsableImageDataUrl(source.thumbnailDataUrl);
}

export function hasDesktopSourcesMissingThumbnails(
	sources: ReadonlyArray<DesktopSource>,
	predicate: (source: DesktopSource) => boolean,
): boolean {
	return sources.some((source) => predicate(source) && !desktopSourceHasThumbnail(source));
}

export function getDesktopSourceThumbnailStateKey(
	sources: ReadonlyArray<DesktopSource>,
	predicate: (source: DesktopSource) => boolean,
): string {
	return sources
		.filter(predicate)
		.map((source) => `${source.id}:${desktopSourceHasThumbnail(source) ? 'thumbnail' : 'missing'}`)
		.join('|');
}

export function mergeDesktopSources(previous: Array<DesktopSource>, next: Array<DesktopSource>): Array<DesktopSource> {
	const previousById = new Map(previous.map((source) => [source.id, source]));
	return next.map((source) => {
		const prior = previousById.get(source.id);
		if (!prior) return source;
		return {
			...source,
			thumbnailDataUrl: source.thumbnailDataUrl ?? prior.thumbnailDataUrl,
			appIconDataUrl: source.appIconDataUrl ?? prior.appIconDataUrl,
		};
	});
}

export function desktopSourceIdentitiesMatch(
	a: ReadonlyArray<DesktopSource>,
	b: ReadonlyArray<DesktopSource>,
): boolean {
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		if (a[i].id !== b[i].id || a[i].name !== b[i].name) return false;
	}
	return true;
}

export function isWindowSource(source: DesktopSource): boolean {
	return source.id.startsWith('window:');
}

export function isDisplaySource(source: DesktopSource): boolean {
	return source.id.startsWith('screen:');
}

function parseDesktopSourceId(sourceId: string): {kind: NativeScreenCaptureSourceKind; token: string} | null {
	const match = /^(window|screen):([^:]+):(?:0|1)$/.exec(sourceId);
	if (!match) return null;
	return {
		kind: match[1] === 'window' ? 'window' : 'screen',
		token: match[2],
	};
}

function parseNumericId(value: string): bigint | null {
	const trimmed = value.trim();
	if (/^[0-9]+$/.test(trimmed)) {
		return BigInt(trimmed);
	}
	if (/^0x[0-9a-fA-F]+$/.test(trimmed)) {
		return BigInt(trimmed);
	}
	return null;
}

function parseScreenOrdinal(value: string | undefined): number | null {
	if (!value) return null;
	const trimmed = value.trim();
	if (!/^[0-9]+$/.test(trimmed)) return null;
	const parsed = Number.parseInt(trimmed, 10);
	return Number.isSafeInteger(parsed) && parsed >= 0 ? parsed : null;
}

function parseNamedScreenOrdinal(value: string | undefined): number | null {
	const match = /\b(?:screen|display)\s+([0-9]+)\b/i.exec(value ?? '');
	if (!match) return null;
	const parsed = Number.parseInt(match[1], 10);
	return Number.isSafeInteger(parsed) && parsed > 0 ? parsed - 1 : null;
}

function nativeIdsMatch(a: string | undefined, b: string | undefined): boolean {
	if (!a || !b) return false;
	if (a === b) return true;
	const numericA = parseNumericId(a);
	const numericB = parseNumericId(b);
	return numericA != null && numericB != null && numericA === numericB;
}

function nativeKindsMatch(
	desktopKind: NativeScreenCaptureSourceKind,
	nativeKind: NativeScreenCaptureSourceKind,
): boolean {
	if (desktopKind === 'screen') {
		return nativeKind === 'screen';
	}
	return nativeKind === desktopKind;
}

function normalizeSourceName(value: string | undefined): string {
	return (value ?? '')
		.toLowerCase()
		.replace(/[\u2010-\u2015]/g, '-')
		.replace(/\s+/g, ' ')
		.trim();
}

function windowNamesMatch(desktopSource: DesktopSource, nativeSource: NativeScreenCaptureSource): boolean {
	const desktopName = normalizeSourceName(desktopSource.name);
	const nativeName = normalizeSourceName(nativeSource.name);
	const nativeAppName = normalizeSourceName(nativeSource.appName);
	if (!desktopName || !nativeName) return false;
	if (desktopName === nativeName || desktopName === nativeAppName) return true;
	if (nativeName.endsWith(` - ${desktopName}`) || nativeName.startsWith(`${desktopName} - `)) return true;
	if (nativeName.includes(` - ${desktopName} - `)) return true;
	return false;
}

function dimensionsMatch(desktopSource: DesktopSource, nativeSource: NativeScreenCaptureSource): boolean {
	const desktopWidth = desktopSource.nativeWidth;
	const desktopHeight = desktopSource.nativeHeight;
	if (!desktopWidth || !desktopHeight) return true;
	if (nativeSource.width <= 0 || nativeSource.height <= 0) return true;
	const widthDelta = Math.abs(nativeSource.width - desktopWidth);
	const heightDelta = Math.abs(nativeSource.height - desktopHeight);
	return widthDelta <= 2 && heightDelta <= 2;
}

function findUniqueNativeCandidate(
	nativeSources: ReadonlyArray<NativeScreenCaptureSource>,
	predicate: (nativeSource: NativeScreenCaptureSource) => boolean,
): NativeScreenCaptureSource | undefined {
	let match: NativeScreenCaptureSource | undefined;
	for (const nativeSource of nativeSources) {
		if (!predicate(nativeSource)) continue;
		if (match) return undefined;
		match = nativeSource;
	}
	return match;
}

export function findNativeCaptureSourceForDesktopSource(
	desktopSource: DesktopSource,
	nativeSources: ReadonlyArray<NativeScreenCaptureSource>,
): NativeScreenCaptureSource | undefined {
	const parsed = parseDesktopSourceId(desktopSource.id);
	if (!parsed) return undefined;
	const directMatch = nativeSources.find((nativeSource) => {
		if (!nativeKindsMatch(parsed.kind, nativeSource.kind)) return false;
		if (nativeSource.id === desktopSource.id) return true;
		if (parsed.kind === 'screen' && nativeIdsMatch(nativeSource.id, desktopSource.display_id)) {
			return true;
		}
		return nativeIdsMatch(nativeSource.id, parsed.token);
	});
	if (directMatch) return directMatch;
	if (parsed.kind === 'screen') {
		const nativeScreenSources = nativeSources.filter((nativeSource) => nativeKindsMatch('screen', nativeSource.kind));
		const ordinal = parseScreenOrdinal(parsed.token) ?? parseNamedScreenOrdinal(desktopSource.name);
		const ordinalMatch = ordinal == null ? undefined : nativeScreenSources[ordinal];
		if (ordinalMatch && dimensionsMatch(desktopSource, ordinalMatch)) {
			return ordinalMatch;
		}
	}
	return findUniqueNativeCandidate(nativeSources, (nativeSource) => {
		if (!nativeKindsMatch(parsed.kind, nativeSource.kind)) return false;
		if (!dimensionsMatch(desktopSource, nativeSource)) return false;
		if (parsed.kind === 'screen') {
			return true;
		}
		return windowNamesMatch(desktopSource, nativeSource);
	});
}
