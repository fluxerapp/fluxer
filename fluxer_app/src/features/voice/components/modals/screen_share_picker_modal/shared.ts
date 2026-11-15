// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import type {DesktopSource} from '@app/types/electron.d';
import type {AppWindowIcon} from '@phosphor-icons/react';

export const logger = new Logger('ScreenSharePickerModal');

export type ScreenSharePickerTab = 'apps' | 'displays' | 'devices';

export const DESKTOP_SOURCE_PRELOAD_TTL_MS = 2_500;
export const DESKTOP_SOURCE_LIST_POLL_INTERVAL_MS = 1_000;
export const THUMBNAIL_REFRESH_DEBOUNCE_MS = 750;
export const NATIVE_DISPLAY_SELECTION_ID = '__native_display__';

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

function desktopSourceHasThumbnail(source: DesktopSource): boolean {
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
