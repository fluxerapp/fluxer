// SPDX-License-Identifier: AGPL-3.0-or-later

import {useSaveData} from '@app/features/app/hooks/useSaveData';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {buildViewerMediaURL, isViewerImageItem} from '@app/features/messaging/utils/MediaViewerItemUtils';
import type {MediaViewerItem} from '@app/features/ui/state/MediaViewer';
import {useCallback, useEffect, useRef} from 'react';

export const MEDIA_VIEWER_HOVER_WARM_DELAY_MS = 220;

export interface MediaViewerWarmGates {
	saveData: boolean;
	allowAnimated: boolean;
}

export function resolveViewerWarmURL(item: MediaViewerItem, gates: MediaViewerWarmGates): string | null {
	if (gates.saveData) {
		return null;
	}
	if (item.src.length === 0 || item.src.startsWith('blob:')) {
		return null;
	}
	if (!isViewerImageItem(item)) {
		return null;
	}
	if ((item.animated === true || item.type === 'gif') && !gates.allowAnimated) {
		return null;
	}
	const url = buildViewerMediaURL(item);
	if (url.length === 0 || ImageCacheUtils.hasImage(url)) {
		return null;
	}
	return url;
}

export interface UseMediaViewerHoverWarmOptions {
	allowAnimated: boolean;
	enabled?: boolean;
}

export function useMediaViewerHoverWarm(
	item: MediaViewerItem | null | undefined,
	{allowAnimated, enabled = true}: UseMediaViewerHoverWarmOptions,
): {
	scheduleViewerWarm: () => void;
	cancelViewerWarm: () => void;
} {
	const saveData = useSaveData();
	const timerRef = useRef<number | null>(null);
	const cancelViewerWarm = useCallback(() => {
		if (timerRef.current == null || typeof window === 'undefined') {
			timerRef.current = null;
			return;
		}
		window.clearTimeout(timerRef.current);
		timerRef.current = null;
	}, []);
	const warmViewerNow = useCallback(() => {
		cancelViewerWarm();
		if (item == null) {
			return;
		}
		const url = resolveViewerWarmURL(item, {saveData, allowAnimated});
		if (url == null) {
			return;
		}
		ImageCacheUtils.warmImage(url);
	}, [allowAnimated, cancelViewerWarm, item, saveData]);
	const scheduleViewerWarm = useCallback(() => {
		if (!enabled || item == null || typeof window === 'undefined') {
			return;
		}
		cancelViewerWarm();
		timerRef.current = window.setTimeout(warmViewerNow, MEDIA_VIEWER_HOVER_WARM_DELAY_MS);
	}, [cancelViewerWarm, enabled, item, warmViewerNow]);
	useEffect(() => cancelViewerWarm, [allowAnimated, cancelViewerWarm, enabled, item?.src, saveData]);
	return {scheduleViewerWarm, cancelViewerWarm};
}
