// SPDX-License-Identifier: AGPL-3.0-or-later

import {NearViewportSurfaceContext, resolveObserverRoot} from '@app/features/messaging/hooks/useNearViewport';
import {trackViewportPlayback} from '@app/features/platform/utils/ViewportPlaybackRegistry';
import {useContext, useEffect, useState} from 'react';

export function useViewportPlayback(element: Element | null): boolean {
	const resolveScrollSurface = useContext(NearViewportSurfaceContext);
	const [isPlaybackVisible, setIsPlaybackVisible] = useState(() => typeof IntersectionObserver === 'undefined');
	useEffect(() => {
		if (typeof IntersectionObserver === 'undefined') {
			setIsPlaybackVisible(true);
			return;
		}
		if (element == null) {
			setIsPlaybackVisible(false);
			return;
		}
		const untrack = trackViewportPlayback(
			element,
			setIsPlaybackVisible,
			resolveObserverRoot(resolveScrollSurface, element),
		);
		return () => {
			untrack();
			setIsPlaybackVisible(false);
		};
	}, [element, resolveScrollSurface]);
	return isPlaybackVisible;
}
