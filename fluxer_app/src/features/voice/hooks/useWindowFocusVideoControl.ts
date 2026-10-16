// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getAnimatedMediaPlaybackAllowed,
	subscribeAnimatedMediaPlaybackChange,
} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import type React from 'react';
import {useEffect, useRef} from 'react';

interface VideoPoolControl {
	pauseAll: () => void;
	resumeAll: () => void;
}

interface UseWindowFocusVideoControlOptions {
	scrollerRef: React.RefObject<ScrollerHandle | null>;
	videoPool: VideoPoolControl;
}

export function useWindowFocusVideoControl({scrollerRef, videoPool}: UseWindowFocusVideoControlOptions): void {
	const poolRef = useRef(videoPool);
	poolRef.current = videoPool;
	const scrollerRefRef = useRef(scrollerRef);
	scrollerRefRef.current = scrollerRef;
	useEffect(() => {
		const updatePlayback = () => {
			if (getAnimatedMediaPlaybackAllowed()) {
				poolRef.current.resumeAll();
			} else {
				poolRef.current.pauseAll();
			}
		};
		const handleBlur = () => {
			const node = scrollerRefRef.current.current?.getViewportElement();
			if (document.activeElement instanceof HTMLElement && node?.contains(document.activeElement)) {
				const scrollTop = node.scrollTop;
				document.activeElement.blur();
				node.scrollTop = scrollTop;
			}
			updatePlayback();
		};
		const unsubscribePlayback = subscribeAnimatedMediaPlaybackChange(updatePlayback);
		updatePlayback();
		window.addEventListener('blur', handleBlur);
		return () => {
			unsubscribePlayback();
			window.removeEventListener('blur', handleBlur);
		};
	}, []);
}
