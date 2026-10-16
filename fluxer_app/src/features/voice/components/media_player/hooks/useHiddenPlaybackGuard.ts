// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect, useRef} from 'react';

interface UseHiddenPlaybackGuardOptions {
	videoRef: React.RefObject<HTMLVideoElement | null>;
	onHoldPlayback: () => void;
	onReleasePlayback: () => void;
}

export function useHiddenPlaybackGuard({
	videoRef,
	onHoldPlayback,
	onReleasePlayback,
}: UseHiddenPlaybackGuardOptions): void {
	const callbacksRef = useRef({onHoldPlayback, onReleasePlayback});
	callbacksRef.current = {onHoldPlayback, onReleasePlayback};
	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		const isPlayingOutsideTheDocument = () => document.pictureInPictureElement === video;
		const shouldHoldPlayback = () => document.visibilityState === 'hidden' && !isPlayingOutsideTheDocument();
		const handlePlay = () => {
			if (!shouldHoldPlayback()) return;
			callbacksRef.current.onHoldPlayback();
		};
		const handleVisibilityChange = () => {
			if (document.visibilityState !== 'visible') return;
			callbacksRef.current.onReleasePlayback();
		};
		video.addEventListener('play', handlePlay);
		document.addEventListener('visibilitychange', handleVisibilityChange);
		if (!video.paused && shouldHoldPlayback()) {
			callbacksRef.current.onHoldPlayback();
		}
		return () => {
			video.removeEventListener('play', handlePlay);
			document.removeEventListener('visibilitychange', handleVisibilityChange);
		};
	}, [videoRef]);
}
