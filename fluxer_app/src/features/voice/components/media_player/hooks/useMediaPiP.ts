// SPDX-License-Identifier: AGPL-3.0-or-later

import {supportsDisablePictureInPicture} from '@app/features/platform/types/Browser';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useCallback, useEffect, useState} from 'react';

const logger = new Logger('useMediaPiP');

export interface UseMediaPiPOptions {
	videoRef: React.RefObject<HTMLVideoElement | null> | React.RefObject<HTMLVideoElement>;
}

export interface UseMediaPiPReturn {
	isPiP: boolean;
	supportsPiP: boolean;
	enterPiP: () => Promise<void>;
	exitPiP: () => Promise<void>;
	togglePiP: () => Promise<void>;
}

export function useMediaPiP(options: UseMediaPiPOptions): UseMediaPiPReturn {
	const {videoRef} = options;
	const [isPiP, setIsPiP] = useState(false);
	const [supportsPiP] = useState(() => {
		if (!document.pictureInPictureEnabled) return false;
		return true;
	});
	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		const handleEnterPiP = () => setIsPiP(true);
		const handleLeavePiP = () => setIsPiP(false);
		video.addEventListener('enterpictureinpicture', handleEnterPiP);
		video.addEventListener('leavepictureinpicture', handleLeavePiP);
		if (document.pictureInPictureElement === video) {
			setIsPiP(true);
		}
		return () => {
			video.removeEventListener('enterpictureinpicture', handleEnterPiP);
			video.removeEventListener('leavepictureinpicture', handleLeavePiP);
		};
	}, [videoRef]);
	const enterPiP = useCallback(async () => {
		const video = videoRef.current;
		if (!video || !supportsPiP) return;
		if (supportsDisablePictureInPicture(video) && video.disablePictureInPicture) return;
		try {
			await video.requestPictureInPicture();
		} catch (error) {
			logger.error('Failed to enter PiP:', error);
		}
	}, [videoRef, supportsPiP]);
	const exitPiP = useCallback(async () => {
		if (!document.pictureInPictureElement) return;
		try {
			await document.exitPictureInPicture();
		} catch (error) {
			logger.error('Failed to exit PiP:', error);
		}
	}, []);
	const togglePiP = useCallback(async () => {
		if (isPiP) {
			await exitPiP();
		} else {
			await enterPiP();
		}
	}, [isPiP, enterPiP, exitPiP]);
	return {
		isPiP,
		supportsPiP,
		enterPiP,
		exitPiP,
		togglePiP,
	};
}
