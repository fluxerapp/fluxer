/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useCallback, useEffect, useState} from 'react';
import {supportsDisablePictureInPicture} from '~/types/browser';

export interface UseMediaPiPOptions {
	videoRef: React.RefObject<HTMLVideoElement | null> | React.RefObject<HTMLVideoElement>;
	onPiPChange?: (isPiP: boolean) => void;
}

export interface UseMediaPiPReturn {
	isPiP: boolean;
	supportsPiP: boolean;
	enterPiP: () => Promise<void>;
	exitPiP: () => Promise<void>;
	togglePiP: () => Promise<void>;
}

export function useMediaPiP(options: UseMediaPiPOptions): UseMediaPiPReturn {
	const {videoRef, onPiPChange} = options;

	const [isPiP, setIsPiP] = useState(false);
	const [supportsPiP] = useState(() => {
		if (!document.pictureInPictureEnabled) return false;
		return true;
	});

	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;

		const handleEnterPiP = () => {
			setIsPiP(true);
			onPiPChange?.(true);
		};

		const handleLeavePiP = () => {
			setIsPiP(false);
			onPiPChange?.(false);
		};

		video.addEventListener('enterpictureinpicture', handleEnterPiP);
		video.addEventListener('leavepictureinpicture', handleLeavePiP);

		if (document.pictureInPictureElement === video) {
			setIsPiP(true);
		}

		return () => {
			video.removeEventListener('enterpictureinpicture', handleEnterPiP);
			video.removeEventListener('leavepictureinpicture', handleLeavePiP);
		};
	}, [videoRef, onPiPChange]);

	const enterPiP = useCallback(async () => {
		const video = videoRef.current;
		if (!video || !supportsPiP) return;

		if (supportsDisablePictureInPicture(video) && video.disablePictureInPicture) return;

		try {
			await video.requestPictureInPicture();
		} catch (error) {
			console.error('Failed to enter PiP:', error);
		}
	}, [videoRef, supportsPiP]);

	const exitPiP = useCallback(async () => {
		if (!document.pictureInPictureElement) return;

		try {
			await document.exitPictureInPicture();
		} catch (error) {
			console.error('Failed to exit PiP:', error);
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
