// SPDX-License-Identifier: AGPL-3.0-or-later

import {sameAttachmentSource} from '@app/features/messaging/state/AttachmentUrlRefresher';
import {armPendingSeekTarget, clampMediaTime} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';
import {type RefObject, useEffect, useRef} from 'react';

const HAVE_METADATA_READY_STATE = 1;

interface MediaSourceResume {
	time: number;
	playing: boolean;
}

interface MediaSourceResumeRefs {
	resumeRef: RefObject<MediaSourceResume | null>;
	playOnSwapRef: RefObject<boolean>;
}

export function useMediaSourceResume(
	mediaRef: RefObject<HTMLMediaElement | null>,
	attached = true,
): MediaSourceResumeRefs {
	const resumeRef = useRef<MediaSourceResume | null>(null);
	const playOnSwapRef = useRef(false);
	useEffect(() => {
		const media = mediaRef.current;
		if (!media || !attached) return;
		let source = media.currentSrc;
		let time = 0;
		let playing = !media.paused;
		let pausedByError = false;
		const recordTime = () => {
			if (media.readyState >= HAVE_METADATA_READY_STATE) time = media.currentTime;
		};
		const markPlaying = () => {
			pausedByError = false;
			playing = true;
		};
		const markStopped = () => {
			if (pausedByError) {
				pausedByError = false;
				return;
			}
			playing = false;
		};
		const handleError = () => {
			pausedByError = playing;
		};
		const handleEnded = () => {
			playing = false;
			time = 0;
		};
		const handleLoadStart = () => {
			const nextSource = media.currentSrc;
			const swapped = source !== '' && source !== nextSource && sameAttachmentSource(source, nextSource);
			const playOnSwap = playOnSwapRef.current;
			playOnSwapRef.current = false;
			pausedByError = false;
			source = nextSource;
			if (!swapped) {
				resumeRef.current = null;
				time = 0;
				playing = !media.paused;
				return;
			}
			playing = playing || playOnSwap;
			resumeRef.current = {time, playing};
			if (playing) void Promise.resolve(media.play()).catch(() => {});
		};
		const handleLoadedMetadata = () => {
			const resume = resumeRef.current;
			resumeRef.current = null;
			if (resume === null || resume.time <= 0) return;
			const target = clampMediaTime(resume.time, media.duration);
			armPendingSeekTarget(media, target);
			media.currentTime = target;
		};
		media.addEventListener('timeupdate', recordTime);
		media.addEventListener('seeking', recordTime);
		media.addEventListener('play', markPlaying);
		media.addEventListener('pause', markStopped);
		media.addEventListener('ended', handleEnded);
		media.addEventListener('error', handleError);
		media.addEventListener('loadstart', handleLoadStart);
		media.addEventListener('loadedmetadata', handleLoadedMetadata);
		return () => {
			media.removeEventListener('timeupdate', recordTime);
			media.removeEventListener('seeking', recordTime);
			media.removeEventListener('play', markPlaying);
			media.removeEventListener('pause', markStopped);
			media.removeEventListener('ended', handleEnded);
			media.removeEventListener('error', handleError);
			media.removeEventListener('loadstart', handleLoadStart);
			media.removeEventListener('loadedmetadata', handleLoadedMetadata);
		};
	}, [attached, mediaRef]);
	return {resumeRef, playOnSwapRef};
}
