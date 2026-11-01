// SPDX-License-Identifier: AGPL-3.0-or-later

import {sameAttachmentSource} from '@app/features/messaging/state/AttachmentUrlRefresher';
import {armPendingSeekTarget, clampMediaTime} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';
import {type RefObject, useEffect, useRef} from 'react';

const HAVE_METADATA_READY_STATE = 1;

interface MediaSourceResume {
	time: number;
	playing: boolean;
}

export function useMediaSourceResume(
	mediaRef: RefObject<HTMLMediaElement | null>,
	attached = true,
): RefObject<MediaSourceResume | null> {
	const resumeRef = useRef<MediaSourceResume | null>(null);
	useEffect(() => {
		const media = mediaRef.current;
		if (!media || !attached) return;
		let source = media.currentSrc;
		let time = 0;
		let playing = !media.paused;
		const recordTime = () => {
			if (media.readyState >= HAVE_METADATA_READY_STATE) time = media.currentTime;
		};
		const markPlaying = () => {
			playing = true;
		};
		const markStopped = () => {
			playing = false;
		};
		const handleEnded = () => {
			playing = false;
			time = 0;
		};
		const handleLoadStart = () => {
			const nextSource = media.currentSrc;
			const swapped = source !== '' && source !== nextSource && sameAttachmentSource(source, nextSource);
			source = nextSource;
			if (!swapped) {
				resumeRef.current = null;
				time = 0;
				playing = !media.paused;
				return;
			}
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
		media.addEventListener('loadstart', handleLoadStart);
		media.addEventListener('loadedmetadata', handleLoadedMetadata);
		return () => {
			media.removeEventListener('timeupdate', recordTime);
			media.removeEventListener('seeking', recordTime);
			media.removeEventListener('play', markPlaying);
			media.removeEventListener('pause', markStopped);
			media.removeEventListener('ended', handleEnded);
			media.removeEventListener('loadstart', handleLoadStart);
			media.removeEventListener('loadedmetadata', handleLoadedMetadata);
		};
	}, [attached, mediaRef]);
	return resumeRef;
}
