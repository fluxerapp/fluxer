// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	createVideoDecodabilityProbe,
	type VideoDecodabilityProbe,
} from '@app/features/voice/components/media_player/utils/MediaDecodability';
import type React from 'react';
import {useEffect, useRef} from 'react';

const logger = new Logger('VideoDecodability');

export interface UseVideoDecodabilityOptions {
	videoRef: React.RefObject<HTMLVideoElement | null>;
	isPlaying: boolean;
	src: string;
}

export function useVideoDecodability(options: UseVideoDecodabilityOptions): void {
	const {videoRef, isPlaying, src} = options;
	const probeRef = useRef<VideoDecodabilityProbe | null>(null);
	useEffect(() => {
		const probe = createVideoDecodabilityProbe({
			readSample: () => {
				const video = videoRef.current;
				if (!video) return null;
				return {
					videoHeight: video.videoHeight,
					readyState: video.readyState,
					currentTime: video.currentTime,
				};
			},
			schedule: (callback, delayMs) => {
				const timer = setTimeout(callback, delayMs);
				return () => clearTimeout(timer);
			},
			onVerdict: (verdict) => {
				if (verdict !== 'undecodable') return;
				logger.warn('video advanced its clock without ever decoding a frame', {
					src,
					readyState: videoRef.current?.readyState ?? null,
					currentTime: videoRef.current?.currentTime ?? null,
				});
			},
		});
		probeRef.current = probe;
		return () => {
			probeRef.current = null;
			probe.cancel();
		};
	}, [src, videoRef]);
	useEffect(() => {
		if (!isPlaying) return;
		const video = videoRef.current;
		const probe = probeRef.current;
		if (video == null || probe == null) return;
		probe.probe();
		video.addEventListener('timeupdate', probe.probe);
		return () => {
			video.removeEventListener('timeupdate', probe.probe);
		};
	}, [isPlaying, videoRef]);
}
