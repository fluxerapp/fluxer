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

import * as React from 'react';

export interface GifVideoPoolLike {
	getElement: (key: string) => HTMLVideoElement;
	getBlobUrl: (key: string) => Promise<string>;
	registerActive: (video: HTMLVideoElement) => void;
	unregisterActive: (video: HTMLVideoElement) => void;
	poolElement: (video: HTMLVideoElement, key: string) => void;
}

export function usePooledVideo({
	src,
	containerRef,
	videoPool,
	autoPlay,
	enabled = true,
}: {
	src: string | null | undefined;
	containerRef: React.RefObject<HTMLDivElement | null>;
	videoPool: GifVideoPoolLike;
	autoPlay: boolean;
	enabled?: boolean;
}) {
	const videoRef = React.useRef<HTMLVideoElement | null>(null);

	React.useEffect(() => {
		if (!enabled) return;
		if (!src) return;

		const container = containerRef.current;
		if (!container) return;

		let cancelled = false;
		let attached = false;

		const video = videoPool.getElement(src);
		videoRef.current = video;

		const run = async () => {
			try {
				const blobUrl = await videoPool.getBlobUrl(src);
				if (cancelled) return;
				if (video.src !== blobUrl) video.src = blobUrl;
			} catch {
				if (cancelled) return;
				if (video.src !== src) video.src = src;
			}

			if (cancelled) return;

			const currentContainer = containerRef.current;
			if (!currentContainer) return;

			currentContainer.appendChild(video);
			attached = true;
			videoPool.registerActive(video);

			if (!autoPlay) {
				video.pause();
			}
		};

		void run();

		return () => {
			cancelled = true;

			if (attached) {
				videoPool.unregisterActive(video);
			}

			try {
				video.pause();
				video.currentTime = 0;
			} catch {}

			videoPool.poolElement(video, src);

			if (videoRef.current === video) {
				videoRef.current = null;
			}
		};
	}, [src, enabled, containerRef, videoPool, autoPlay]);

	return videoRef;
}
