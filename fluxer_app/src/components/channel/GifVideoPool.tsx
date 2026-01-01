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

import React from 'react';

class ElementPool<T> {
	private _elements: Array<T>;
	private _createElement: () => T;
	private _cleanElement: (element: T) => void;

	constructor(createElement: () => T, cleanElement: (element: T) => void) {
		this._elements = [];
		this._createElement = createElement;
		this._cleanElement = cleanElement;
	}

	getElement(): T {
		return this._elements.length === 0 ? this._createElement() : this._elements.pop()!;
	}

	poolElement(element: T): void {
		this._cleanElement(element);
		this._elements.push(element);
	}

	clearPool(): void {
		this._elements.length = 0;
	}
}

interface PooledVideo {
	getElement: (src?: string) => HTMLVideoElement;
	poolElement: (element: HTMLVideoElement, src?: string) => void;
	clearPool: () => void;
	getBlobUrl: (src: string) => Promise<string>;
	clearBlobCache: () => void;
	registerActive: (element: HTMLVideoElement) => void;
	unregisterActive: (element: HTMLVideoElement) => void;
	pauseAll: () => void;
	resumeAll: () => void;
}

const GifVideoPoolContext = React.createContext<PooledVideo | null>(null);

export const GifVideoPoolProvider = ({children}: {children: React.ReactNode}) => {
	const [videoPool] = React.useState<PooledVideo>(() => {
		const basePool = new ElementPool<HTMLVideoElement>(
			() => {
				const video = document.createElement('video');
				video.autoplay = true;
				video.loop = true;
				video.muted = true;
				video.playsInline = true;
				video.preload = 'auto';
				video.controls = false;
				video.style.width = '100%';
				video.style.height = '100%';
				video.style.objectFit = 'cover';
				video.style.display = 'block';
				return video;
			},
			(video) => {
				video.src = '';
				video.oncanplay = null;
				video.currentTime = 0;
				const {parentNode} = video;
				if (parentNode != null) {
					parentNode.removeChild(video);
				}
			},
		);

		const elementCache = new Map<string, HTMLVideoElement>();
		const MAX_ELEMENTS = 16;

		const blobCache = new Map<string, string>();
		const inflight = new Map<string, Promise<string>>();
		const MAX_BLOBS = 32;
		const activeElements = new Set<HTMLVideoElement>();

		const evictOldestBlob = () => {
			const oldest = blobCache.keys().next();
			if (!oldest.done) {
				const key = oldest.value;
				const url = blobCache.get(key);
				if (url) {
					URL.revokeObjectURL(url);
				}
				blobCache.delete(key);
			}
		};

		const getBlobUrl = async (src: string): Promise<string> => {
			if (blobCache.has(src)) {
				return blobCache.get(src)!;
			}
			if (inflight.has(src)) {
				return inflight.get(src)!;
			}

			const promise = (async () => {
				const response = await fetch(src, {cache: 'force-cache'});
				const blob = await response.blob();
				const url = URL.createObjectURL(blob);
				if (blobCache.size >= MAX_BLOBS) {
					evictOldestBlob();
				}
				blobCache.set(src, url);
				return url;
			})().finally(() => {
				inflight.delete(src);
			});

			inflight.set(src, promise);
			return promise;
		};

		return {
			getElement(src?: string): HTMLVideoElement {
				if (src && elementCache.has(src)) {
					const el = elementCache.get(src)!;
					elementCache.delete(src);
					return el;
				}
				return basePool.getElement();
			},
			poolElement(element: HTMLVideoElement, src?: string): void {
				activeElements.delete(element);
				const {parentNode} = element;
				if (parentNode != null) {
					parentNode.removeChild(element);
				}

				if (src) {
					element.oncanplay = null;
					element.pause();
					element.currentTime = 0;
					element.src = '';
					if (elementCache.size >= MAX_ELEMENTS) {
						const oldestKey = elementCache.keys().next().value as string | undefined;
						if (oldestKey) {
							const oldest = elementCache.get(oldestKey);
							if (oldest) {
								basePool.poolElement(oldest);
							}
							elementCache.delete(oldestKey);
						}
					}
					elementCache.set(src, element);
					return;
				}

				basePool.poolElement(element);
			},
			clearPool(): void {
				activeElements.clear();
				elementCache.forEach((el) => {
					el.src = '';
					el.oncanplay = null;
				});
				elementCache.clear();
				basePool.clearPool();
				blobCache.forEach((url) => URL.revokeObjectURL(url));
				blobCache.clear();
				inflight.clear();
			},
			registerActive(element: HTMLVideoElement) {
				activeElements.add(element);
			},
			unregisterActive(element: HTMLVideoElement) {
				activeElements.delete(element);
			},
			pauseAll() {
				activeElements.forEach((el) => {
					try {
						el.pause();
					} catch {}
				});
			},
			resumeAll() {
				activeElements.forEach((el) => {
					try {
						const playPromise = el.play();
						void playPromise?.catch(() => {});
					} catch {}
				});
			},
			getBlobUrl,
			clearBlobCache(): void {
				blobCache.forEach((url) => URL.revokeObjectURL(url));
				blobCache.clear();
			},
		};
	});

	React.useEffect(() => {
		return () => {
			videoPool.clearPool();
		};
	}, [videoPool]);

	return <GifVideoPoolContext.Provider value={videoPool}>{children}</GifVideoPoolContext.Provider>;
};

export const useGifVideoPool = (): PooledVideo => {
	const pool = React.useContext(GifVideoPoolContext);
	if (!pool) {
		throw new Error('useGifVideoPool must be used within GifVideoPoolProvider');
	}
	return pool;
};
