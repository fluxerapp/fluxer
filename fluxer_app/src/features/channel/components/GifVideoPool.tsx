// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAnimatedMediaPlaybackAllowed} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import React, {useContext, useEffect, useState} from 'react';

const PENDING_PLAY = Symbol('pendingPlay');
const PLAY_REQUEST_ID = Symbol('playRequestId');

type VideoWithPending = HTMLVideoElement & {[PENDING_PLAY]?: Promise<void> | null; [PLAY_REQUEST_ID]?: number};

export function safePlay(video: HTMLVideoElement): Promise<void> {
	if (!getAnimatedMediaPlaybackAllowed()) return Promise.resolve();
	const v = video as VideoWithPending;
	const playRequestId = (v[PLAY_REQUEST_ID] ?? 0) + 1;
	v[PLAY_REQUEST_ID] = playRequestId;
	const promise = v.play().catch(() => {});
	v[PENDING_PLAY] = promise;
	void promise.finally(() => {
		if (v[PENDING_PLAY] === promise) v[PENDING_PLAY] = null;
	});
	return promise;
}

export function safePause(video: HTMLVideoElement): void {
	const v = video as VideoWithPending;
	const pending = v[PENDING_PLAY];
	const doPause = () => {
		try {
			v.pause();
		} catch {}
	};
	if (pending) {
		const playRequestId = v[PLAY_REQUEST_ID] ?? 0;
		void pending.finally(() => {
			if (v[PLAY_REQUEST_ID] === playRequestId) doPause();
		});
	} else {
		doPause();
	}
}

class ElementPool<T> {
	private idleElements: Array<T>;
	private spawnElement: () => T;
	private recycleElement: (element: T) => void;

	constructor(spawnElement: () => T, recycleElement: (element: T) => void) {
		this.idleElements = [];
		this.spawnElement = spawnElement;
		this.recycleElement = recycleElement;
	}

	takeElement(): T {
		return this.idleElements.length === 0 ? this.spawnElement() : this.idleElements.pop()!;
	}

	returnElement(element: T): void {
		this.recycleElement(element);
		this.idleElements.push(element);
	}

	dropAllElements(): void {
		this.idleElements.length = 0;
	}
}

interface PooledVideo {
	takeElement: () => HTMLVideoElement;
	returnElement: (element: HTMLVideoElement) => void;
	dropAllElements: () => void;
	registerActive: (element: HTMLVideoElement) => void;
	unregisterActive: (element: HTMLVideoElement) => void;
	pauseAll: () => void;
	resumeAll: () => void;
	isGloballyPaused: () => boolean;
}

const GifVideoPoolContext = React.createContext<PooledVideo | null>(null);
export const GifVideoPoolProvider = ({children}: {children: React.ReactNode}) => {
	const [videoPool] = useState<PooledVideo>(() => {
		const basePool = new ElementPool<HTMLVideoElement>(
			() => {
				const video = document.createElement('video');
				video.autoplay = false;
				video.loop = true;
				video.muted = true;
				video.playsInline = true;
				video.setAttribute('playsinline', '');
				video.setAttribute('webkit-playsinline', '');
				video.preload = 'auto';
				video.controls = false;
				video.style.width = '100%';
				video.style.height = '100%';
				video.style.objectFit = 'cover';
				video.style.display = 'block';
				return video;
			},
			(video) => {
				video.oncanplay = null;
				video.removeAttribute('src');
				video.load();
				const {parentNode} = video;
				if (parentNode != null) {
					parentNode.removeChild(video);
				}
			},
		);
		const activeElements = new Set<HTMLVideoElement>();
		let globallyPaused = !getAnimatedMediaPlaybackAllowed();
		return {
			takeElement(): HTMLVideoElement {
				return basePool.takeElement();
			},
			returnElement(element: HTMLVideoElement): void {
				activeElements.delete(element);
				const {parentNode} = element;
				if (parentNode != null) {
					parentNode.removeChild(element);
				}
				basePool.returnElement(element);
			},
			dropAllElements(): void {
				activeElements.clear();
				basePool.dropAllElements();
			},
			registerActive(element: HTMLVideoElement) {
				activeElements.add(element);
				if (globallyPaused) {
					safePause(element);
				}
			},
			unregisterActive(element: HTMLVideoElement) {
				activeElements.delete(element);
			},
			pauseAll() {
				globallyPaused = true;
				activeElements.forEach(safePause);
			},
			resumeAll() {
				globallyPaused = false;
				activeElements.forEach((el) => {
					void safePlay(el);
				});
			},
			isGloballyPaused() {
				return globallyPaused;
			},
		};
	});
	useEffect(() => {
		return () => {
			videoPool.dropAllElements();
		};
	}, [videoPool]);
	return <GifVideoPoolContext.Provider value={videoPool}>{children}</GifVideoPoolContext.Provider>;
};
export const useGifVideoPool = (): PooledVideo => {
	const pool = useContext(GifVideoPoolContext);
	if (!pool) {
		throw new Error('useGifVideoPool must be used within GifVideoPoolProvider');
	}
	return pool;
};
