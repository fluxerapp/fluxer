// SPDX-License-Identifier: AGPL-3.0-or-later

import {useHover} from '@app/features/app/hooks/useHover';
import {type ShouldAnimateKind, useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import UserSettings from '@app/features/user/state/UserSettings';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

interface UseAnimatedImageUrlOptions {
	staticUrl: string | null;
	animatedUrl?: string | null;
	kind: ShouldAnimateKind;
	isFocused?: boolean;
}

interface AnimatedImageUrlState {
	hoverRef: React.RefCallback<HTMLElement>;
	imageUrl: string | null;
	shouldAnimate: boolean;
	showGifIndicator: boolean;
}

type AnimatedImageLoadStatus = 'idle' | 'loading' | 'loaded' | 'failed';

interface AnimatedImageLoadState {
	url: string | null;
	status: AnimatedImageLoadStatus;
}

export function useAnimatedImageUrl({
	staticUrl,
	animatedUrl: providedAnimatedUrl,
	kind,
	isFocused,
}: UseAnimatedImageUrlOptions): AnimatedImageUrlState {
	const animatedUrl: string | null = providedAnimatedUrl == null ? null : providedAnimatedUrl;
	const [hoverRef, isHovering] = useHover();
	const shouldAnimate = useShouldAnimate({kind, isHovering, isFocused});
	const gifAutoPlayEnabled = kind === 'gif' && UserSettings.getGifAutoPlay();
	const hasAnimatedUrl = Boolean(animatedUrl && animatedUrl !== staticUrl);
	const initialAnimatedLoadState: AnimatedImageLoadState = {
		url: animatedUrl,
		status: animatedUrl != null && ImageCacheUtils.hasImage(animatedUrl) ? 'loaded' : 'idle',
	};
	const [animatedLoadState, setAnimatedLoadState] = useState<AnimatedImageLoadState>(() => initialAnimatedLoadState);
	const animatedLoadStateRef = useRef<AnimatedImageLoadState>(initialAnimatedLoadState);
	const animatedUrlCached = animatedUrl != null && ImageCacheUtils.hasImage(animatedUrl);
	useEffect(() => {
		const currentLoadState = animatedLoadStateRef.current;
		if (currentLoadState.url === animatedUrl) return;
		const idleState: AnimatedImageLoadState = {url: animatedUrl, status: 'idle'};
		animatedLoadStateRef.current = idleState;
		setAnimatedLoadState(idleState);
	}, [animatedUrl]);
	useEffect(() => {
		if (!animatedUrlCached || animatedUrl == null) return;
		const currentLoadState = animatedLoadStateRef.current;
		if (currentLoadState.url === animatedUrl && currentLoadState.status === 'loaded') return;
		const loadedState: AnimatedImageLoadState = {url: animatedUrl, status: 'loaded'};
		animatedLoadStateRef.current = loadedState;
		setAnimatedLoadState(loadedState);
	}, [animatedUrl, animatedUrlCached]);
	useEffect(() => {
		if (!shouldAnimate || animatedUrl == null || animatedUrlCached) return;
		const currentLoadState = animatedLoadStateRef.current;
		if (currentLoadState.url === animatedUrl && currentLoadState.status === 'failed') return;
		if (currentLoadState.url === animatedUrl && currentLoadState.status === 'loading') return;
		if (currentLoadState.url === animatedUrl && currentLoadState.status === 'loaded') return;
		const loadingState: AnimatedImageLoadState = {url: animatedUrl, status: 'loading'};
		animatedLoadStateRef.current = loadingState;
		setAnimatedLoadState(loadingState);
		let active = true;
		const cleanup = ImageCacheUtils.loadImage(
			animatedUrl,
			() => {
				if (!active) return;
				const loadedState: AnimatedImageLoadState = {url: animatedUrl, status: 'loaded'};
				animatedLoadStateRef.current = loadedState;
				setAnimatedLoadState(loadedState);
			},
			() => {
				if (!active) return;
				const failedState: AnimatedImageLoadState = {url: animatedUrl, status: 'failed'};
				animatedLoadStateRef.current = failedState;
				setAnimatedLoadState(failedState);
			},
		);
		return () => {
			active = false;
			cleanup();
		};
	}, [animatedUrl, animatedUrlCached, shouldAnimate]);
	useEffect(() => {
		if (shouldAnimate) return;
		const currentLoadState = animatedLoadStateRef.current;
		if (currentLoadState.status !== 'failed' && currentLoadState.status !== 'loading') return;
		const idleState: AnimatedImageLoadState = {url: animatedUrl, status: 'idle'};
		animatedLoadStateRef.current = idleState;
		setAnimatedLoadState(idleState);
	}, [animatedUrl, shouldAnimate]);
	const animatedSourceReady =
		shouldAnimate &&
		animatedUrl != null &&
		animatedLoadState.url === animatedUrl &&
		animatedLoadState.status === 'loaded';
	const imageUrl = animatedSourceReady ? animatedUrl : staticUrl;
	const showGifIndicator = kind === 'gif' && hasAnimatedUrl && !gifAutoPlayEnabled && !shouldAnimate;
	return {hoverRef, imageUrl, shouldAnimate, showGifIndicator};
}
