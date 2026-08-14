// SPDX-License-Identifier: AGPL-3.0-or-later

import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {decodeThumbHashDataURL} from '@app/features/messaging/utils/ThumbHashUtils';
import {type SyntheticEvent, useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

type MediaLoadingElement = HTMLImageElement | HTMLVideoElement;
type MediaElementStatus = 'loaded' | 'error' | 'pending';

export interface MediaLoadingState {
	loaded: boolean;
	error: boolean;
	cached: boolean;
	cachedOnMount: boolean;
	thumbHashURL?: string;
	ref: (element: MediaLoadingElement | null) => void;
	onLoad: (event: SyntheticEvent<MediaLoadingElement>) => void;
	onError: (event: SyntheticEvent<MediaLoadingElement>) => void;
}

interface UseMediaLoadingOptions {
	enabled?: boolean;
}

interface MediaLoadingFlags {
	loaded: boolean;
	error: boolean;
}

interface MediaLoadingSourceState extends MediaLoadingFlags {
	src: string;
	loadEnabled: boolean;
	cached: boolean;
}

interface MediaLoadingCacheAtMount {
	src: string;
	loadEnabled: boolean;
	cached: boolean;
}

interface MediaLoadingSourceIdentity {
	src: string;
	loadEnabled: boolean;
}

function isImageElement(element: MediaLoadingElement): element is HTMLImageElement {
	return element.tagName === 'IMG';
}

function getMediaElementStatus(element: MediaLoadingElement | null): MediaElementStatus {
	if (!element) return 'pending';
	if (isImageElement(element)) {
		if (!element.currentSrc && !element.src) return 'pending';
		if (element.complete && element.naturalWidth > 0) return 'loaded';
		if (element.complete) return 'error';
		return 'pending';
	}
	if (element.error) return 'error';
	return element.readyState >= 2 ? 'loaded' : 'pending';
}

function mediaElementMatchesSource(element: MediaLoadingElement | null, src: string): boolean {
	if (!element || src.length === 0) return false;
	let resolvedSource = src;
	try {
		resolvedSource = new URL(src, element.ownerDocument.baseURI).href;
	} catch {
		resolvedSource = src;
	}
	const matchesResolvedSource = (value: string): boolean => {
		if (value === src || value === resolvedSource) return true;
		try {
			return new URL(value, element.ownerDocument.baseURI).href === resolvedSource;
		} catch {
			return false;
		}
	};
	const attributeSource = element.getAttribute('src');
	if (attributeSource != null && !matchesResolvedSource(attributeSource)) return false;
	if (element.src.length > 0 && !matchesResolvedSource(element.src)) return false;
	if (element.currentSrc.length > 0 && !matchesResolvedSource(element.currentSrc)) return false;
	return attributeSource != null || element.src.length > 0 || element.currentSrc.length > 0;
}

function createMediaLoadingSourceState(src: string, loadEnabled: boolean, cached: boolean): MediaLoadingSourceState {
	return {
		src,
		loadEnabled,
		cached,
		loaded: cached,
		error: false,
	};
}

function resolveMediaLoadingSourceState(
	state: MediaLoadingSourceState,
	src: string,
	loadEnabled: boolean,
	cached: boolean,
): MediaLoadingSourceState {
	if (state.src === src && state.loadEnabled === loadEnabled) return state;
	return createMediaLoadingSourceState(src, loadEnabled, cached);
}

function sourceIdentityMatches(
	currentIdentity: MediaLoadingSourceIdentity,
	expectedIdentity: MediaLoadingSourceIdentity,
): boolean {
	return currentIdentity.src === expectedIdentity.src && currentIdentity.loadEnabled === expectedIdentity.loadEnabled;
}

function pendingMediaLoadingState(src: string, loadEnabled: boolean): MediaLoadingSourceState {
	return {src, loadEnabled, cached: false, loaded: false, error: false};
}

export function useMediaLoading(
	src: string,
	placeholder?: string,
	options: UseMediaLoadingOptions = {},
): MediaLoadingState {
	const {enabled = true} = options;
	const shouldForcePlaceholder = DeveloperOptions.forceRenderPlaceholders || DeveloperOptions.forceMediaLoading;
	const mediaElementRef = useRef<MediaLoadingElement | null>(null);
	const loadEnabled = enabled && src.length > 0 && !shouldForcePlaceholder;
	const sourceIdentity = useMemo<MediaLoadingSourceIdentity>(() => ({src, loadEnabled}), [loadEnabled, src]);
	const committedSourceIdentityRef = useRef(sourceIdentity);
	const initialCached = loadEnabled && ImageCacheUtils.hasImage(src);
	const [cacheAtMount] = useState<MediaLoadingCacheAtMount>(() => ({
		src,
		loadEnabled,
		cached: initialCached,
	}));
	const cachedOnMount = cacheAtMount.src === src && cacheAtMount.loadEnabled === loadEnabled && cacheAtMount.cached;
	const [sourceState, setSourceState] = useState<MediaLoadingSourceState>(() =>
		createMediaLoadingSourceState(src, loadEnabled, initialCached),
	);
	const currentSourceState = resolveMediaLoadingSourceState(sourceState, src, loadEnabled, initialCached);
	const thumbHashURL = useMemo(() => {
		return decodeThumbHashDataURL(placeholder);
	}, [placeholder]);
	const ref = useCallback((element: MediaLoadingElement | null) => {
		mediaElementRef.current = element;
	}, []);
	useLayoutEffect(() => {
		committedSourceIdentityRef.current = sourceIdentity;
		if (!loadEnabled) {
			setSourceState(pendingMediaLoadingState(src, loadEnabled));
			return;
		}
		const element = mediaElementRef.current;
		if (mediaElementMatchesSource(element, src)) {
			const status = getMediaElementStatus(element);
			if (status === 'loaded') {
				if (element != null && isImageElement(element)) ImageCacheUtils.rememberImage(src, element);
				setSourceState({src, loadEnabled, cached: true, loaded: true, error: false});
				return;
			}
			if (status === 'error') {
				ImageCacheUtils.forgetImage(src);
				setSourceState({src, loadEnabled, cached: false, loaded: false, error: true});
				return;
			}
		}
		const cached = ImageCacheUtils.hasImage(src);
		setSourceState(createMediaLoadingSourceState(src, loadEnabled, cached));
	}, [loadEnabled, sourceIdentity, src]);
	const onLoad = useCallback(
		(event: SyntheticEvent<MediaLoadingElement>) => {
			const element = event.currentTarget;
			if (!sourceIdentityMatches(committedSourceIdentityRef.current, sourceIdentity)) return;
			if (mediaElementRef.current !== element) return;
			if (!mediaElementMatchesSource(element, src)) return;
			if (isImageElement(element)) ImageCacheUtils.rememberImage(src, element);
			setSourceState({src, loadEnabled, cached: true, loaded: true, error: false});
		},
		[loadEnabled, sourceIdentity, src],
	);
	const onError = useCallback(
		(event: SyntheticEvent<MediaLoadingElement>) => {
			const element = event.currentTarget;
			if (!sourceIdentityMatches(committedSourceIdentityRef.current, sourceIdentity)) return;
			if (mediaElementRef.current !== element) return;
			if (!mediaElementMatchesSource(element, src)) return;
			ImageCacheUtils.forgetImage(src);
			setSourceState((currentState) => {
				if (currentState.src === src && currentState.loadEnabled === loadEnabled && currentState.loaded) {
					return {...currentState, cached: false};
				}
				return {src, loadEnabled, cached: false, loaded: false, error: true};
			});
		},
		[loadEnabled, sourceIdentity, src],
	);
	return {
		loaded: currentSourceState.loaded,
		error: currentSourceState.error,
		cached: currentSourceState.cached,
		cachedOnMount,
		thumbHashURL,
		ref,
		onLoad,
		onError,
	};
}
