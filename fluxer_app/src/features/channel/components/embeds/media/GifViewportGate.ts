// SPDX-License-Identifier: AGPL-3.0-or-later

import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {observeIntersection} from '@app/features/platform/utils/SharedIntersectionObserver';
import {useEffect, useState} from 'react';

const ANIMATION_ROOT_MARGIN = '600px 0px';

export interface GifViewportGate {
	loadMedia: boolean;
	animate: boolean;
}

export function resolveGifViewportGate({
	isNearViewport,
	isInViewport,
	shouldBlur,
}: {
	isNearViewport: boolean;
	isInViewport: boolean;
	shouldBlur: boolean;
}): GifViewportGate {
	if (shouldBlur) return {loadMedia: false, animate: false};
	return {loadMedia: isNearViewport, animate: isNearViewport && isInViewport};
}

export function useGifViewportGate<T extends Element>({
	element,
	rememberKey,
	shouldBlur,
}: {
	element: Element | null;
	rememberKey: string;
	shouldBlur: boolean;
}): GifViewportGate & {ref: (node: T | null) => void} {
	const {ref, isNearViewport} = useNearViewport<T>({rememberKey});
	const [isInViewport, setIsInViewport] = useState(() => typeof IntersectionObserver === 'undefined');
	useEffect(() => {
		if (typeof IntersectionObserver === 'undefined') {
			setIsInViewport(true);
			return;
		}
		if (element == null) {
			setIsInViewport(false);
			return;
		}
		return observeIntersection(
			element,
			(entry) => {
				setIsInViewport(entry.isIntersecting || entry.intersectionRatio > 0);
			},
			{rootMargin: ANIMATION_ROOT_MARGIN},
		);
	}, [element]);
	return {ref, ...resolveGifViewportGate({isNearViewport, isInViewport, shouldBlur})};
}
