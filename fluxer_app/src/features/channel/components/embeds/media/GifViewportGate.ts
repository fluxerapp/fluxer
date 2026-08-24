// SPDX-License-Identifier: AGPL-3.0-or-later

import {useViewportPlayback} from '@app/features/app/hooks/useViewportPlayback';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';

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
	const isInViewport = useViewportPlayback(element);
	return {ref, ...resolveGifViewportGate({isNearViewport, isInViewport, shouldBlur})};
}
