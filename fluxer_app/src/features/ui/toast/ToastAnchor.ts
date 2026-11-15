// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RefObject, useLayoutEffect} from 'react';

const anchors = new Set<HTMLElement>();

export function useToastAnchor(ref: RefObject<HTMLElement | null>): void {
	useLayoutEffect(() => {
		const element = ref.current;
		if (!element) return;
		anchors.add(element);
		return () => {
			anchors.delete(element);
		};
	}, [ref]);
}

export function getToastAnchorBottom(): number | null {
	let bottom: number | null = null;
	for (const element of anchors) {
		if (!element.isConnected) continue;
		const rect = element.getBoundingClientRect();
		if (rect.height === 0) continue;
		bottom = Math.max(bottom ?? 0, rect.bottom);
	}
	return bottom;
}
