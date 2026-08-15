// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RefObject, useCallback, useEffect, useRef} from 'react';

export function useDragTargetRect(targetRef: RefObject<HTMLElement | null>): () => DOMRect | null {
	const cachedRectRef = useRef<DOMRect | null>(null);
	const clearFrameRef = useRef<number | null>(null);
	const clearFrameWindowRef = useRef<Window | null>(null);
	const getTargetRect = useCallback((): DOMRect | null => {
		const target = targetRef.current;
		if (target == null) return null;
		if (cachedRectRef.current != null) return cachedRectRef.current;
		const targetRect = target.getBoundingClientRect();
		cachedRectRef.current = targetRect;
		const ownerWindow = target.ownerDocument.defaultView;
		if (ownerWindow != null && clearFrameRef.current == null) {
			clearFrameWindowRef.current = ownerWindow;
			clearFrameRef.current = ownerWindow.requestAnimationFrame(() => {
				clearFrameRef.current = null;
				clearFrameWindowRef.current = null;
				cachedRectRef.current = null;
			});
		}
		return targetRect;
	}, [targetRef]);
	useEffect(() => {
		return () => {
			if (clearFrameWindowRef.current != null && clearFrameRef.current != null) {
				clearFrameWindowRef.current.cancelAnimationFrame(clearFrameRef.current);
			}
			clearFrameRef.current = null;
			clearFrameWindowRef.current = null;
			cachedRectRef.current = null;
		};
	}, [targetRef]);
	return getTargetRect;
}
