// SPDX-License-Identifier: AGPL-3.0-or-later

import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import type React from 'react';
import {useLayoutEffect, useState} from 'react';

const CORNER_GAP_PX = 4;
const CORNER_STACK_GAP_PX = 8;

interface HorizontalRect {
	left: number;
	right: number;
	width: number;
	height: number;
}

function getVoiceCallCornerLift(bar: HorizontalRect | null, corners: ReadonlyArray<HorizontalRect>): number {
	if (!bar || bar.width === 0) return 0;
	const crowded = corners.some(
		(corner) => corner.width > 0 && corner.left < bar.right + CORNER_GAP_PX && bar.left - CORNER_GAP_PX < corner.right,
	);
	return crowded ? Math.ceil(appZoomLayoutPx(bar.height + CORNER_STACK_GAP_PX)) : 0;
}

export function useVoiceCallCornerLift(rootRef: React.RefObject<HTMLElement | null>): number {
	const [lift, setLift] = useState(0);
	useLayoutEffect(() => {
		const root = rootRef.current;
		if (!root || typeof ResizeObserver === 'undefined') return;
		const ownedBy = (element: Element) => element.closest('[data-voice-call-root]') === root;
		const observed = new Set<Element>();
		let frame: number | null = null;
		const measure = () => {
			frame = null;
			const bar = Array.from(root.querySelectorAll('[data-voice-call-control-bar]')).find(ownedBy) ?? null;
			const corners = Array.from(root.querySelectorAll('[data-voice-call-corner]')).filter(ownedBy);
			const targets = new Set<Element>(bar ? [bar, ...corners] : corners);
			for (const element of observed) {
				if (!targets.has(element)) {
					resizeObserver.unobserve(element);
					observed.delete(element);
				}
			}
			for (const element of targets) {
				if (!observed.has(element)) {
					resizeObserver.observe(element);
					observed.add(element);
				}
			}
			setLift(
				getVoiceCallCornerLift(
					bar?.getBoundingClientRect() ?? null,
					corners.map((corner) => corner.getBoundingClientRect()),
				),
			);
		};
		const schedule = () => {
			if (frame == null) frame = requestAnimationFrame(measure);
		};
		const resizeObserver = new ResizeObserver(schedule);
		const mutationObserver = new MutationObserver(schedule);
		resizeObserver.observe(root);
		mutationObserver.observe(root, {childList: true, subtree: true});
		measure();
		return () => {
			if (frame != null) cancelAnimationFrame(frame);
			resizeObserver.disconnect();
			mutationObserver.disconnect();
		};
	}, [rootRef]);
	return lift;
}
