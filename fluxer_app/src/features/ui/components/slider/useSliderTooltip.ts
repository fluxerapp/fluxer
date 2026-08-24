// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getFullscreenOverflowBoundary, useTooltipPortalRoot} from '@app/features/ui/tooltip/Tooltip';
import {appZoomCssPx, appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {
	getReducedMotionProps,
	type MotionAnimation,
	TOOLTIP_MOTION,
} from '@app/features/ui/utils/ReducedMotionAnimation';
import {arrow, autoUpdate, computePosition, flip, offset, shift} from '@floating-ui/react-dom';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const logger = new Logger('Slider');

export interface SliderTooltipPosition {
	x: number;
	y: number;
	arrowX: number;
	arrowY: number;
	isReady: boolean;
}

interface UseSliderTooltipOptions {
	showTooltip: boolean;
	value: number;
	isDragging: boolean;
	thumbRef: React.RefObject<HTMLDivElement | null>;
	portalRoot?: HTMLElement | null;
}

const initialTooltipPosition: SliderTooltipPosition = {
	x: 0,
	y: 0,
	arrowX: 0,
	arrowY: 0,
	isReady: false,
};

export interface SliderTooltipController {
	tooltipRef: React.RefObject<HTMLDivElement | null>;
	arrowRef: React.RefObject<HTMLDivElement | null>;
	tooltipPortalRoot: HTMLElement | undefined;
	tooltipMotion: MotionAnimation;
	tooltipPosition: SliderTooltipPosition;
	resetTooltipPosition: () => void;
	updateTooltipPosition: () => void;
}

export function useSliderTooltip({
	showTooltip,
	value,
	thumbRef,
	portalRoot,
}: UseSliderTooltipOptions): SliderTooltipController {
	const tooltipRef = useRef<HTMLDivElement>(null);
	const arrowRef = useRef<HTMLDivElement>(null);
	const cleanupRef = useRef<(() => void) | null>(null);
	const isCalculatingRef = useRef(false);
	const rafRef = useRef<number | null>(null);
	const rafWindowRef = useRef<Window | null>(null);
	const getThumbOwnerWindow = useCallback((): Window => {
		return thumbRef.current?.ownerDocument.defaultView ?? window;
	}, [thumbRef]);
	const globalTooltipPortalRoot = useTooltipPortalRoot(
		showTooltip && portalRoot == null,
		thumbRef.current?.ownerDocument,
	);
	const tooltipPortalRoot = portalRoot ?? globalTooltipPortalRoot;
	const isCrossDocumentTooltip = tooltipPortalRoot != null && tooltipPortalRoot.ownerDocument !== document;
	const tooltipMotion = getReducedMotionProps(TOOLTIP_MOTION, Accessibility.useReducedMotion || isCrossDocumentTooltip);
	const [tooltipPosition, setTooltipPosition] = useState<SliderTooltipPosition>(initialTooltipPosition);
	const cancelScheduledPosition = useCallback(() => {
		if (rafRef.current != null) {
			(rafWindowRef.current ?? window).cancelAnimationFrame(rafRef.current);
			rafRef.current = null;
			rafWindowRef.current = null;
		}
	}, []);
	const resetTooltipPosition = useCallback(() => {
		cancelScheduledPosition();
		setTooltipPosition((prev) => ({...prev, isReady: false}));
	}, [cancelScheduledPosition]);
	useEffect(() => {
		if (!showTooltip) {
			resetTooltipPosition();
		}
	}, [resetTooltipPosition, showTooltip]);
	const updateTooltipPositionNow = useCallback(async () => {
		if (!showTooltip || !thumbRef.current || !tooltipRef.current || isCalculatingRef.current) {
			return;
		}
		isCalculatingRef.current = true;
		try {
			const target = thumbRef.current;
			const tooltip = tooltipRef.current;
			Object.assign(tooltip.style, {
				position: 'fixed',
				left: '-9999px',
				top: '-9999px',
			});
			const overflowBoundary = getFullscreenOverflowBoundary(target);
			const boundaryOptions = overflowBoundary ? {boundary: overflowBoundary} : undefined;
			const middleware = [
				offset(8),
				flip(boundaryOptions),
				shift({padding: 8, ...boundaryOptions}),
				arrow({element: arrowRef}),
			];
			const {x, y, middlewareData} = await computePosition(target, tooltip, {
				placement: 'top',
				strategy: 'fixed',
				middleware,
			});
			Object.assign(tooltip.style, {
				left: appZoomCssPx(x),
				top: appZoomCssPx(y),
			});
			setTooltipPosition({
				x: appZoomLayoutPx(x),
				y: appZoomLayoutPx(y),
				arrowX: appZoomLayoutPx(middlewareData.arrow?.x ?? 0),
				arrowY: middlewareData.arrow?.y ?? 0,
				isReady: true,
			});
		} catch (error) {
			logger.error('Error positioning slider tooltip:', error);
			if (tooltipRef.current) {
				tooltipRef.current.style.visibility = 'visible';
			}
		} finally {
			isCalculatingRef.current = false;
		}
	}, [showTooltip, thumbRef]);
	const updateTooltipPosition = useCallback(() => {
		if (rafRef.current != null) return;
		const ownerWindow = getThumbOwnerWindow();
		rafWindowRef.current = ownerWindow;
		rafRef.current = ownerWindow.requestAnimationFrame(() => {
			rafRef.current = null;
			rafWindowRef.current = null;
			void updateTooltipPositionNow();
		});
	}, [getThumbOwnerWindow, updateTooltipPositionNow]);
	useLayoutEffect(() => {
		if (!showTooltip || !thumbRef.current || !tooltipRef.current) {
			if (cleanupRef.current) {
				cleanupRef.current();
				cleanupRef.current = null;
			}
			return;
		}
		updateTooltipPosition();
		if (thumbRef.current.ownerDocument.contains(thumbRef.current)) {
			cleanupRef.current = autoUpdate(thumbRef.current, tooltipRef.current, updateTooltipPosition);
		}
		return () => {
			if (cleanupRef.current) {
				cleanupRef.current();
				cleanupRef.current = null;
			}
			cancelScheduledPosition();
		};
	}, [showTooltip, thumbRef, updateTooltipPosition, cancelScheduledPosition]);
	useEffect(() => {
		if (showTooltip) {
			updateTooltipPosition();
		}
	}, [value, showTooltip, updateTooltipPosition]);
	return {
		tooltipRef,
		arrowRef,
		tooltipPortalRoot,
		tooltipMotion,
		tooltipPosition,
		resetTooltipPosition,
		updateTooltipPosition,
	};
}
