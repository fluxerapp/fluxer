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

import {
	autoUpdate,
	computePosition,
	flip,
	limitShift,
	type Middleware,
	offset,
	type Placement,
	shift,
	size,
} from '@floating-ui/react-dom';
import React from 'react';
import {getAdaptivePadding} from '~/utils/positioning';

const DEFAULT_MIDDLEWARE: Array<Middleware> = [];

interface FloatingState {
	x: number;
	y: number;
	isReady: boolean;
}

interface UseAntiShiftFloatingOptions {
	placement: Placement;
	offsetMainAxis?: number;
	offsetCrossAxis?: number;
	middleware?: Array<Middleware>;
	shouldAutoUpdate?: boolean;
	enableSmartBoundary?: boolean;
	constrainHeight?: boolean;
}

export function useAntiShiftFloating(
	target: HTMLElement | null,
	enabled: boolean,
	options: UseAntiShiftFloatingOptions,
) {
	const {
		placement,
		offsetMainAxis = 8,
		offsetCrossAxis = 0,
		middleware: extraMiddleware = DEFAULT_MIDDLEWARE,
		shouldAutoUpdate = true,
		enableSmartBoundary = false,
		constrainHeight = false,
	} = options;

	const floatingRef = React.useRef<HTMLElement>(null);
	const [state, setState] = React.useState<FloatingState>({
		x: -9999,
		y: -9999,
		isReady: false,
	});
	const cleanupRef = React.useRef<(() => void) | null>(null);
	const isCalculatingRef = React.useRef(false);
	const rafIdRef = React.useRef<number | null>(null);

	const updatePosition = React.useCallback(async () => {
		if (!enabled || !target || !floatingRef.current || isCalculatingRef.current) {
			return;
		}

		isCalculatingRef.current = true;

		if (rafIdRef.current !== null) {
			cancelAnimationFrame(rafIdRef.current);
		}

		rafIdRef.current = requestAnimationFrame(async () => {
			try {
				const floating = floatingRef.current;
				if (!floating) {
					isCalculatingRef.current = false;
					return;
				}

				Object.assign(floating.style, {
					visibility: 'hidden',
					position: 'fixed',
					left: '-9999px',
					top: '-9999px',
				});

				const adaptivePadding = enableSmartBoundary ? getAdaptivePadding() : 8;
				const shiftPadding = Math.max(6, adaptivePadding);

				const middleware: Array<Middleware> = [
					offset({mainAxis: offsetMainAxis, crossAxis: offsetCrossAxis}),
					flip({padding: shiftPadding}),
					shift({
						padding: shiftPadding,
						limiter: limitShift({
							offset: adaptivePadding,
						}),
					}),
					...extraMiddleware,
				];

				if (constrainHeight) {
					middleware.push(
						size({
							apply({availableHeight, elements}) {
								const maxHeight = Math.max(100, availableHeight - shiftPadding * 2);
								Object.assign(elements.floating.style, {
									maxHeight: `${maxHeight}px`,
									overflowY: maxHeight < 400 ? 'auto' : 'visible',
								});
							},
							padding: shiftPadding,
						}),
					);
				}

				const {x, y} = await computePosition(target, floating, {
					placement,
					middleware,
				});

				Object.assign(floating.style, {
					left: `${x}px`,
					top: `${y}px`,
					visibility: 'visible',
				});

				setState({x, y, isReady: true});
			} catch (error) {
				console.error('Error positioning floating element:', error);
				if (floatingRef.current) {
					floatingRef.current.style.visibility = 'visible';
				}
			} finally {
				isCalculatingRef.current = false;
				rafIdRef.current = null;
			}
		});
	}, [
		enabled,
		target,
		placement,
		offsetMainAxis,
		offsetCrossAxis,
		extraMiddleware,
		enableSmartBoundary,
		constrainHeight,
	]);

	React.useEffect(() => {
		if (!enabled || !target || !floatingRef.current) {
			setState((prev) => ({...prev, isReady: false}));
			return;
		}

		if (!document.contains(target)) {
			setState((prev) => ({...prev, isReady: false}));
			return;
		}

		updatePosition();

		if (shouldAutoUpdate) {
			cleanupRef.current = autoUpdate(target, floatingRef.current, updatePosition, {
				ancestorScroll: true,
				ancestorResize: true,
				elementResize: true,
				layoutShift: true,
			});
		}

		return () => {
			cleanupRef.current?.();
			cleanupRef.current = null;
			if (rafIdRef.current !== null) {
				cancelAnimationFrame(rafIdRef.current);
				rafIdRef.current = null;
			}
			setState((prev) => ({...prev, isReady: false}));
		};
	}, [enabled, target, shouldAutoUpdate, updatePosition]);

	return {
		ref: floatingRef,
		state,
		style: {
			position: 'fixed' as const,
			left: state.x,
			top: state.y,
		},
		updatePosition,
	};
}
