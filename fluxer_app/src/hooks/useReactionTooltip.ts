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

import {autoUpdate, computePosition, flip, offset, shift} from '@floating-ui/react-dom';
import React from 'react';

interface ReactionTooltipState {
	x: number;
	y: number;
	isOpen: boolean;
	isReady: boolean;
}

export function useReactionTooltip(hoverDelay = 500) {
	const targetRef = React.useRef<HTMLElement | null>(null);
	const tooltipRef = React.useRef<HTMLDivElement | null>(null);
	const cleanupRef = React.useRef<(() => void) | null>(null);
	const hoverTimerRef = React.useRef<NodeJS.Timeout | null>(null);
	const closeTimerRef = React.useRef<NodeJS.Timeout | null>(null);
	const isCalculatingRef = React.useRef(false);

	const [state, setState] = React.useState<ReactionTooltipState>({
		x: 0,
		y: 0,
		isOpen: false,
		isReady: false,
	});

	const clearTimers = React.useCallback(() => {
		if (hoverTimerRef.current) {
			clearTimeout(hoverTimerRef.current);
			hoverTimerRef.current = null;
		}
		if (closeTimerRef.current) {
			clearTimeout(closeTimerRef.current);
			closeTimerRef.current = null;
		}
	}, []);

	const isInHoverRegion = React.useCallback((node: EventTarget | null) => {
		if (!node || !(node instanceof Node)) return false;
		return !!targetRef.current?.contains(node) || !!tooltipRef.current?.contains(node);
	}, []);

	const updatePosition = React.useCallback(async () => {
		if (!state.isOpen || !targetRef.current || !tooltipRef.current || isCalculatingRef.current) {
			return;
		}

		if (!document.contains(targetRef.current)) {
			setState((prev) => ({...prev, isOpen: false, isReady: false}));
			return;
		}

		isCalculatingRef.current = true;

		try {
			const target = targetRef.current;
			const tooltip = tooltipRef.current;

			Object.assign(tooltip.style, {
				position: 'fixed',
				left: '-9999px',
				top: '-9999px',
			});

			const middleware = [offset(8), flip(), shift({padding: 8})];

			const {x, y} = await computePosition(target, tooltip, {
				placement: 'top',
				middleware,
			});

			Object.assign(tooltip.style, {
				left: `${x}px`,
				top: `${y}px`,
			});

			setState((prev) => ({...prev, x, y, isReady: true}));
		} catch (error) {
			console.error('Error positioning reaction tooltip:', error);
		} finally {
			isCalculatingRef.current = false;
		}
	}, [state.isOpen]);

	const show = React.useCallback(() => {
		clearTimers();
		setState((prev) => ({...prev, isOpen: true, isReady: false}));
	}, [clearTimers]);

	const hide = React.useCallback(() => {
		clearTimers();
		setState({x: 0, y: 0, isOpen: false, isReady: false});
	}, [clearTimers]);

	const handleMouseEnter = React.useCallback(() => {
		clearTimers();
		if (state.isOpen) return;
		hoverTimerRef.current = setTimeout(() => {
			show();
		}, hoverDelay);
	}, [show, hoverDelay, clearTimers, state.isOpen]);

	const handleMouseLeave = React.useCallback(
		(event: React.MouseEvent) => {
			clearTimers();

			if (isInHoverRegion(event.relatedTarget)) {
				return;
			}

			closeTimerRef.current = setTimeout(() => {
				hide();
			}, 100);
		},
		[hide, clearTimers],
	);

	const handleTooltipMouseEnter = React.useCallback(() => {
		clearTimers();
	}, [clearTimers]);

	const handleTooltipMouseLeave = React.useCallback(
		(event: React.MouseEvent) => {
			clearTimers();

			if (isInHoverRegion(event.relatedTarget)) {
				return;
			}

			closeTimerRef.current = setTimeout(() => {
				hide();
			}, 100);
		},
		[hide, clearTimers, isInHoverRegion],
	);

	React.useEffect(() => {
		if (!state.isOpen || !targetRef.current || !tooltipRef.current) {
			if (cleanupRef.current) {
				cleanupRef.current();
				cleanupRef.current = null;
			}
			return;
		}

		if (!document.contains(targetRef.current)) {
			setState({x: 0, y: 0, isOpen: false, isReady: false});
			return;
		}

		updatePosition();

		cleanupRef.current = autoUpdate(targetRef.current, tooltipRef.current, updatePosition, {
			ancestorScroll: true,
			ancestorResize: true,
			elementResize: true,
			layoutShift: true,
		});

		return () => {
			if (cleanupRef.current) {
				cleanupRef.current();
				cleanupRef.current = null;
			}
		};
	}, [state.isOpen, updatePosition]);

	React.useEffect(() => {
		return () => {
			clearTimers();
			if (cleanupRef.current) {
				cleanupRef.current();
				cleanupRef.current = null;
			}
		};
	}, [clearTimers]);

	return {
		targetRef,
		tooltipRef,
		state,
		updatePosition,
		show,
		hide,
		handlers: {
			onMouseEnter: handleMouseEnter,
			onMouseLeave: handleMouseLeave,
		},
		tooltipHandlers: {
			onMouseEnter: handleTooltipMouseEnter,
			onMouseLeave: handleTooltipMouseLeave,
		},
	};
}
