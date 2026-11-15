// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	calculateSwipeVelocity,
	pushVelocitySample,
	SWIPE_VELOCITY_THRESHOLD,
	type VelocitySample,
} from '@app/features/app/hooks/SwipeVelocity';
import {useCallback, useEffect, useRef, useState} from 'react';

const PRESS_MOVEMENT_THRESHOLD = 10;
const PRESS_HIGHLIGHT_DELAY_MS = 100;
export const isTouchDevice = window.matchMedia?.('(hover: none) and (pointer: coarse)').matches ?? false;

interface UsePressableOptions {
	disabled?: boolean;
}

interface UsePressableResult {
	isPressed: boolean;
	pressableProps: {
		onPointerDown: (e: React.PointerEvent) => void;
		onPointerMove: (e: React.PointerEvent) => void;
		onPointerUp: (e: React.PointerEvent) => void;
		onPointerCancel: (e: React.PointerEvent) => void;
		onPointerLeave: (e: React.PointerEvent) => void;
	};
}

export function usePressable(options: UsePressableOptions | boolean = {}): UsePressableResult {
	const opts = typeof options === 'boolean' ? {disabled: options} : options;
	const {disabled = false} = opts;
	const [isPressed, setIsPressed] = useState(false);
	const pressStartPos = useRef<{
		x: number;
		y: number;
	} | null>(null);
	const activePointerId = useRef<number | null>(null);
	const highlightTimerId = useRef<NodeJS.Timeout | null>(null);
	const velocitySamples = useRef<Array<VelocitySample>>([]);
	const isPressIntent = useRef(false);
	const clearPress = useCallback(() => {
		if (highlightTimerId.current) {
			clearTimeout(highlightTimerId.current);
			highlightTimerId.current = null;
		}
		setIsPressed(false);
		pressStartPos.current = null;
		activePointerId.current = null;
		velocitySamples.current = [];
		isPressIntent.current = false;
	}, []);
	const handlePointerDown = useCallback(
		(e: React.PointerEvent) => {
			if (disabled || !isTouchDevice) return;
			if (e.pointerType !== 'touch') return;
			activePointerId.current = e.pointerId;
			pressStartPos.current = {x: e.clientX, y: e.clientY};
			velocitySamples.current = [{x: e.clientX, y: e.clientY, timestamp: performance.now()}];
			isPressIntent.current = true;
			if (highlightTimerId.current) {
				clearTimeout(highlightTimerId.current);
			}
			highlightTimerId.current = setTimeout(() => {
				if (isPressIntent.current) {
					setIsPressed(true);
				}
				highlightTimerId.current = null;
			});
		},
		[disabled, PRESS_HIGHLIGHT_DELAY_MS],
	);
	const handlePointerMove = useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current !== e.pointerId) return;
			const startPos = pressStartPos.current;
			if (!startPos) return;
			pushVelocitySample(velocitySamples.current, e.clientX, e.clientY);
			const deltaX = Math.abs(e.clientX - startPos.x);
			const deltaY = Math.abs(e.clientY - startPos.y);
			if (deltaX > PRESS_MOVEMENT_THRESHOLD || deltaY > PRESS_MOVEMENT_THRESHOLD) {
				clearPress();
				return;
			}
			const velocity = calculateSwipeVelocity(velocitySamples.current, performance.now());
			if (velocity > SWIPE_VELOCITY_THRESHOLD) {
				clearPress();
				return;
			}
		},
		[clearPress],
	);
	const handlePointerUp = useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current === e.pointerId) {
				clearPress();
			}
		},
		[clearPress],
	);
	const handlePointerCancel = useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current === e.pointerId) {
				clearPress();
			}
		},
		[clearPress],
	);
	const handlePointerLeave = useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current === e.pointerId) {
				clearPress();
			}
		},
		[clearPress],
	);
	useEffect(() => {
		if (!isTouchDevice) return;
		const handleScroll = () => {
			if (isPressIntent.current) {
				clearPress();
			}
		};
		window.addEventListener('scroll', handleScroll, {capture: true, passive: true});
		return () => {
			window.removeEventListener('scroll', handleScroll, {capture: true});
			if (highlightTimerId.current) {
				clearTimeout(highlightTimerId.current);
			}
		};
	}, [clearPress]);
	return {
		isPressed,
		pressableProps: {
			onPointerDown: handlePointerDown,
			onPointerMove: handlePointerMove,
			onPointerUp: handlePointerUp,
			onPointerCancel: handlePointerCancel,
			onPointerLeave: handlePointerLeave,
		},
	};
}
