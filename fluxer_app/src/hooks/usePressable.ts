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

import React from 'react';

const PRESS_MOVEMENT_THRESHOLD = 10;

const PRESS_HIGHLIGHT_DELAY_MS = 100;

const SWIPE_VELOCITY_THRESHOLD = 0.4;

const MIN_VELOCITY_SAMPLES = 2;

const MAX_VELOCITY_SAMPLE_AGE = 100;

export const isTouchDevice = window.matchMedia?.('(hover: none) and (pointer: coarse)').matches ?? false;

interface VelocitySample {
	x: number;
	y: number;
	timestamp: number;
}

interface UsePressableOptions {
	disabled?: boolean;
	highlightDelay?: number;
	movementThreshold?: number;
}

interface UsePressableResult {
	isPressed: boolean;
	clearPress: () => void;
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
	const {
		disabled = false,
		highlightDelay = PRESS_HIGHLIGHT_DELAY_MS,
		movementThreshold = PRESS_MOVEMENT_THRESHOLD,
	} = opts;

	const [isPressed, setIsPressed] = React.useState(false);
	const pressStartPos = React.useRef<{x: number; y: number} | null>(null);
	const activePointerId = React.useRef<number | null>(null);
	const highlightTimerId = React.useRef<ReturnType<typeof setTimeout> | null>(null);
	const velocitySamples = React.useRef<Array<VelocitySample>>([]);
	const isPressIntent = React.useRef(false);

	const clearPress = React.useCallback(() => {
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

	const calculateVelocity = React.useCallback((): number => {
		const samples = velocitySamples.current;
		if (samples.length < MIN_VELOCITY_SAMPLES) return 0;

		const now = performance.now();
		const recentSamples = samples.filter((s) => now - s.timestamp < MAX_VELOCITY_SAMPLE_AGE);
		if (recentSamples.length < MIN_VELOCITY_SAMPLES) return 0;

		const first = recentSamples[0];
		const last = recentSamples[recentSamples.length - 1];
		const dt = last.timestamp - first.timestamp;
		if (dt === 0) return 0;

		const dx = last.x - first.x;
		const dy = last.y - first.y;
		const distance = Math.sqrt(dx * dx + dy * dy);

		return distance / dt;
	}, []);

	const handlePointerDown = React.useCallback(
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
			}, highlightDelay);
		},
		[disabled, highlightDelay],
	);

	const handlePointerMove = React.useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current !== e.pointerId) return;

			const startPos = pressStartPos.current;
			if (!startPos) return;

			velocitySamples.current.push({x: e.clientX, y: e.clientY, timestamp: performance.now()});
			if (velocitySamples.current.length > 10) {
				velocitySamples.current = velocitySamples.current.slice(-10);
			}

			const deltaX = Math.abs(e.clientX - startPos.x);
			const deltaY = Math.abs(e.clientY - startPos.y);

			if (deltaX > movementThreshold || deltaY > movementThreshold) {
				clearPress();
				return;
			}

			const velocity = calculateVelocity();
			if (velocity > SWIPE_VELOCITY_THRESHOLD) {
				clearPress();
				return;
			}
		},
		[clearPress, calculateVelocity, movementThreshold],
	);

	const handlePointerUp = React.useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current === e.pointerId) {
				clearPress();
			}
		},
		[clearPress],
	);

	const handlePointerCancel = React.useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current === e.pointerId) {
				clearPress();
			}
		},
		[clearPress],
	);

	const handlePointerLeave = React.useCallback(
		(e: React.PointerEvent) => {
			if (!isTouchDevice) return;
			if (activePointerId.current === e.pointerId) {
				clearPress();
			}
		},
		[clearPress],
	);

	React.useEffect(() => {
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
		clearPress,
		pressableProps: {
			onPointerDown: handlePointerDown,
			onPointerMove: handlePointerMove,
			onPointerUp: handlePointerUp,
			onPointerCancel: handlePointerCancel,
			onPointerLeave: handlePointerLeave,
		},
	};
}
