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

export const LONG_PRESS_DURATION_MS = 500;

const LONG_PRESS_MOVEMENT_THRESHOLD = 10;

const SWIPE_VELOCITY_THRESHOLD = 0.4;

const MIN_VELOCITY_SAMPLES = 2;

const MAX_VELOCITY_SAMPLE_AGE = 100;

const PRESS_HIGHLIGHT_DELAY_MS = 100;

const HAS_POINTER_EVENTS = 'PointerEvent' in window;

interface VelocitySample {
	x: number;
	y: number;
	timestamp: number;
}

type LongPressEvent = React.PointerEvent<HTMLDivElement> | React.TouchEvent<HTMLDivElement>;

interface LongPressableProps extends React.HTMLAttributes<HTMLDivElement> {
	delay?: number;
	onLongPress?: (event: LongPressEvent) => void;
	disabled?: boolean;
	pressedClassName?: string;
	onPressStateChange?: (isPressed: boolean) => void;
}

export const LongPressable = React.forwardRef<HTMLDivElement, LongPressableProps>(
	(
		{delay = LONG_PRESS_DURATION_MS, onLongPress, disabled, pressedClassName, onPressStateChange, ...rest},
		forwardedRef,
	) => {
		const innerRef = React.useRef<HTMLDivElement | null>(null);
		const longPressTimer = React.useRef<ReturnType<typeof setTimeout> | null>(null);
		const highlightTimer = React.useRef<ReturnType<typeof setTimeout> | null>(null);
		const pressStartPos = React.useRef<{x: number; y: number} | null>(null);
		const pointerIdRef = React.useRef<number | null>(null);
		const storedEvent = React.useRef<LongPressEvent | null>(null);
		const velocitySamples = React.useRef<Array<VelocitySample>>([]);
		const isPressIntent = React.useRef(false);
		const [isPressed, setIsPressed] = React.useState(false);

		React.useImperativeHandle(forwardedRef, () => innerRef.current as HTMLDivElement);

		const setPressed = React.useCallback(
			(pressed: boolean) => {
				setIsPressed(pressed);
				onPressStateChange?.(pressed);
			},
			[onPressStateChange],
		);

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

		const clearTimer = React.useCallback(() => {
			if (longPressTimer.current) {
				clearTimeout(longPressTimer.current);
				longPressTimer.current = null;
			}
			if (highlightTimer.current) {
				clearTimeout(highlightTimer.current);
				highlightTimer.current = null;
			}
			if (pointerIdRef.current !== null && innerRef.current?.releasePointerCapture) {
				try {
					innerRef.current.releasePointerCapture(pointerIdRef.current);
				} catch {}
			}
			pointerIdRef.current = null;
			pressStartPos.current = null;
			storedEvent.current = null;
			velocitySamples.current = [];
			isPressIntent.current = false;
			setPressed(false);
		}, [setPressed]);

		const {
			onPointerDown: userOnPointerDown,
			onPointerMove: userOnPointerMove,
			onPointerUp: userOnPointerUp,
			onPointerCancel: userOnPointerCancel,
			onTouchStart: userOnTouchStart,
			onTouchMove: userOnTouchMove,
			onTouchEnd: userOnTouchEnd,
			onTouchCancel: userOnTouchCancel,
			className,
			...restWithoutPointer
		} = rest;

		const startLongPressTimer = React.useCallback(
			(event: LongPressEvent, x: number, y: number, pointerId?: number, capturePointer = false) => {
				if (disabled || !onLongPress) return;
				clearTimer();
				pressStartPos.current = {x, y};
				pointerIdRef.current = pointerId ?? null;
				velocitySamples.current = [{x, y, timestamp: performance.now()}];
				isPressIntent.current = true;

				if (capturePointer && pointerId != null && innerRef.current?.setPointerCapture) {
					try {
						innerRef.current.setPointerCapture(pointerId);
					} catch {}
				}
				storedEvent.current = event;

				highlightTimer.current = setTimeout(() => {
					if (isPressIntent.current) {
						setPressed(true);
					}
					highlightTimer.current = null;
				}, PRESS_HIGHLIGHT_DELAY_MS);

				longPressTimer.current = setTimeout(() => {
					if (!disabled && onLongPress && storedEvent.current && isPressIntent.current) {
						onLongPress(storedEvent.current);
						setPressed(false);
					}
					clearTimer();
				}, delay);
			},
			[clearTimer, delay, disabled, onLongPress, setPressed],
		);

		const handlePointerDown = React.useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				userOnPointerDown?.(event);
				if (disabled || !onLongPress || event.button !== 0) return;
				if (event.pointerType !== 'touch') return;
				startLongPressTimer(event, event.clientX, event.clientY, event.pointerId, true);
			},
			[disabled, onLongPress, startLongPressTimer, userOnPointerDown],
		);

		const handlePointerMove = React.useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				userOnPointerMove?.(event);
				if (pointerIdRef.current !== event.pointerId) return;
				const startPos = pressStartPos.current;
				if (!startPos) return;

				velocitySamples.current.push({x: event.clientX, y: event.clientY, timestamp: performance.now()});
				if (velocitySamples.current.length > 10) {
					velocitySamples.current = velocitySamples.current.slice(-10);
				}

				const deltaX = Math.abs(event.clientX - startPos.x);
				const deltaY = Math.abs(event.clientY - startPos.y);

				if (deltaX > LONG_PRESS_MOVEMENT_THRESHOLD || deltaY > LONG_PRESS_MOVEMENT_THRESHOLD) {
					clearTimer();
					return;
				}

				const velocity = calculateVelocity();
				if (velocity > SWIPE_VELOCITY_THRESHOLD) {
					clearTimer();
				}
			},
			[clearTimer, calculateVelocity, userOnPointerMove],
		);

		const handlePointerUp = React.useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (pointerIdRef.current === event.pointerId) {
					clearTimer();
				}
				userOnPointerUp?.(event);
			},
			[clearTimer, userOnPointerUp],
		);

		const handlePointerCancel = React.useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (pointerIdRef.current === event.pointerId) {
					clearTimer();
				}
				userOnPointerCancel?.(event);
			},
			[clearTimer, userOnPointerCancel],
		);

		const handleTouchStart = React.useCallback(
			(event: React.TouchEvent<HTMLDivElement>) => {
				userOnTouchStart?.(event);
				if (disabled || !onLongPress) return;
				const touch = event.touches[0];
				if (!touch) return;
				startLongPressTimer(event, touch.clientX, touch.clientY);
			},
			[disabled, onLongPress, startLongPressTimer, userOnTouchStart],
		);

		const handleTouchMove = React.useCallback(
			(event: React.TouchEvent<HTMLDivElement>) => {
				userOnTouchMove?.(event);
				if (!pressStartPos.current) return;
				const touch = event.touches[0];
				if (!touch) return;

				velocitySamples.current.push({x: touch.clientX, y: touch.clientY, timestamp: performance.now()});
				if (velocitySamples.current.length > 10) {
					velocitySamples.current = velocitySamples.current.slice(-10);
				}

				const deltaX = Math.abs(touch.clientX - pressStartPos.current.x);
				const deltaY = Math.abs(touch.clientY - pressStartPos.current.y);

				if (deltaX > LONG_PRESS_MOVEMENT_THRESHOLD || deltaY > LONG_PRESS_MOVEMENT_THRESHOLD) {
					clearTimer();
					return;
				}

				const velocity = calculateVelocity();
				if (velocity > SWIPE_VELOCITY_THRESHOLD) {
					clearTimer();
				}
			},
			[clearTimer, calculateVelocity, userOnTouchMove],
		);

		const handleTouchEnd = React.useCallback(
			(event: React.TouchEvent<HTMLDivElement>) => {
				clearTimer();
				userOnTouchEnd?.(event);
			},
			[clearTimer, userOnTouchEnd],
		);

		const handleTouchCancel = React.useCallback(
			(event: React.TouchEvent<HTMLDivElement>) => {
				clearTimer();
				userOnTouchCancel?.(event);
			},
			[clearTimer, userOnTouchCancel],
		);

		React.useEffect(() => {
			const handleScroll = () => {
				if (isPressIntent.current) {
					clearTimer();
				}
			};

			window.addEventListener('scroll', handleScroll, {capture: true, passive: true});
			return () => {
				window.removeEventListener('scroll', handleScroll, {capture: true});
				if (longPressTimer.current) {
					clearTimeout(longPressTimer.current);
				}
				if (highlightTimer.current) {
					clearTimeout(highlightTimer.current);
				}
			};
		}, [clearTimer]);

		const finalClassName = isPressed && pressedClassName ? `${className ?? ''} ${pressedClassName}`.trim() : className;

		return (
			<div
				ref={innerRef}
				className={finalClassName}
				onPointerDown={HAS_POINTER_EVENTS ? handlePointerDown : undefined}
				onPointerMove={HAS_POINTER_EVENTS ? handlePointerMove : undefined}
				onPointerUp={HAS_POINTER_EVENTS ? handlePointerUp : undefined}
				onPointerCancel={HAS_POINTER_EVENTS ? handlePointerCancel : undefined}
				onTouchStart={!HAS_POINTER_EVENTS ? handleTouchStart : undefined}
				onTouchMove={!HAS_POINTER_EVENTS ? handleTouchMove : undefined}
				onTouchEnd={!HAS_POINTER_EVENTS ? handleTouchEnd : undefined}
				onTouchCancel={!HAS_POINTER_EVENTS ? handleTouchCancel : undefined}
				{...restWithoutPointer}
			/>
		);
	},
);
LongPressable.displayName = 'LongPressable';
