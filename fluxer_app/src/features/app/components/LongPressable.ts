// SPDX-License-Identifier: AGPL-3.0-or-later

import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import React, {useCallback, useEffect, useRef, useState} from 'react';

const LONG_PRESS_MOVEMENT_THRESHOLD = 10;
const SWIPE_VELOCITY_THRESHOLD = 0.4;
const MIN_VELOCITY_SAMPLES = 2;
const MAX_VELOCITY_SAMPLE_AGE = 100;
const PRESS_HIGHLIGHT_DELAY_MS = 100;
const LONG_PRESS_DURATION_MS = 500;
const LONG_PRESS_CLICK_SUPPRESSION_MS = 750;
const HAS_POINTER_EVENTS = typeof window !== 'undefined' && 'PointerEvent' in window;

interface VelocitySample {
	x: number;
	y: number;
	timestamp: number;
}

type LongPressableElement = 'div' | 'span';
type LongPressEvent = React.PointerEvent<HTMLElement> | React.TouchEvent<HTMLElement>;
type LongPressHandler = {
	bivarianceHack(event: LongPressEvent): void;
}['bivarianceHack'];

interface LongPressableProps extends React.HTMLAttributes<HTMLElement> {
	as?: LongPressableElement;
	delay?: number;
	onLongPress?: LongPressHandler;
	disabled?: boolean;
	pressedClassName?: string;
	onPressStateChange?: (isPressed: boolean) => void;
}

export const LongPressable = React.forwardRef<HTMLElement, LongPressableProps>(
	(
		{
			as: Component = 'div',
			delay = LONG_PRESS_DURATION_MS,
			onLongPress,
			disabled,
			pressedClassName,
			onPressStateChange,
			...rest
		},
		forwardedRef,
	) => {
		const innerRef = useRef<HTMLElement | null>(null);
		const longPressTimer = useRef<NodeJS.Timeout | null>(null);
		const highlightTimer = useRef<NodeJS.Timeout | null>(null);
		const pressStartPos = useRef<{x: number; y: number} | null>(null);
		const pointerIdRef = useRef<number | null>(null);
		const storedEvent = useRef<LongPressEvent | null>(null);
		const velocitySamples = useRef<Array<VelocitySample>>([]);
		const isPressIntent = useRef(false);
		const pressScrollCancelHandler = useRef<(() => void) | null>(null);
		const clearTimerRef = useRef<() => void>(() => {});
		const suppressNextClickRef = useRef(false);
		const suppressClickTimer = useRef<NodeJS.Timeout | null>(null);
		const [isPressed, setIsPressed] = useState(false);
		const composedRef = useMergeRefs<HTMLElement>([forwardedRef, innerRef]);
		const setPressed = useCallback(
			(pressed: boolean) => {
				setIsPressed(pressed);
				onPressStateChange?.(pressed);
			},
			[onPressStateChange],
		);
		const clearSuppressedClick = useCallback(() => {
			suppressNextClickRef.current = false;
			if (suppressClickTimer.current) {
				clearTimeout(suppressClickTimer.current);
				suppressClickTimer.current = null;
			}
		}, []);
		const suppressNextClick = useCallback(() => {
			suppressNextClickRef.current = true;
			if (suppressClickTimer.current) clearTimeout(suppressClickTimer.current);
			suppressClickTimer.current = setTimeout(() => {
				suppressClickTimer.current = null;
				suppressNextClickRef.current = false;
			}, LONG_PRESS_CLICK_SUPPRESSION_MS);
		}, []);
		const calculateVelocity = useCallback((): number => {
			const samples = velocitySamples.current;
			if (samples.length < MIN_VELOCITY_SAMPLES) return 0;
			const now = performance.now();
			let firstRecentIndex = 0;
			while (
				firstRecentIndex < samples.length &&
				now - samples[firstRecentIndex].timestamp >= MAX_VELOCITY_SAMPLE_AGE
			) {
				firstRecentIndex++;
			}
			if (samples.length - firstRecentIndex < MIN_VELOCITY_SAMPLES) return 0;
			const first = samples[firstRecentIndex];
			const last = samples[samples.length - 1];
			const dt = last.timestamp - first.timestamp;
			if (dt === 0) return 0;
			const dx = last.x - first.x;
			const dy = last.y - first.y;
			const distance = Math.sqrt(dx * dx + dy * dy);
			return distance / dt;
		}, []);
		const detachPressScrollCancel = useCallback(() => {
			const handler = pressScrollCancelHandler.current;
			if (handler == null) return;
			pressScrollCancelHandler.current = null;
			window.removeEventListener('scroll', handler, {capture: true});
		}, []);
		const attachPressScrollCancel = useCallback(() => {
			if (pressScrollCancelHandler.current != null) return;
			const handler = () => {
				if (isPressIntent.current) {
					clearTimerRef.current();
				}
			};
			pressScrollCancelHandler.current = handler;
			window.addEventListener('scroll', handler, {capture: true, passive: true});
		}, []);
		const clearTimer = useCallback(() => {
			detachPressScrollCancel();
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
		}, [detachPressScrollCancel, setPressed]);
		clearTimerRef.current = clearTimer;
		const {
			onPointerDown: userOnPointerDown,
			onPointerMove: userOnPointerMove,
			onPointerUp: userOnPointerUp,
			onPointerCancel: userOnPointerCancel,
			onTouchStart: userOnTouchStart,
			onTouchMove: userOnTouchMove,
			onTouchEnd: userOnTouchEnd,
			onTouchCancel: userOnTouchCancel,
			onClick: userOnClick,
			onKeyDown: userOnKeyDown,
			className,
			...restWithoutPointer
		} = rest;
		const handleKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLElement>) => {
				userOnKeyDown?.(event);
				if (event.defaultPrevented || userOnKeyDown || disabled || !userOnClick) return;
				if (restWithoutPointer.role !== 'button' || !isKeyboardActivationKey(event.key)) return;
				event.preventDefault();
				innerRef.current?.click();
			},
			[disabled, restWithoutPointer.role, userOnClick, userOnKeyDown],
		);
		const startLongPressTimer = useCallback(
			(event: LongPressEvent, x: number, y: number, pointerId?: number, capturePointer = false) => {
				if (disabled || !onLongPress) return;
				clearTimer();
				pressStartPos.current = {x, y};
				pointerIdRef.current = pointerId ?? null;
				velocitySamples.current = [{x, y, timestamp: performance.now()}];
				isPressIntent.current = true;
				attachPressScrollCancel();
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
						suppressNextClick();
						onLongPress(storedEvent.current);
						setPressed(false);
					}
					clearTimer();
				}, delay);
			},
			[attachPressScrollCancel, clearTimer, delay, disabled, onLongPress, setPressed, suppressNextClick],
		);
		const handlePointerDown = useCallback(
			(event: React.PointerEvent<HTMLElement>) => {
				userOnPointerDown?.(event);
				if (disabled || !onLongPress || event.button !== 0) return;
				if (event.pointerType !== 'touch') return;
				startLongPressTimer(event, event.clientX, event.clientY, event.pointerId, true);
			},
			[disabled, onLongPress, startLongPressTimer, userOnPointerDown],
		);
		const handlePointerMove = useCallback(
			(event: React.PointerEvent<HTMLElement>) => {
				userOnPointerMove?.(event);
				if (pointerIdRef.current !== event.pointerId) return;
				const startPos = pressStartPos.current;
				if (!startPos) return;
				velocitySamples.current.push({x: event.clientX, y: event.clientY, timestamp: performance.now()});
				if (velocitySamples.current.length > 10) {
					velocitySamples.current.splice(0, velocitySamples.current.length - 10);
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
		const handlePointerUp = useCallback(
			(event: React.PointerEvent<HTMLElement>) => {
				if (pointerIdRef.current === event.pointerId) {
					clearTimer();
				}
				userOnPointerUp?.(event);
			},
			[clearTimer, userOnPointerUp],
		);
		const handlePointerCancel = useCallback(
			(event: React.PointerEvent<HTMLElement>) => {
				if (pointerIdRef.current === event.pointerId) {
					clearTimer();
				}
				userOnPointerCancel?.(event);
			},
			[clearTimer, userOnPointerCancel],
		);
		const handleTouchStart = useCallback(
			(event: React.TouchEvent<HTMLElement>) => {
				userOnTouchStart?.(event);
				if (disabled || !onLongPress) return;
				const touch = event.touches[0];
				if (!touch) return;
				startLongPressTimer(event, touch.clientX, touch.clientY);
			},
			[disabled, onLongPress, startLongPressTimer, userOnTouchStart],
		);
		const handleTouchMove = useCallback(
			(event: React.TouchEvent<HTMLElement>) => {
				userOnTouchMove?.(event);
				if (!pressStartPos.current) return;
				const touch = event.touches[0];
				if (!touch) return;
				velocitySamples.current.push({x: touch.clientX, y: touch.clientY, timestamp: performance.now()});
				if (velocitySamples.current.length > 10) {
					velocitySamples.current.splice(0, velocitySamples.current.length - 10);
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
		const handleTouchEnd = useCallback(
			(event: React.TouchEvent<HTMLElement>) => {
				clearTimer();
				userOnTouchEnd?.(event);
			},
			[clearTimer, userOnTouchEnd],
		);
		const handleTouchCancel = useCallback(
			(event: React.TouchEvent<HTMLElement>) => {
				clearTimer();
				userOnTouchCancel?.(event);
			},
			[clearTimer, userOnTouchCancel],
		);
		const handleClick = useCallback(
			(event: React.MouseEvent<HTMLElement>) => {
				if (suppressNextClickRef.current) {
					event.preventDefault();
					event.stopPropagation();
					clearSuppressedClick();
					return;
				}
				userOnClick?.(event);
			},
			[clearSuppressedClick, userOnClick],
		);
		useEffect(() => {
			return () => {
				detachPressScrollCancel();
				if (longPressTimer.current) {
					clearTimeout(longPressTimer.current);
				}
				if (highlightTimer.current) {
					clearTimeout(highlightTimer.current);
				}
				if (suppressClickTimer.current) {
					clearTimeout(suppressClickTimer.current);
				}
			};
		}, [detachPressScrollCancel]);
		const finalClassName = isPressed && pressedClassName ? `${className ?? ''} ${pressedClassName}`.trim() : className;
		return React.createElement(Component, {
			ref: composedRef,
			className: finalClassName,
			onPointerDown: HAS_POINTER_EVENTS ? handlePointerDown : undefined,
			onPointerMove: HAS_POINTER_EVENTS ? handlePointerMove : undefined,
			onPointerUp: HAS_POINTER_EVENTS ? handlePointerUp : undefined,
			onPointerCancel: HAS_POINTER_EVENTS ? handlePointerCancel : undefined,
			onTouchStart: !HAS_POINTER_EVENTS ? handleTouchStart : undefined,
			onTouchMove: !HAS_POINTER_EVENTS ? handleTouchMove : undefined,
			onTouchEnd: !HAS_POINTER_EVENTS ? handleTouchEnd : undefined,
			onTouchCancel: !HAS_POINTER_EVENTS ? handleTouchCancel : undefined,
			onClick: userOnClick ? handleClick : undefined,
			onKeyDown: handleKeyDown,
			...restWithoutPointer,
			'data-long-press-owner': !disabled && onLongPress ? 'true' : undefined,
		});
	},
);

LongPressable.displayName = 'LongPressable';
