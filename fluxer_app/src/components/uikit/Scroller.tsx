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

import {clsx} from 'clsx';
import {
	type CSSProperties,
	forwardRef,
	type KeyboardEvent,
	type MouseEvent as ReactMouseEvent,
	type ReactNode,
	type UIEvent,
	useCallback,
	useEffect,
	useImperativeHandle,
	useRef,
} from 'react';
import FocusRingScope from '~/components/uikit/FocusRing/FocusRingScope';
import styles from './Scroller.module.css';

export interface ScrollerState {
	scrollTop: number;
	scrollHeight: number;
	offsetHeight: number;
	scrollLeft: number;
	scrollWidth: number;
	offsetWidth: number;
}

export interface ScrollerHandle {
	getScrollerNode(): HTMLElement | null;
	getScrollerState(): ScrollerState;
	scrollTo(options: {to: number; animate?: boolean; callback?: () => void}): void;
	mergeTo(options: {to: number; callback?: () => void}): void;
	scrollIntoViewRect(options: {
		start: number;
		end: number;
		shouldScrollToStart?: boolean;
		padding?: number;
		animate?: boolean;
		callback?: () => void;
	}): void;
	scrollIntoViewNode(options: {
		node: HTMLElement;
		shouldScrollToStart?: boolean;
		padding?: number;
		animate?: boolean;
		callback?: () => void;
	}): void;
	scrollPageUp(options?: {animate?: boolean; callback?: () => void}): void;
	scrollPageDown(options?: {animate?: boolean; callback?: () => void}): void;
	scrollToTop(options?: {animate?: boolean; callback?: () => void}): void;
	scrollToBottom(options?: {animate?: boolean; callback?: () => void}): void;
	isScrolledToTop(): boolean;
	isScrolledToBottom(): boolean;
	getDistanceFromTop(): number;
	getDistanceFromBottom(): number;
}

type ScrollAxis = 'vertical' | 'horizontal';
type ScrollOverflow = 'scroll' | 'auto' | 'hidden';

interface ScrollerProps {
	children: ReactNode;
	className?: string;
	dir?: 'ltr' | 'rtl';
	orientation?: ScrollAxis;
	overflow?: ScrollOverflow;
	fade?: boolean;
	scrollbar?: 'thin' | 'regular';
	hideThumbWhenWindowBlurred?: boolean;
	reserveScrollbarTrack?: boolean;
	style?: CSSProperties;
	onScroll?: (event: UIEvent<HTMLDivElement>) => void;
	onKeyDown?: (event: KeyboardEvent<HTMLDivElement>) => void;
	onMouseLeave?: (event: ReactMouseEvent<HTMLDivElement>) => void;
	onResize?: (entry: ResizeObserverEntry, type: 'container' | 'content') => void;
}

function getScrollState(element: HTMLElement | null): ScrollerState {
	if (!element) {
		return {
			scrollTop: 0,
			scrollHeight: 0,
			offsetHeight: 0,
			scrollLeft: 0,
			scrollWidth: 0,
			offsetWidth: 0,
		};
	}
	return {
		scrollTop: element.scrollTop,
		scrollHeight: element.scrollHeight,
		offsetHeight: element.offsetHeight,
		scrollLeft: element.scrollLeft,
		scrollWidth: element.scrollWidth,
		offsetWidth: element.offsetWidth,
	};
}

function measureElementOffset(element: HTMLElement, axis: ScrollAxis, container: HTMLElement): number {
	const elementRect = element.getBoundingClientRect();
	const containerRect = container.getBoundingClientRect();
	const scrollPos = axis === 'horizontal' ? container.scrollLeft : container.scrollTop;
	const delta = axis === 'horizontal' ? elementRect.left - containerRect.left : elementRect.top - containerRect.top;
	return scrollPos + delta;
}

type SpringConfig = {
	stiffness: number;
	damping: number;
	mass: number;
	precision: number;
	maxDurationMs: number;
};

const DEFAULT_SPRING: SpringConfig = {
	stiffness: 520,
	damping: 70,
	mass: 1,
	precision: 0.5,
	maxDurationMs: 1400,
};

function prefersReducedMotion(): boolean {
	return window.matchMedia?.('(prefers-reduced-motion: reduce)')?.matches ?? false;
}

export const Scroller = forwardRef<ScrollerHandle, ScrollerProps>(function Scroller(
	{
		children,
		className,
		dir = 'ltr',
		orientation = 'vertical',
		overflow = 'auto',
		fade = true,
		scrollbar = 'thin',
		hideThumbWhenWindowBlurred = false,
		reserveScrollbarTrack = true,
		style,
		onScroll,
		onKeyDown,
		onMouseLeave,
		onResize,
	},
	forwardedRef,
) {
	const scrollRef = useRef<HTMLDivElement>(null);
	const rootRef = useRef<HTMLDivElement>(null);
	const scrollTimeoutRef = useRef<number>(0);
	const onResizeRef = useRef(onResize);
	onResizeRef.current = onResize;

	const animRef = useRef<{
		token: number;
		rafId: number | null;
		velocity: number;
		lastTime: number;
		startTime: number;
		target: number;
		callback?: () => void;
	} | null>(null);

	const cancelAnimation = useCallback(() => {
		const anim = animRef.current;
		if (!anim) return;

		if (anim.rafId != null) {
			cancelAnimationFrame(anim.rafId);
		}
		animRef.current = null;
	}, []);

	const getMaxScroll = useCallback(
		(node: HTMLDivElement): number => {
			if (orientation === 'vertical') {
				return Math.max(0, node.scrollHeight - node.offsetHeight);
			}
			return Math.max(0, node.scrollWidth - node.offsetWidth);
		},
		[orientation],
	);

	const getScrollPos = useCallback(
		(node: HTMLDivElement): number => (orientation === 'vertical' ? node.scrollTop : node.scrollLeft),
		[orientation],
	);

	const setScrollPos = useCallback(
		(node: HTMLDivElement, value: number) => {
			if (orientation === 'vertical') {
				node.scrollTop = value;
			} else {
				node.scrollLeft = value;
			}
		},
		[orientation],
	);

	const clampToRange = useCallback(
		(node: HTMLDivElement, value: number): number => {
			const max = getMaxScroll(node);
			return Math.max(0, Math.min(value, max));
		},
		[getMaxScroll],
	);

	const springScrollTo = useCallback(
		(node: HTMLDivElement, target: number, callback?: () => void, config: SpringConfig = DEFAULT_SPRING) => {
			cancelAnimation();

			const token = (animRef.current?.token ?? 0) + 1;
			const now = performance.now();

			animRef.current = {
				token,
				rafId: null,
				velocity: 0,
				lastTime: now,
				startTime: now,
				target: clampToRange(node, target),
				callback,
			};

			const step = (t: number) => {
				const anim = animRef.current;
				if (!anim || anim.token !== token) return;

				anim.target = clampToRange(node, anim.target);

				const dtRaw = (t - anim.lastTime) / 1000;
				const dt = Math.max(0, Math.min(0.04, dtRaw));
				anim.lastTime = t;

				const x = getScrollPos(node);
				const displacement = anim.target - x;

				const a = (config.stiffness * displacement - config.damping * anim.velocity) / config.mass;

				anim.velocity += a * dt;
				let next = x + anim.velocity * dt;
				next = clampToRange(node, next);

				setScrollPos(node, next);

				const elapsed = t - anim.startTime;
				const settled = Math.abs(displacement) <= config.precision && Math.abs(anim.velocity) <= config.precision;
				const timedOut = elapsed >= config.maxDurationMs;

				if (settled || timedOut) {
					setScrollPos(node, clampToRange(node, anim.target));

					const cb = anim.callback;
					animRef.current = null;
					cb?.();
					return;
				}

				anim.rafId = requestAnimationFrame(step);
			};

			animRef.current.rafId = requestAnimationFrame(step);
		},
		[cancelAnimation, clampToRange, getScrollPos, setScrollPos],
	);

	useEffect(() => {
		if (!onResizeRef.current) return;

		const containerEl = rootRef.current;
		const contentEl = scrollRef.current;
		if (!containerEl || !contentEl) return;

		const containerObserver = new ResizeObserver((entries) => {
			for (const entry of entries) {
				onResizeRef.current?.(entry, 'container');
			}
		});

		const contentObserver = new ResizeObserver((entries) => {
			for (const entry of entries) {
				onResizeRef.current?.(entry, 'content');
			}
		});

		containerObserver.observe(containerEl);
		contentObserver.observe(contentEl);

		return () => {
			containerObserver.disconnect();
			contentObserver.disconnect();
		};
	}, []);

	useEffect(() => {
		return () => cancelAnimation();
	}, [cancelAnimation]);

	useEffect(() => {
		if (!hideThumbWhenWindowBlurred) return;

		const el = scrollRef.current;
		if (!el) return;

		const update = () => {
			el.classList.toggle(styles.windowBlurred, !document.hasFocus());
		};

		update();
		window.addEventListener('blur', update);
		window.addEventListener('focus', update);
		document.addEventListener('visibilitychange', update);

		return () => {
			window.removeEventListener('blur', update);
			window.removeEventListener('focus', update);
			document.removeEventListener('visibilitychange', update);
		};
	}, [hideThumbWhenWindowBlurred]);

	useImperativeHandle(
		forwardedRef,
		() => ({
			getScrollerNode: () => scrollRef.current,
			getScrollerState: () => getScrollState(scrollRef.current),

			scrollTo({to, animate = false, callback}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}

				const clampedTo = clampToRange(node, to);

				if (!animate || prefersReducedMotion()) {
					cancelAnimation();
					setScrollPos(node, clampedTo);
					callback?.();
					return;
				}

				springScrollTo(node, clampedTo, callback);
			},

			mergeTo({to, callback}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}

				cancelAnimation();
				setScrollPos(node, clampToRange(node, to));
				callback?.();
			},

			scrollIntoViewRect({start, end, shouldScrollToStart = false, padding = 0, animate = false, callback}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}

				const scrollPos = getScrollPos(node);
				const viewportSize = orientation === 'vertical' ? node.offsetHeight : node.offsetWidth;

				const paddedStart = start - padding;
				const paddedEnd = end + padding;
				const visibleEnd = scrollPos + viewportSize;

				if (paddedStart >= scrollPos && paddedEnd <= visibleEnd) {
					callback?.();
					return;
				}

				let targetScroll: number;
				if (paddedStart < scrollPos || shouldScrollToStart) {
					targetScroll = paddedStart;
				} else {
					targetScroll = paddedEnd - viewportSize;
				}

				this.scrollTo({to: targetScroll, animate, callback});
			},

			scrollIntoViewNode({node: targetNode, shouldScrollToStart = false, padding = 0, animate = false, callback}) {
				const container = scrollRef.current;
				if (!container) {
					callback?.();
					return;
				}

				const offset = measureElementOffset(targetNode, orientation, container);
				const size = orientation === 'vertical' ? targetNode.offsetHeight : targetNode.offsetWidth;

				this.scrollIntoViewRect({
					start: offset,
					end: offset + size,
					shouldScrollToStart,
					padding,
					animate,
					callback,
				});
			},

			scrollPageUp({animate = false, callback} = {}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}

				const scrollPos = getScrollPos(node);
				const viewportSize = orientation === 'vertical' ? node.offsetHeight : node.offsetWidth;
				const target = Math.max(0, scrollPos - 0.9 * viewportSize);

				this.scrollTo({to: target, animate, callback});
			},

			scrollPageDown({animate = false, callback} = {}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}

				const scrollPos = getScrollPos(node);
				const viewportSize = orientation === 'vertical' ? node.offsetHeight : node.offsetWidth;
				const maxScroll = getMaxScroll(node);
				const target = Math.min(maxScroll, scrollPos + 0.9 * viewportSize);

				this.scrollTo({to: target, animate, callback});
			},

			scrollToTop({animate = false, callback} = {}) {
				this.scrollTo({to: 0, animate, callback});
			},

			scrollToBottom({animate = false, callback} = {}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}

				this.scrollTo({to: getMaxScroll(node), animate, callback});
			},

			isScrolledToTop() {
				const node = scrollRef.current;
				if (!node) return false;
				return getScrollPos(node) === 0;
			},

			isScrolledToBottom() {
				const node = scrollRef.current;
				if (!node) return false;
				const scrollPos = getScrollPos(node);
				const maxScroll = getMaxScroll(node);
				return scrollPos >= maxScroll;
			},

			getDistanceFromTop() {
				const node = scrollRef.current;
				if (!node) return 0;
				return Math.max(0, getScrollPos(node));
			},

			getDistanceFromBottom() {
				const node = scrollRef.current;
				if (!node) return 0;
				const scrollPos = getScrollPos(node);
				const maxScroll = getMaxScroll(node);
				return Math.max(0, maxScroll - scrollPos);
			},
		}),
		[orientation, cancelAnimation, clampToRange, getMaxScroll, getScrollPos, setScrollPos, springScrollTo],
	);

	const handleScroll = useCallback(
		(event: UIEvent<HTMLDivElement>) => {
			if (fade) {
				const el = scrollRef.current;
				if (el) {
					el.classList.add(styles.scrolling);
					window.clearTimeout(scrollTimeoutRef.current);
					scrollTimeoutRef.current = window.setTimeout(() => {
						el.classList.remove(styles.scrolling);
					}, 1000);
				}
			}
			onScroll?.(event);
		},
		[fade, onScroll],
	);

	const scrollerStyle: CSSProperties = {
		...style,
		overflowY: orientation === 'vertical' ? overflow : 'hidden',
		overflowX: orientation === 'horizontal' ? overflow : 'hidden',
	};

	const containerClassName = clsx(styles.scrollerWrap, {
		[styles.horizontal]: orientation === 'horizontal',
		[styles.regular]: scrollbar === 'regular',
		[styles.noScrollbarReserve]: !reserveScrollbarTrack,
	});

	const scrollerClassName = clsx(styles.scroller, className, {
		[styles.fade]: fade,
		[styles.regular]: scrollbar === 'regular',
	});

	return (
		<div className={containerClassName} ref={rootRef}>
			<FocusRingScope containerRef={rootRef}>
				{/* biome-ignore lint/a11y/noStaticElementInteractions: this is fine */}
				<div
					ref={scrollRef}
					className={scrollerClassName}
					style={scrollerStyle}
					dir={dir}
					onScroll={handleScroll}
					onKeyDown={onKeyDown}
					onMouseLeave={onMouseLeave}
				>
					<div className={styles.scrollerChildren}>{children}</div>
				</div>
			</FocusRingScope>
		</div>
	);
});

Scroller.displayName = 'Scroller';
