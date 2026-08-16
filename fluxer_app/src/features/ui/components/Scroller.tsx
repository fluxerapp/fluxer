// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/ui/components/Scroller.module.css';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {ScrollerTrack} from '@app/features/ui/scroller/ScrollerTrack';
import {useScrollerThumb} from '@app/features/ui/scroller/useScrollerThumb';
import {
	canUseWindowFocusedHoverControls,
	subscribeWindowHoverControlsChange,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {clsx} from 'clsx';
import {
	type CSSProperties,
	forwardRef,
	type HTMLAttributes,
	type KeyboardEvent as ReactKeyboardEvent,
	type MouseEvent as ReactMouseEvent,
	type ReactNode,
	type PointerEvent as ReactPointerEvent,
	type TouchEvent as ReactTouchEvent,
	type WheelEvent as ReactWheelEvent,
	type UIEvent,
	useCallback,
	useEffect,
	useImperativeHandle,
	useLayoutEffect,
	useRef,
	useState,
} from 'react';

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
	cancelScroll(): void;
	scrollTo(options: {to: number; animate?: boolean; callback?: () => void}): void;
	mergeTo(options: {to: number; callback?: () => void}): void;
	scrollIntoViewRect(options: {
		start: number;
		end: number;
		shouldScrollToStart?: boolean;
		alignment?: 'start' | 'center' | 'end' | 'nearest';
		padding?: number;
		animate?: boolean;
		callback?: () => void;
	}): void;
	scrollIntoViewNode(options: {
		node: HTMLElement;
		shouldScrollToStart?: boolean;
		alignment?: 'start' | 'center' | 'end' | 'nearest';
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
type ScrollbarTrackMode = 'overlay' | 'reserve';
const LINE_DELTA_PX = 16;

interface ScrollerProps
	extends Omit<
		HTMLAttributes<HTMLDivElement>,
		'children' | 'className' | 'style' | 'onScroll' | 'onKeyDown' | 'onMouseLeave' | 'onTouchStart' | 'onWheel'
	> {
	children: ReactNode;
	className?: string;
	contentClassName?: string;
	dir?: 'ltr' | 'rtl';
	orientation?: ScrollAxis;
	overflow?: ScrollOverflow;
	fade?: boolean;
	scrollbar?: 'thin' | 'regular';
	showTrack?: boolean;
	hideThumbWhenWindowBlurred?: boolean;
	scrollbarTrackMode?: ScrollbarTrackMode;
	style?: CSSProperties;
	estimatedContentSize?: number | null;
	onScroll?: (event: UIEvent<HTMLDivElement>) => void;
	onKeyDown?: (event: ReactKeyboardEvent<HTMLDivElement>) => void;
	onMouseLeave?: (event: ReactMouseEvent<HTMLDivElement>) => void;
	onTouchStart?: (event: ReactTouchEvent<HTMLDivElement>) => void;
	onWheel?: (event: ReactWheelEvent<HTMLDivElement>) => void;
	onScrollIntent?: () => void;
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

function getTrackWheelDelta(
	event: ReactWheelEvent<HTMLDivElement>,
	orientation: ScrollAxis,
	viewportSize: number,
): number {
	const raw = orientation === 'vertical' ? event.deltaY : event.deltaX || event.deltaY;
	if (event.deltaMode === WheelEvent.DOM_DELTA_LINE) {
		return raw * LINE_DELTA_PX;
	}
	if (event.deltaMode === WheelEvent.DOM_DELTA_PAGE) {
		return raw * viewportSize;
	}
	return raw;
}

function measureElementOffset(element: HTMLElement, axis: ScrollAxis, container: HTMLElement): number {
	const elementRect = element.getBoundingClientRect();
	const containerRect = container.getBoundingClientRect();
	const scrollPos = axis === 'horizontal' ? container.scrollLeft : container.scrollTop;
	const delta = axis === 'horizontal' ? elementRect.left - containerRect.left : elementRect.top - containerRect.top;
	return scrollPos + delta;
}

interface SpringConfig {
	stiffness: number;
	damping: number;
	mass: number;
	precision: number;
	maxDurationMs: number;
}

const DEFAULT_SPRING: SpringConfig = {
	stiffness: 520,
	damping: 70,
	mass: 1,
	precision: 0.5,
	maxDurationMs: 1400,
};
const KEYBOARD_SCROLL_IGNORED_TARGET_SELECTOR = [
	'a[href]',
	'input',
	'textarea',
	'select',
	'[contenteditable=""]',
	'[contenteditable="true"]',
	'button',
	'[role="button"]',
].join(',');
const KEYBOARD_SCROLL_KEYS = new Set([
	'ArrowDown',
	'ArrowLeft',
	'ArrowRight',
	'ArrowUp',
	'End',
	'Home',
	'PageDown',
	'PageUp',
	' ',
	'Space',
	'Spacebar',
]);

function shouldAnimateScroll(): boolean {
	return Accessibility.useSmoothScrolling;
}

function targetMatches(target: EventTarget | null, selector: string): boolean {
	return target instanceof Element && target.closest(selector) !== null;
}

function shouldTreatKeyAsScrollIntent(event: ReactKeyboardEvent<HTMLDivElement>): boolean {
	if (!KEYBOARD_SCROLL_KEYS.has(event.key)) {
		return false;
	}
	return !targetMatches(event.target, KEYBOARD_SCROLL_IGNORED_TARGET_SELECTOR);
}

export const Scroller = forwardRef<ScrollerHandle, ScrollerProps>(function Scroller(
	{
		children,
		className,
		contentClassName,
		dir = 'ltr',
		orientation = 'vertical',
		overflow = 'auto',
		fade = true,
		scrollbar = 'thin',
		showTrack = true,
		hideThumbWhenWindowBlurred = false,
		scrollbarTrackMode = 'overlay',
		style,
		estimatedContentSize,
		onScroll,
		onKeyDown,
		onMouseLeave,
		onTouchStart,
		onWheel,
		onScrollIntent,
		onResize,
		...rest
	},
	forwardedRef,
) {
	const scrollRef = useRef<HTMLDivElement>(null);
	const contentRef = useRef<HTMLDivElement>(null);
	const rootRef = useRef<HTMLDivElement>(null);
	const scrollTimeoutRef = useRef<number>(0);
	const thumbRefreshRafRef = useRef<number | null>(null);
	const onResizeRef = useRef(onResize);
	const [isHovered, setIsHovered] = useState(false);
	const [isScrolling, setIsScrolling] = useState(false);
	const [isWindowBlurred, setIsWindowBlurred] = useState(false);
	const minThumbSize = scrollbar === 'regular' ? 40 : 24;
	const {
		isDragging,
		refreshThumbState,
		thumbState,
		setTrackElement,
		setThumbElement,
		onThumbPointerDown,
		onTrackPointerDown,
	} = useScrollerThumb({
		orientation,
		overflow,
		minThumbSize,
		scrollRef,
		showTrack,
	});
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
	const cancelProgrammaticScrollForUserIntent = useCallback(() => {
		cancelAnimation();
		onScrollIntent?.();
	}, [cancelAnimation, onScrollIntent]);
	const cancelScheduledThumbRefresh = useCallback(() => {
		if (thumbRefreshRafRef.current != null) {
			cancelAnimationFrame(thumbRefreshRafRef.current);
			thumbRefreshRafRef.current = null;
		}
	}, []);
	const scheduleThumbRefresh = useCallback(() => {
		if (!showTrack) {
			return;
		}
		if (thumbRefreshRafRef.current != null) {
			return;
		}
		thumbRefreshRafRef.current = requestAnimationFrame(() => {
			thumbRefreshRafRef.current = null;
			refreshThumbState();
		});
	}, [refreshThumbState, showTrack]);
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
			refreshThumbState();
		},
		[orientation, refreshThumbState],
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
	useLayoutEffect(() => {
		const containerEl = rootRef.current;
		const contentEl = contentRef.current;
		if (!containerEl || !contentEl) return;
		const ownerWindow = containerEl.ownerDocument.defaultView ?? window;
		const containerObserver = new ownerWindow.ResizeObserver((entries) => {
			scheduleThumbRefresh();
			for (const entry of entries) {
				onResizeRef.current?.(entry, 'container');
			}
		});
		const contentObserver = new ownerWindow.ResizeObserver((entries) => {
			scheduleThumbRefresh();
			for (const entry of entries) {
				onResizeRef.current?.(entry, 'content');
			}
		});
		const childObserver = new ownerWindow.ResizeObserver((entries) => {
			scheduleThumbRefresh();
			for (const entry of entries) {
				onResizeRef.current?.(entry, 'content');
			}
		});
		const observedChildren = new Set<Element>();
		const syncChildObservers = () => {
			const currentChildren = new Set<Element>(Array.from(contentEl.children));
			for (const child of observedChildren) {
				if (!currentChildren.has(child)) {
					childObserver.unobserve(child);
					observedChildren.delete(child);
				}
			}
			for (const child of currentChildren) {
				if (!observedChildren.has(child)) {
					childObserver.observe(child);
					observedChildren.add(child);
				}
			}
		};
		let childSyncRafId: number | null = null;
		const childListObserver = new MutationObserver(() => {
			if (childSyncRafId != null) {
				return;
			}
			childSyncRafId = requestAnimationFrame(() => {
				childSyncRafId = null;
				syncChildObservers();
				refreshThumbState();
			});
		});
		containerObserver.observe(containerEl);
		contentObserver.observe(contentEl);
		syncChildObservers();
		childListObserver.observe(contentEl, {childList: true, subtree: true});
		refreshThumbState();
		return () => {
			cancelScheduledThumbRefresh();
			if (childSyncRafId != null) {
				cancelAnimationFrame(childSyncRafId);
				childSyncRafId = null;
			}
			containerObserver.disconnect();
			contentObserver.disconnect();
			childObserver.disconnect();
			childListObserver.disconnect();
			observedChildren.clear();
		};
	}, [cancelScheduledThumbRefresh, refreshThumbState, scheduleThumbRefresh]);
	useEffect(() => {
		refreshThumbState();
		scheduleThumbRefresh();
		return cancelScheduledThumbRefresh;
	}, [cancelScheduledThumbRefresh, refreshThumbState, scheduleThumbRefresh]);
	useEffect(() => {
		return () => cancelAnimation();
	}, [cancelAnimation]);
	useEffect(() => {
		return () => {
			window.clearTimeout(scrollTimeoutRef.current);
		};
	}, []);
	useEffect(() => {
		return cancelScheduledThumbRefresh;
	}, [cancelScheduledThumbRefresh]);
	useEffect(() => {
		if (!hideThumbWhenWindowBlurred) {
			setIsWindowBlurred(false);
			return;
		}
		const update = () => setIsWindowBlurred(!document.hasFocus());
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
			cancelScroll() {
				cancelAnimation();
			},
			scrollTo({to, animate = false, callback}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}
				const clampedTo = clampToRange(node, to);
				if (!animate || !shouldAnimateScroll()) {
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
			scrollIntoViewRect({start, end, shouldScrollToStart = false, alignment, padding = 0, animate = false, callback}) {
				const node = scrollRef.current;
				if (!node) {
					callback?.();
					return;
				}
				const scrollPos = getScrollPos(node);
				const viewportSize = orientation === 'vertical' ? node.offsetHeight : node.offsetWidth;
				const resolvedAlignment = alignment ?? (shouldScrollToStart ? 'start' : 'nearest');
				if (resolvedAlignment === 'center') {
					const targetScroll = start - (viewportSize - (end - start)) / 2;
					this.scrollTo({to: targetScroll, animate, callback});
					return;
				}
				if (resolvedAlignment === 'start') {
					this.scrollTo({to: start - padding, animate, callback});
					return;
				}
				if (resolvedAlignment === 'end') {
					this.scrollTo({to: end + padding - viewportSize, animate, callback});
					return;
				}
				const paddedStart = start - padding;
				const paddedEnd = end + padding;
				const visibleEnd = scrollPos + viewportSize;
				if (paddedStart >= scrollPos && paddedEnd <= visibleEnd) {
					callback?.();
					return;
				}
				let targetScroll: number;
				if (paddedStart < scrollPos) {
					targetScroll = paddedStart;
				} else {
					targetScroll = paddedEnd - viewportSize;
				}
				this.scrollTo({to: targetScroll, animate, callback});
			},
			scrollIntoViewNode({
				node: targetNode,
				shouldScrollToStart = false,
				alignment,
				padding = 0,
				animate = false,
				callback,
			}) {
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
					alignment,
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
			if (fade && showTrack) {
				setIsScrolling(true);
				window.clearTimeout(scrollTimeoutRef.current);
				scrollTimeoutRef.current = window.setTimeout(() => {
					setIsScrolling(false);
				}, 1000);
			}
			scheduleThumbRefresh();
			onScroll?.(event);
		},
		[fade, onScroll, scheduleThumbRefresh, showTrack],
	);
	const handleScrollerWheel = useCallback(
		(event: ReactWheelEvent<HTMLDivElement>) => {
			cancelProgrammaticScrollForUserIntent();
			onWheel?.(event);
		},
		[cancelProgrammaticScrollForUserIntent, onWheel],
	);
	const handleScrollerTouchStart = useCallback(
		(event: ReactTouchEvent<HTMLDivElement>) => {
			cancelProgrammaticScrollForUserIntent();
			onTouchStart?.(event);
		},
		[cancelProgrammaticScrollForUserIntent, onTouchStart],
	);
	const handleScrollerKeyDown = useCallback(
		(event: ReactKeyboardEvent<HTMLDivElement>) => {
			if (shouldTreatKeyAsScrollIntent(event)) {
				cancelProgrammaticScrollForUserIntent();
			}
			onKeyDown?.(event);
		},
		[cancelProgrammaticScrollForUserIntent, onKeyDown],
	);
	const handleTrackWheel = useCallback(
		(event: ReactWheelEvent<HTMLDivElement>) => {
			const scrollElement = scrollRef.current;
			if (!scrollElement) return;
			cancelProgrammaticScrollForUserIntent();
			const viewportSize = orientation === 'vertical' ? scrollElement.clientHeight : scrollElement.clientWidth;
			const delta = getTrackWheelDelta(event, orientation, viewportSize);
			if (delta === 0) {
				return;
			}
			if (event.cancelable) {
				event.preventDefault();
			}
			setScrollPos(scrollElement, getScrollPos(scrollElement) + delta);
		},
		[cancelProgrammaticScrollForUserIntent, getScrollPos, orientation, scrollRef, setScrollPos],
	);
	const handleTrackPointerDown = useCallback(
		(event: ReactPointerEvent<HTMLDivElement>) => {
			cancelProgrammaticScrollForUserIntent();
			onTrackPointerDown(event);
		},
		[cancelProgrammaticScrollForUserIntent, onTrackPointerDown],
	);
	const handleThumbPointerDown = useCallback(
		(event: ReactPointerEvent<HTMLDivElement>) => {
			cancelProgrammaticScrollForUserIntent();
			onThumbPointerDown(event);
		},
		[cancelProgrammaticScrollForUserIntent, onThumbPointerDown],
	);
	const handleMouseEnter = useCallback(() => {
		setIsHovered(canUseWindowFocusedHoverControls());
	}, []);
	const handleMouseLeaveInternal = useCallback(
		(event: ReactMouseEvent<HTMLDivElement>) => {
			setIsHovered(false);
			onMouseLeave?.(event);
		},
		[onMouseLeave],
	);
	useEffect(() => {
		const syncHoverWithWindowFocus = () => {
			setIsHovered(canUseWindowFocusedHoverControls() && Boolean(rootRef.current?.matches(':hover')));
		};
		const unsubscribe = subscribeWindowHoverControlsChange(syncHoverWithWindowFocus);
		window.addEventListener('blur', syncHoverWithWindowFocus);
		return () => {
			unsubscribe();
			window.removeEventListener('blur', syncHoverWithWindowFocus);
		};
	}, []);
	const scrollerStyle: CSSProperties = {
		...style,
		overflowY: orientation === 'vertical' ? overflow : 'hidden',
		overflowX: orientation === 'horizontal' ? overflow : 'hidden',
	};
	const normalizedEstimatedContentSize =
		estimatedContentSize != null && Number.isFinite(estimatedContentSize)
			? Math.max(0, Math.floor(estimatedContentSize))
			: null;
	const contentStyle: CSSProperties | undefined =
		normalizedEstimatedContentSize != null
			? orientation === 'vertical'
				? {minHeight: `${normalizedEstimatedContentSize}px`}
				: {minWidth: `${normalizedEstimatedContentSize}px`}
			: undefined;
	const shouldReserveTrackSpace = scrollbarTrackMode === 'reserve';
	const containerClassName = clsx(styles.scrollerWrap, {
		[styles.horizontal]: orientation === 'horizontal',
		[styles.regular]: scrollbar === 'regular',
		[styles.scrollbarReserve]: shouldReserveTrackSpace,
	});
	const scrollerClassName = clsx(styles.scroller, className, {
		[styles.regular]: scrollbar === 'regular',
		[styles.dragging]: isDragging,
	});
	const hasTrack = showTrack && thumbState.hasTrack;
	const isTrackVisible =
		hasTrack && (!hideThumbWhenWindowBlurred || !isWindowBlurred) && (!fade || isHovered || isScrolling || isDragging);
	return (
		<div
			role="group"
			className={containerClassName}
			ref={rootRef}
			onMouseEnter={handleMouseEnter}
			onMouseLeave={handleMouseLeaveInternal}
			data-flx="ui.scroller.group"
		>
			<FocusRingScope containerRef={rootRef} data-flx="ui.scroller.focus-ring-scope">
				<div
					ref={scrollRef}
					role="group"
					data-fluxer-scroll-container="true"
					className={scrollerClassName}
					style={scrollerStyle}
					dir={dir}
					onScroll={handleScroll}
					onKeyDown={handleScrollerKeyDown}
					onTouchStart={handleScrollerTouchStart}
					onWheel={handleScrollerWheel}
					tabIndex={-1}
					data-flx="ui.scroller.group.scroller-key-down"
					{...rest}
				>
					<div
						className={clsx(styles.scrollerChildren, contentClassName)}
						ref={contentRef}
						style={contentStyle}
						data-scroller-has-track={hasTrack ? 'true' : undefined}
						data-flx="ui.scroller.scroller-children"
					>
						{children}
					</div>
				</div>
				<ScrollerTrack
					orientation={orientation}
					scrollbar={scrollbar}
					hasTrack={hasTrack}
					isVisible={isTrackVisible}
					isDragging={isDragging}
					trackRef={setTrackElement}
					thumbRef={setThumbElement}
					onTrackPointerDown={handleTrackPointerDown}
					onThumbPointerDown={handleThumbPointerDown}
					onWheel={handleTrackWheel}
					data-flx="ui.scroller.scroller-track"
				/>
			</FocusRingScope>
		</div>
	);
});

Scroller.displayName = 'Scroller';
