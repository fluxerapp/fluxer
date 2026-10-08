// SPDX-License-Identifier: AGPL-3.0-or-later

import {useBottomSheetBackHandler} from '@app/features/app/hooks/useBottomSheetBackHandler';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useAccountTransitionDismissal} from '@app/features/platform/hooks/useAccountTransitionDismissal';
import {resolvePortalHost, usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import styles from '@app/features/ui/sheet/Sheet.module.css';
import OverlayStack from '@app/features/ui/state/OverlayStack';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {
	createContext,
	useCallback,
	useContext,
	useEffect,
	useId,
	useLayoutEffect,
	useMemo,
	useRef,
	useState,
} from 'react';
import {createPortal} from 'react-dom';

const BOTTOM_SHEET_DESCRIPTOR = msg({
	message: 'Bottom sheet',
	comment: 'Accessible label for the bottom-sheet container element.',
});
const SHEET_ANIMATION_MS = 220;
const DRAG_START_THRESHOLD_PX = 6;
const HIGH_VELOCITY_THRESHOLD = 850;
const LOW_VELOCITY_THRESHOLD = 80;
const CLOSE_SNAP_INDEX = 0;
const DEFAULT_MODAL_EFFECT_ROOT_ID = 'root';
const INTERACTIVE_DRAG_IGNORE_SELECTOR = [
	'input',
	'textarea',
	'select',
	'option',
	'[contenteditable=""]',
	'[contenteditable="true"]',
	'[role="slider"]',
	'[data-sheet-drag-disabled="true"]',
].join(',');

type Surface = 'primary' | 'secondary' | 'tertiary';
type SheetState = 'open' | 'closing';
type DragMode = 'pending' | 'sheet' | 'scroll' | 'ignore';

interface RootProps {
	isOpen: boolean;
	onClose: () => void;
	children: React.ReactNode;
	initialSnap?: number;
	snapPoints?: Array<number>;
	surface?: Surface;
	showHandle?: boolean;
	zIndex?: number;
	modalEffectRootId?: string;
	backdropOpacity?: number;
	showBackdrop?: boolean;
	className?: string;
	role?: 'dialog' | 'alertdialog';
	ariaLabel?: string;
	ariaLabelledBy?: string;
	ariaDescribedBy?: string;
}

interface SheetA11yContextValue {
	titleId: string;
	setHasTitle: (hasTitle: boolean) => void;
}

interface ComputedSnapPoint {
	index: number;
	value: number;
	y: number;
}

interface DragState {
	identifier: number | null;
	mode: DragMode;
	startX: number;
	startY: number;
	lastX: number;
	lastY: number;
	lastTime: number;
	velocityY: number;
	scrollTarget: HTMLElement | null;
	didDragSheet: boolean;
}

const SheetA11yContext = createContext<SheetA11yContextValue | null>(null);
const surfaceClassMap: Record<Surface, string> = {
	primary: styles.surfacePrimary,
	secondary: styles.surfaceSecondary,
	tertiary: styles.surfaceTertiary,
};
const useIsomorphicLayoutEffect = typeof window === 'undefined' ? useEffect : useLayoutEffect;

let scrollLockCount = 0;
let scrollLockState: {
	bodyOverflow: string;
	bodyPosition: string;
	bodyTop: string;
	bodyWidth: string;
	documentOverflow: string;
	documentOverscrollBehavior: string;
	scrollY: number;
} | null = null;

function clamp(value: number, min: number, max: number): number {
	return Math.min(max, Math.max(min, value));
}

function getViewportHeight(): number {
	if (typeof window === 'undefined') return 0;
	return window.visualViewport?.height ?? window.innerHeight;
}

function getViewportOffsetBottom(): number {
	if (typeof window === 'undefined' || !window.visualViewport) return 0;
	const viewport = window.visualViewport;
	return Math.max(0, window.innerHeight - viewport.height - viewport.offsetTop);
}

function getTransitionDuration(enabled: boolean): string {
	if (!enabled) return '0ms';
	if (typeof window !== 'undefined' && window.matchMedia?.('(prefers-reduced-motion: reduce)').matches) {
		return '1ms';
	}
	return `${SHEET_ANIMATION_MS}ms`;
}

function normalizeSnapPoints(snapPoints: Array<number>): Array<number> {
	const normalized = [...snapPoints];
	if (normalized.length === 0) {
		return [0, 1];
	}
	if (normalized[0] !== 0) {
		normalized.unshift(0);
	}
	if (normalized[normalized.length - 1] !== 1) {
		normalized.push(1);
	}
	return normalized;
}

function computeSnapPoints(snapPoints: Array<number>, sheetHeight: number): Array<ComputedSnapPoint> {
	const normalized = normalizeSnapPoints(snapPoints);
	return normalized.map((point, index) => {
		let value = point;
		if (point > 0 && point <= 1) {
			value = point * sheetHeight;
		} else if (point < 0) {
			value = sheetHeight + point;
		}
		const clampedValue = clamp(value, 0, sheetHeight);
		return {
			index,
			value: clampedValue,
			y: sheetHeight - clampedValue,
		};
	});
}

function resolveSnapIndex(index: number, snapPoints: Array<ComputedSnapPoint>): number {
	if (snapPoints.length === 0) return CLOSE_SNAP_INDEX;
	return clamp(index, CLOSE_SNAP_INDEX, snapPoints.length - 1);
}

function findClosestSnapPoint(y: number, snapPoints: Array<ComputedSnapPoint>): ComputedSnapPoint {
	return snapPoints.reduce((closest, snapPoint) =>
		Math.abs(snapPoint.y - y) < Math.abs(closest.y - y) ? snapPoint : closest,
	);
}

function findSnapPointInDirection(
	y: number,
	direction: 'up' | 'down',
	snapPoints: Array<ComputedSnapPoint>,
): ComputedSnapPoint | undefined {
	if (direction === 'up') {
		return snapPoints.find((snapPoint) => snapPoint.y < y - 1);
	}
	return snapPoints
		.slice()
		.reverse()
		.find((snapPoint) => snapPoint.y > y + 1);
}

function getScrollTop(element: HTMLElement): number {
	return Math.max(0, element.scrollTop);
}

function getMaxScrollTop(element: HTMLElement): number {
	return Math.max(0, element.scrollHeight - element.clientHeight);
}

function isScrollableElement(element: HTMLElement): boolean {
	if (element.dataset.sheetDragDisabled === 'true') return false;
	if (element.dataset.fluxerScrollContainer === 'true') {
		return getMaxScrollTop(element) > 1;
	}
	const style = window.getComputedStyle(element);
	if (!/(auto|scroll|overlay)/.test(style.overflowY)) return false;
	return getMaxScrollTop(element) > 1;
}

function findScrollableTarget(target: EventTarget | null, boundary: HTMLElement): HTMLElement | null {
	if (!(target instanceof Element)) return null;
	let current: Element | null = target;
	while (current && current !== boundary) {
		if (current instanceof HTMLElement && isScrollableElement(current)) {
			return current;
		}
		current = current.parentElement;
	}
	return null;
}

function shouldIgnoreDragStart(target: EventTarget | null): boolean {
	return target instanceof Element && target.closest(INTERACTIVE_DRAG_IGNORE_SELECTOR) !== null;
}

function isEventTargetInsideContainer(target: EventTarget | null, container: HTMLElement): boolean {
	return target instanceof Node && container.contains(target);
}

function lockDocumentScroll(): () => void {
	if (typeof document === 'undefined') {
		return () => undefined;
	}
	scrollLockCount += 1;
	if (scrollLockCount === 1) {
		const body = document.body;
		const documentElement = document.documentElement;
		scrollLockState = {
			bodyOverflow: body.style.overflow,
			bodyPosition: body.style.position,
			bodyTop: body.style.top,
			bodyWidth: body.style.width,
			documentOverflow: documentElement.style.overflow,
			documentOverscrollBehavior: documentElement.style.overscrollBehavior,
			scrollY: window.scrollY,
		};
		body.style.overflow = 'hidden';
		body.style.position = 'fixed';
		body.style.top = `-${scrollLockState.scrollY}px`;
		body.style.width = '100%';
		documentElement.style.overflow = 'hidden';
		documentElement.style.overscrollBehavior = 'none';
	}
	return () => {
		scrollLockCount = Math.max(0, scrollLockCount - 1);
		if (scrollLockCount !== 0 || !scrollLockState) return;
		const body = document.body;
		const documentElement = document.documentElement;
		const previousScrollY = scrollLockState.scrollY;
		body.style.overflow = scrollLockState.bodyOverflow;
		body.style.position = scrollLockState.bodyPosition;
		body.style.top = scrollLockState.bodyTop;
		body.style.width = scrollLockState.bodyWidth;
		documentElement.style.overflow = scrollLockState.documentOverflow;
		documentElement.style.overscrollBehavior = scrollLockState.documentOverscrollBehavior;
		scrollLockState = null;
		window.scrollTo(0, previousScrollY);
	};
}

function cleanupModalEffect(rootId: string | undefined): void {
	if (!rootId || typeof document === 'undefined') return;
	const root = document.getElementById(rootId);
	if (!root) return;
	root.style.transform = '';
	root.style.borderTopLeftRadius = '';
	root.style.borderTopRightRadius = '';
	root.style.overflow = '';
	root.style.transition = '';
}

const RootComponent: React.FC<RootProps> = ({
	isOpen,
	onClose,
	children,
	initialSnap = 1,
	snapPoints = [0, 0.6, 1],
	surface = 'secondary',
	zIndex: explicitZIndex,
	modalEffectRootId = DEFAULT_MODAL_EFFECT_ROOT_ID,
	backdropOpacity = 0.7,
	showBackdrop = true,
	className,
	role = 'dialog',
	ariaLabel,
	ariaLabelledBy,
	ariaDescribedBy,
}) => {
	const {i18n} = useLingui();
	const [isMounted, setIsMounted] = useState(isOpen);
	const [sheetState, setSheetState] = useState<SheetState>(isOpen ? 'open' : 'closing');
	const sheetPortalHost = usePortalHost();
	const [acquiredZIndex, setAcquiredZIndex] = useState<number | null>(null);
	const rootRef = useRef<HTMLDivElement>(null);
	const containerRef = useRef<HTMLDivElement>(null);
	const closeTimerRef = useRef<number | null>(null);
	const dragStateRef = useRef<DragState | null>(null);
	const sheetHeightRef = useRef(0);
	const sheetYRef = useRef(0);
	const snapPointsRef = useRef<Array<ComputedSnapPoint>>([]);
	const currentSnapRef = useRef(initialSnap);
	const suppressClickUntilRef = useRef(0);
	const previousFocusRef = useRef<HTMLElement | null>(null);
	const onCloseRef = useRef(onClose);
	const isOpenRef = useRef(isOpen);
	const initialSnapRef = useRef(initialSnap);
	const snapPointsPropRef = useRef(snapPoints);
	const backdropOpacityRef = useRef(backdropOpacity);
	const modalEffectRootIdRef = useRef(modalEffectRootId);
	const titleId = useId();
	const [hasTitle, setHasTitle] = useState(false);
	const a11yContext = useMemo(() => ({titleId, setHasTitle}), [titleId]);
	const zIndex = explicitZIndex ?? acquiredZIndex ?? OverlayStack.peek();
	const labelledBy = ariaLabelledBy ?? (hasTitle ? titleId : undefined);
	const resolvedAriaLabel = labelledBy ? ariaLabel : (ariaLabel ?? i18n._(BOTTOM_SHEET_DESCRIPTOR));
	const snapPointsKey = snapPoints.join('|');
	onCloseRef.current = onClose;
	isOpenRef.current = isOpen;
	initialSnapRef.current = initialSnap;
	snapPointsPropRef.current = snapPoints;
	backdropOpacityRef.current = backdropOpacity;
	modalEffectRootIdRef.current = modalEffectRootId;
	const applyModalEffect = useCallback((y: number, sheetHeight: number) => {
		const rootId = modalEffectRootIdRef.current;
		if (!rootId || typeof document === 'undefined' || sheetHeight <= 0) return;
		const modalRoot = document.getElementById(rootId);
		if (!modalRoot) return;
		const snapThresholdPoint =
			snapPointsRef.current.length > 1 ? snapPointsRef.current[snapPointsRef.current.length - 2] : null;
		let progress = 1 - y / sheetHeight;
		if (snapThresholdPoint) {
			const thresholdY = snapThresholdPoint.y;
			progress = thresholdY > 0 ? (thresholdY - y) / thresholdY : 1 - y / sheetHeight;
		}
		const clampedProgress = clamp(progress, 0, 1);
		if (clampedProgress <= 0) {
			cleanupModalEffect(rootId);
			return;
		}
		const pageWidth = Math.max(1, window.innerWidth);
		const scale = 1 - (8 / pageWidth) * clampedProgress;
		const translateY = 24 * clampedProgress;
		const borderRadius = 10 * clampedProgress;
		const transitionDuration =
			rootRef.current?.style.getPropertyValue('--sheet-transition-duration') || `${SHEET_ANIMATION_MS}ms`;
		modalRoot.style.transition = [
			`transform ${transitionDuration} cubic-bezier(0.22, 1, 0.36, 1)`,
			`border-radius ${transitionDuration} cubic-bezier(0.22, 1, 0.36, 1)`,
		].join(', ');
		modalRoot.style.transform = `translate3d(0, ${translateY}px, 0) scale(${scale})`;
		modalRoot.style.borderTopLeftRadius = `${borderRadius}px`;
		modalRoot.style.borderTopRightRadius = `${borderRadius}px`;
		modalRoot.style.overflow = 'hidden';
	}, []);
	const setTransitionEnabled = useCallback((enabled: boolean) => {
		rootRef.current?.style.setProperty('--sheet-transition-duration', getTransitionDuration(enabled));
	}, []);
	const setSheetY = useCallback(
		(nextY: number) => {
			const root = rootRef.current;
			if (!root) return;
			const sheetHeight = Math.max(1, sheetHeightRef.current || getViewportHeight());
			const y = clamp(nextY, 0, sheetHeight);
			const progress = clamp(1 - y / sheetHeight, 0, 1);
			sheetYRef.current = y;
			root.style.setProperty('--sheet-y', `${y}px`);
			root.style.setProperty('--sheet-open-progress', `${progress}`);
			root.style.setProperty('--sheet-backdrop-alpha', `${progress * backdropOpacityRef.current}`);
			applyModalEffect(y, sheetHeight);
		},
		[applyModalEffect],
	);
	const syncMeasurements = useCallback(
		({snapToCurrent = false}: {snapToCurrent?: boolean} = {}) => {
			const root = rootRef.current;
			const container = containerRef.current;
			if (!root || !container) return;
			const viewportHeight = getViewportHeight();
			root.style.setProperty('--sheet-viewport-height', `${viewportHeight}px`);
			root.style.setProperty('--sheet-viewport-offset-bottom', `${getViewportOffsetBottom()}px`);
			const sheetHeight = Math.max(1, Math.round(container.getBoundingClientRect().height));
			sheetHeightRef.current = sheetHeight;
			snapPointsRef.current = computeSnapPoints(snapPointsPropRef.current, sheetHeight);
			if (!snapToCurrent || dragStateRef.current?.didDragSheet) return;
			const snapIndex = resolveSnapIndex(currentSnapRef.current ?? initialSnapRef.current, snapPointsRef.current);
			const targetY = isOpenRef.current ? (snapPointsRef.current[snapIndex]?.y ?? 0) : sheetHeight;
			setSheetY(targetY);
		},
		[setSheetY],
	);
	const snapToIndex = useCallback(
		(index: number, {immediate = false}: {immediate?: boolean} = {}) => {
			syncMeasurements();
			const snapIndex = resolveSnapIndex(index, snapPointsRef.current);
			const snapPoint = snapPointsRef.current[snapIndex];
			if (!snapPoint) return;
			currentSnapRef.current = snapIndex;
			if (immediate) {
				setTransitionEnabled(false);
				setSheetY(snapPoint.y);
				window.requestAnimationFrame(() => setTransitionEnabled(true));
				return;
			}
			setTransitionEnabled(true);
			setSheetY(snapPoint.y);
		},
		[setSheetY, setTransitionEnabled, syncMeasurements],
	);
	const animateClosed = useCallback(() => {
		syncMeasurements();
		currentSnapRef.current = CLOSE_SNAP_INDEX;
		setTransitionEnabled(true);
		setSheetY(sheetHeightRef.current || getViewportHeight());
	}, [setSheetY, setTransitionEnabled, syncMeasurements]);
	const getGestureTargetSnap = useCallback((velocityY: number): ComputedSnapPoint | null => {
		const snapPointsForGesture = snapPointsRef.current;
		if (snapPointsForGesture.length === 0) return null;
		const currentY = sheetYRef.current;
		if (Math.abs(velocityY) >= HIGH_VELOCITY_THRESHOLD) {
			return velocityY > 0
				? snapPointsForGesture[CLOSE_SNAP_INDEX]
				: snapPointsForGesture[snapPointsForGesture.length - 1];
		}
		if (Math.abs(velocityY) >= LOW_VELOCITY_THRESHOLD) {
			const nextSnap = findSnapPointInDirection(currentY, velocityY > 0 ? 'down' : 'up', snapPointsForGesture);
			if (nextSnap) return nextSnap;
		}
		return findClosestSnapPoint(currentY, snapPointsForGesture);
	}, []);
	const startSheetDrag = useCallback(() => {
		if (dragStateRef.current?.didDragSheet) return;
		dragStateRef.current = dragStateRef.current ? {...dragStateRef.current, didDragSheet: true, mode: 'sheet'} : null;
		const focusedElement = document.activeElement;
		if (focusedElement instanceof HTMLElement && containerRef.current?.contains(focusedElement)) {
			focusedElement.blur();
		}
		rootRef.current?.setAttribute('data-dragging', 'true');
		setTransitionEnabled(false);
	}, [setTransitionEnabled]);
	const finishSheetDrag = useCallback(() => {
		const dragState = dragStateRef.current;
		rootRef.current?.removeAttribute('data-dragging');
		if (!dragState?.didDragSheet) return;
		const targetSnap = getGestureTargetSnap(dragState.velocityY);
		if (!targetSnap || targetSnap.index === CLOSE_SNAP_INDEX) {
			animateClosed();
			onCloseRef.current();
			return;
		}
		snapToIndex(targetSnap.index);
	}, [animateClosed, getGestureTargetSnap, snapToIndex]);
	const applyDragDelta = useCallback(
		(deltaY: number, scrollTarget: HTMLElement | null) => {
			let remainingDeltaY = deltaY;
			if (remainingDeltaY > 0 && scrollTarget && getScrollTop(scrollTarget) > 0) {
				const previousScrollTop = scrollTarget.scrollTop;
				scrollTarget.scrollTop = clamp(previousScrollTop - remainingDeltaY, 0, getMaxScrollTop(scrollTarget));
				remainingDeltaY -= previousScrollTop - scrollTarget.scrollTop;
				if (remainingDeltaY <= 0) return;
			}
			const sheetHeight = sheetHeightRef.current || getViewportHeight();
			const previousY = sheetYRef.current;
			const nextY = clamp(previousY + remainingDeltaY, 0, sheetHeight);
			const consumedY = nextY - previousY;
			setSheetY(nextY);
			const leftoverY = remainingDeltaY - consumedY;
			if (leftoverY < 0 && scrollTarget) {
				scrollTarget.scrollTop = clamp(scrollTarget.scrollTop - leftoverY, 0, getMaxScrollTop(scrollTarget));
			}
		},
		[setSheetY],
	);
	const shouldDragSheetForDelta = useCallback((deltaY: number, scrollTarget: HTMLElement | null): boolean => {
		const currentY = sheetYRef.current;
		if (deltaY < 0 && currentY > 1) return true;
		if (deltaY > 0 && currentY < (sheetHeightRef.current || getViewportHeight()) - 1) {
			return !scrollTarget || getScrollTop(scrollTarget) <= 0;
		}
		return false;
	}, []);
	const handleTouchMove = useCallback(
		(event: TouchEvent) => {
			const dragState = dragStateRef.current;
			if (!dragState) return;
			const touch = Array.from(event.changedTouches).find((item) => item.identifier === dragState.identifier);
			if (!touch) return;
			const now = performance.now();
			const totalX = touch.clientX - dragState.startX;
			const totalY = touch.clientY - dragState.startY;
			const deltaY = touch.clientY - dragState.lastY;
			dragState.velocityY = (deltaY / Math.max(1, now - dragState.lastTime)) * 1000;
			dragState.lastX = touch.clientX;
			dragState.lastY = touch.clientY;
			dragState.lastTime = now;
			if (dragState.mode === 'pending') {
				if (Math.abs(totalY) < DRAG_START_THRESHOLD_PX && Math.abs(totalX) < DRAG_START_THRESHOLD_PX) return;
				if (Math.abs(totalX) > Math.abs(totalY) * 1.15) {
					dragState.mode = 'ignore';
					return;
				}
				dragState.mode = shouldDragSheetForDelta(totalY, dragState.scrollTarget) ? 'sheet' : 'scroll';
			}
			if (
				dragState.mode === 'scroll' &&
				deltaY > 0 &&
				dragState.scrollTarget &&
				getScrollTop(dragState.scrollTarget) <= 0 &&
				sheetYRef.current < (sheetHeightRef.current || getViewportHeight()) - 1
			) {
				dragState.mode = 'sheet';
			}
			if (dragState.mode !== 'sheet') return;
			event.preventDefault();
			startSheetDrag();
			applyDragDelta(deltaY, dragState.scrollTarget);
		},
		[applyDragDelta, shouldDragSheetForDelta, startSheetDrag],
	);
	const endTouchDrag = useCallback(() => {
		finishSheetDrag();
		if (dragStateRef.current?.didDragSheet) {
			suppressClickUntilRef.current = Date.now() + 350;
		}
		dragStateRef.current = null;
		document.removeEventListener('touchmove', handleTouchMove);
		document.removeEventListener('touchend', endTouchDrag);
		document.removeEventListener('touchcancel', endTouchDrag);
	}, [finishSheetDrag, handleTouchMove]);
	const handleTouchStart = useCallback(
		(event: React.TouchEvent<HTMLDivElement>) => {
			if (
				!isOpenRef.current ||
				event.touches.length !== 1 ||
				!containerRef.current ||
				!isEventTargetInsideContainer(event.target, containerRef.current) ||
				shouldIgnoreDragStart(event.target)
			) {
				return;
			}
			const touch = event.touches[0];
			dragStateRef.current = {
				identifier: touch.identifier,
				mode: 'pending',
				startX: touch.clientX,
				startY: touch.clientY,
				lastX: touch.clientX,
				lastY: touch.clientY,
				lastTime: performance.now(),
				velocityY: 0,
				scrollTarget: findScrollableTarget(event.target, containerRef.current),
				didDragSheet: false,
			};
			document.addEventListener('touchmove', handleTouchMove, {passive: false});
			document.addEventListener('touchend', endTouchDrag);
			document.addEventListener('touchcancel', endTouchDrag);
		},
		[endTouchDrag, handleTouchMove],
	);
	const handlePointerMove = useCallback(
		(event: PointerEvent) => {
			const dragState = dragStateRef.current;
			if (!dragState) return;
			const now = performance.now();
			const totalX = event.clientX - dragState.startX;
			const totalY = event.clientY - dragState.startY;
			const deltaY = event.clientY - dragState.lastY;
			dragState.velocityY = (deltaY / Math.max(1, now - dragState.lastTime)) * 1000;
			dragState.lastX = event.clientX;
			dragState.lastY = event.clientY;
			dragState.lastTime = now;
			if (dragState.mode === 'pending') {
				if (Math.abs(totalY) < DRAG_START_THRESHOLD_PX && Math.abs(totalX) < DRAG_START_THRESHOLD_PX) return;
				if (Math.abs(totalX) > Math.abs(totalY) * 1.15) {
					dragState.mode = 'ignore';
					return;
				}
				dragState.mode = 'sheet';
			}
			if (dragState.mode !== 'sheet') return;
			event.preventDefault();
			startSheetDrag();
			applyDragDelta(deltaY, dragState.scrollTarget);
		},
		[applyDragDelta, startSheetDrag],
	);
	const endPointerDrag = useCallback(() => {
		finishSheetDrag();
		if (dragStateRef.current?.didDragSheet) {
			suppressClickUntilRef.current = Date.now() + 350;
		}
		dragStateRef.current = null;
		document.removeEventListener('pointermove', handlePointerMove);
		document.removeEventListener('pointerup', endPointerDrag);
		document.removeEventListener('pointercancel', endPointerDrag);
	}, [finishSheetDrag, handlePointerMove]);
	const handlePointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (
				event.pointerType === 'touch' ||
				event.button !== 0 ||
				!isOpenRef.current ||
				!containerRef.current ||
				!isEventTargetInsideContainer(event.target, containerRef.current) ||
				shouldIgnoreDragStart(event.target)
			) {
				return;
			}
			dragStateRef.current = {
				identifier: null,
				mode: 'pending',
				startX: event.clientX,
				startY: event.clientY,
				lastX: event.clientX,
				lastY: event.clientY,
				lastTime: performance.now(),
				velocityY: 0,
				scrollTarget: findScrollableTarget(event.target, containerRef.current),
				didDragSheet: false,
			};
			document.addEventListener('pointermove', handlePointerMove);
			document.addEventListener('pointerup', endPointerDrag);
			document.addEventListener('pointercancel', endPointerDrag);
		},
		[endPointerDrag, handlePointerMove],
	);
	const handleClickCapture = useCallback((event: React.MouseEvent<HTMLDivElement>) => {
		if (Date.now() > suppressClickUntilRef.current) return;
		event.preventDefault();
		event.stopPropagation();
	}, []);
	useEffect(() => {
		if (!isMounted) return;
		const zIndexForOverlay = OverlayStack.acquire();
		setAcquiredZIndex(zIndexForOverlay);
		return () => {
			OverlayStack.release();
			setAcquiredZIndex(null);
		};
	}, [isMounted]);
	useEffect(() => {
		if (!isMounted) return undefined;
		return lockDocumentScroll();
	}, [isMounted]);
	useAccountTransitionDismissal(isOpen, onClose);
	useEffect(() => {
		if (!isOpen) return undefined;
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key !== 'Escape') return;
			event.preventDefault();
			onCloseRef.current();
		};
		document.addEventListener('keydown', handleKeyDown);
		return () => document.removeEventListener('keydown', handleKeyDown);
	}, [isOpen]);
	useEffect(() => {
		if (isOpen) {
			if (closeTimerRef.current != null) {
				window.clearTimeout(closeTimerRef.current);
				closeTimerRef.current = null;
			}
			previousFocusRef.current = document.activeElement instanceof HTMLElement ? document.activeElement : null;
			setIsMounted(true);
			setSheetState('open');
			return;
		}
		if (!isMounted) return;
		setSheetState('closing');
		animateClosed();
		closeTimerRef.current = window.setTimeout(() => {
			setIsMounted(false);
			cleanupModalEffect(modalEffectRootIdRef.current);
			const previousFocus = previousFocusRef.current;
			if (previousFocus && document.contains(previousFocus)) {
				previousFocus.focus({preventScroll: true});
			}
		}, SHEET_ANIMATION_MS + 80);
		return () => {
			if (closeTimerRef.current != null) {
				window.clearTimeout(closeTimerRef.current);
				closeTimerRef.current = null;
			}
		};
	}, [animateClosed, isMounted, isOpen]);
	useIsomorphicLayoutEffect(() => {
		if (!isMounted) return;
		syncMeasurements();
		const container = containerRef.current;
		const root = rootRef.current;
		if (!container || !root) return;
		const resizeObserver = new ResizeObserver(() => syncMeasurements({snapToCurrent: true}));
		resizeObserver.observe(container);
		const handleViewportChange = () => syncMeasurements({snapToCurrent: true});
		window.addEventListener('resize', handleViewportChange);
		window.visualViewport?.addEventListener('resize', handleViewportChange);
		window.visualViewport?.addEventListener('scroll', handleViewportChange);
		return () => {
			resizeObserver.disconnect();
			window.removeEventListener('resize', handleViewportChange);
			window.visualViewport?.removeEventListener('resize', handleViewportChange);
			window.visualViewport?.removeEventListener('scroll', handleViewportChange);
		};
	}, [isMounted, snapPointsKey, syncMeasurements]);
	useIsomorphicLayoutEffect(() => {
		if (!isMounted) return;
		syncMeasurements();
		if (isOpen) {
			const snapIndex = resolveSnapIndex(initialSnapRef.current, snapPointsRef.current);
			currentSnapRef.current = snapIndex;
			setTransitionEnabled(false);
			setSheetY(sheetHeightRef.current || getViewportHeight());
			const frame = window.requestAnimationFrame(() => {
				setTransitionEnabled(true);
				snapToIndex(snapIndex);
				const container = containerRef.current;
				if (container && !container.contains(document.activeElement)) {
					container.focus({preventScroll: true});
				}
			});
			return () => window.cancelAnimationFrame(frame);
		}
		animateClosed();
		return undefined;
	}, [animateClosed, isMounted, isOpen, setSheetY, setTransitionEnabled, snapToIndex, syncMeasurements]);
	useEffect(() => {
		return () => {
			if (closeTimerRef.current != null) {
				window.clearTimeout(closeTimerRef.current);
			}
			cleanupModalEffect(modalEffectRootIdRef.current);
			document.removeEventListener('touchmove', handleTouchMove);
			document.removeEventListener('touchend', endTouchDrag);
			document.removeEventListener('touchcancel', endTouchDrag);
			document.removeEventListener('pointermove', handlePointerMove);
			document.removeEventListener('pointerup', endPointerDrag);
			document.removeEventListener('pointercancel', endPointerDrag);
		};
	}, [endPointerDrag, endTouchDrag, handlePointerMove, handleTouchMove]);
	useBottomSheetBackHandler(isOpen, onClose);
	if (!isMounted || typeof document === 'undefined') return null;
	return createPortal(
		<div
			ref={rootRef}
			className={styles.overlayRoot}
			data-rsbs-root=""
			data-rsbs-overlay=""
			data-state={sheetState}
			style={{zIndex}}
			data-flx="ui.sheet.sheet.overlay-root"
		>
			{showBackdrop && (
				<div
					className={styles.backdrop}
					data-rsbs-backdrop=""
					onClick={onClose}
					aria-hidden="true"
					data-flx="ui.sheet.sheet.backdrop"
				/>
			)}
			{/* biome-ignore lint/a11y/useAriaPropsSupportedByRole: RootProps restricts role to dialog or alertdialog. */}
			<div
				ref={containerRef}
				data-rsbs-container=""
				className={clsx(styles.container, surfaceClassMap[surface])}
				role={role}
				aria-modal="true"
				aria-labelledby={labelledBy}
				aria-describedby={ariaDescribedBy}
				aria-label={resolvedAriaLabel}
				tabIndex={-1}
				onTouchStart={handleTouchStart}
				onPointerDown={handlePointerDown}
				onClickCapture={handleClickCapture}
				data-flx="ui.sheet.sheet.container"
			>
				<SheetA11yContext.Provider value={a11yContext}>
					<div className={clsx(styles.root, className)} data-flx="ui.sheet.sheet.root">
						{children}
					</div>
				</SheetA11yContext.Provider>
			</div>
		</div>,
		resolvePortalHost(sheetPortalHost),
	);
};
export const Root = observer(RootComponent);

interface HandleProps {
	className?: string;
	children?: React.ReactNode;
}

export const Handle: React.FC<HandleProps> = ({className, children}) => (
	<div className={clsx(styles.handle, className)} data-flx="ui.sheet.sheet.handle.handle">
		{children ?? <div className={styles.handleBar} aria-hidden="true" data-flx="ui.sheet.sheet.handle.handle-bar" />}
	</div>
);

type HeaderAlign = 'center' | 'start' | 'end';

interface HeaderProps {
	children?: React.ReactNode;
	leading?: React.ReactNode;
	trailing?: React.ReactNode;
	border?: boolean;
	align?: HeaderAlign;
	padding?: 'sm' | 'md' | 'lg';
	className?: string;
	safeAreaTop?: boolean;
	after?: React.ReactNode;
}

const headerAlignClassMap: Record<HeaderAlign, string> = {
	center: '',
	start: styles.headerAlignStart,
	end: styles.headerAlignEnd,
};
const headerPaddingClassMap: Record<'sm' | 'md' | 'lg', string> = {
	sm: styles.headerPaddingSm,
	md: styles.headerPaddingMd,
	lg: styles.headerPaddingLg,
};
export const Header: React.FC<HeaderProps> = ({
	children,
	leading,
	trailing,
	border = true,
	align = 'center',
	padding = 'md',
	className,
	safeAreaTop = false,
	after,
}) => (
	<div
		className={clsx(
			styles.header,
			border && styles.headerBorder,
			headerPaddingClassMap[padding],
			headerAlignClassMap[align],
			safeAreaTop && styles.headerSafeArea,
			className,
		)}
		data-flx="ui.sheet.sheet.header.header"
	>
		<div className={styles.headerGrid} data-flx="ui.sheet.sheet.header.header-grid">
			<div className={clsx(styles.headerSlot, styles.headerSlotLeading)} data-flx="ui.sheet.sheet.header.header-slot">
				{leading}
			</div>
			<div className={styles.headerCenter} data-flx="ui.sheet.sheet.header.header-center">
				{children}
			</div>
			<div
				className={clsx(styles.headerSlot, styles.headerSlotTrailing)}
				data-flx="ui.sheet.sheet.header.header-slot--2"
			>
				{trailing}
			</div>
		</div>
		{after && (
			<div className={styles.headerAfter} data-flx="ui.sheet.sheet.header.header-after">
				{after}
			</div>
		)}
	</div>
);

interface TitleProps {
	children: React.ReactNode;
	as?: 'h2' | 'h3' | 'span';
}

export const Title: React.FC<TitleProps> = ({children, as: Component = 'h2'}) => (
	<SheetTitleContent as={Component} data-flx="ui.sheet.sheet.title.sheet-title-content">
		{children}
	</SheetTitleContent>
);
const SheetTitleContent: React.FC<TitleProps> = ({children, as: Component = 'h2'}) => {
	const context = useContext(SheetA11yContext);
	useEffect(() => {
		if (!context) return;
		context.setHasTitle(true);
		return () => context.setHasTitle(false);
	}, [context]);
	return (
		<Component id={context?.titleId} className={styles.title} data-flx="ui.sheet.sheet.sheet-title-content.title">
			{children}
		</Component>
	);
};

interface ContentProps {
	children: React.ReactNode;
	padding?: 'none' | 'md';
	scrollable?: boolean;
	className?: string;
}

export const Content: React.FC<ContentProps> = ({children, padding = 'md', scrollable = true, className}) => (
	<div
		className={clsx(styles.content, padding === 'none' && styles.contentNoPadding, className)}
		data-flx="ui.sheet.sheet.content.content"
	>
		<div
			className={clsx(styles.contentInner, !scrollable && styles.contentStatic)}
			tabIndex={-1}
			data-sheet-scroll-region="true"
			data-flx="ui.sheet.sheet.content.content-inner"
		>
			{children}
		</div>
	</div>
);

interface CloseButtonProps {
	onClick: () => void;
	className?: string;
}

export const CloseButton: React.FC<CloseButtonProps> = ({onClick, className}) => {
	const {i18n} = useLingui();
	return (
		<button
			type="button"
			onClick={onClick}
			className={clsx(styles.closeButton, className)}
			aria-label={i18n._(CLOSE_DESCRIPTOR)}
			data-flx="ui.sheet.sheet.close-button.close-button.click"
		>
			<XIcon weight="bold" data-flx="ui.sheet.sheet.close-button.x-icon" />
		</button>
	);
};
