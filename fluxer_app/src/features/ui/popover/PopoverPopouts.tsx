// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useAntiShiftFloating} from '@app/features/app/hooks/useAntiShiftFloating';
import {shouldDisableAutofocusOnMobile} from '@app/features/platform/utils/AutofocusUtils';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {type Popout, PopoutKeyContext, type PopoutReferenceRect} from '@app/features/ui/popover';
import {PopoutResizePositionContext} from '@app/features/ui/popover/PopoutResizePositionContext';
import {getPopoutFocusManagerInsideElements} from '@app/features/ui/popover/PopoverFocusManagerUtils';
import styles from '@app/features/ui/popover/PopoverPopout.module.css';
import {scheduleFloatingPortalSweep} from '@app/features/ui/popover/PopoverPortalCleanup';
import LayerManager from '@app/features/ui/state/LayerManager';
import PopoutState from '@app/features/ui/state/Popout';
import {isScrollbarDragActive} from '@app/features/ui/utils/ScrollbarDragState';
import {canUseWindowFocusedHoverControls} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {wasPointerDownInside} from '@app/lib/overlay/DismissGuard';
import {FloatingFocusManager, useFloating, useMergeRefs, type VirtualElement} from '@floating-ui/react';
import {LinguiContext} from '@lingui/react';
import {clsx} from 'clsx';
import {motion, type Transition} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';
import {createPortal} from 'react-dom';

type PopoutItemProps = Omit<Popout, 'key'> & {
	popoutKey: string;
	isTopmost: boolean;
	isClosingRequested: boolean;
	hoverMode?: boolean;
	onContentMouseEnter?: () => void;
	onContentMouseLeave?: () => void;
};

type PopoutAnimationType = NonNullable<Popout['animationType']>;

const FOCUSABLE_SELECTOR =
	'button:not([disabled]), [href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"]), [role="menuitem"], [role="menuitemcheckbox"], [role="menuitemradio"], [contenteditable=""], [contenteditable="true"]';
const MENU_ITEM_SELECTOR =
	'[data-roving-focus="true"], [role="menuitem"], [role="menuitemcheckbox"], [role="menuitemradio"]';
const focusNestedMenuItem = (root: HTMLElement, placement: 'first' | 'last'): boolean => {
	const menu = root.matches('[role="menu"]') ? root : root.querySelector<HTMLElement>('[role="menu"]');
	if (!menu) return false;
	const items = Array.from(menu.querySelectorAll<HTMLElement>(MENU_ITEM_SELECTOR)).filter((element) => {
		if ('disabled' in element && typeof (element as HTMLButtonElement).disabled === 'boolean') {
			return !(element as HTMLButtonElement).disabled;
		}
		return element.getAttribute('aria-disabled') !== 'true';
	});
	const target = placement === 'last' ? items.at(-1) : items[0];
	if (!target) return false;
	target.focus({preventScroll: true});
	return true;
};
const observeTargetRemoval = (target: HTMLElement, onRemoved: () => void): (() => void) => {
	if (typeof MutationObserver === 'undefined' || typeof document === 'undefined') {
		return () => {};
	}
	const ownerDocument = target.ownerDocument;
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) return () => {};
	let rafId: number | null = null;
	let didNotify = false;
	const observer = new MutationObserver(() => {
		if (rafId != null || didNotify) return;
		rafId = ownerWindow.requestAnimationFrame(() => {
			rafId = null;
			if (!didNotify && !ownerDocument.contains(target)) {
				didNotify = true;
				onRemoved();
			}
		});
	});
	for (let ancestor = target.parentElement; ancestor; ancestor = ancestor.parentElement) {
		observer.observe(ancestor, {childList: true});
		if (ancestor === ownerDocument.documentElement) break;
	}
	return () => {
		observer.disconnect();
		if (rafId != null) {
			ownerWindow.cancelAnimationFrame(rafId);
			rafId = null;
		}
	};
};
const createFrozenReference = (rect: PopoutReferenceRect, contextElement: HTMLElement): VirtualElement => ({
	contextElement,
	getBoundingClientRect: () => rect,
	getClientRects: () => [rect],
});
const findInitialFocusTarget = (root: HTMLElement): HTMLElement | null => {
	const explicit = root.querySelector<HTMLElement>('[data-autofocus], [autofocus]');
	if (explicit) {
		return explicit;
	}
	const focusable = root.querySelectorAll<HTMLElement>(FOCUSABLE_SELECTOR);
	for (const element of focusable) {
		if (element.getAttribute('aria-hidden') === 'true') continue;
		return element;
	}
	return null;
};
function getPopoutCloseDuration(animationType: PopoutAnimationType, prefersReducedMotion: boolean): number {
	if (prefersReducedMotion || animationType === 'none') {
		return 0;
	}
	return isProfileSlideAnimation(animationType) ? 140 : 250;
}

function isProfileSlideAnimation(animationType: PopoutAnimationType): boolean {
	return animationType === 'profile-slide' || animationType === 'profile-slide-inverted';
}

function getPopoutMotion(
	animationType: PopoutAnimationType,
	shouldAnimate: boolean,
	isOpen: boolean,
): {
	initial: false | Record<string, number>;
	animate: Record<string, number>;
	transition: Transition;
} {
	if (!shouldAnimate || animationType === 'none') {
		return {
			initial: false as const,
			animate: {opacity: isOpen ? 1 : 0, x: 0, scale: 1},
			transition: {duration: 0},
		};
	}
	if (isProfileSlideAnimation(animationType)) {
		const slideOffset = animationType === 'profile-slide-inverted' ? -10 : 10;
		return {
			initial: {opacity: 0, x: slideOffset},
			animate: {opacity: isOpen ? 1 : 0, x: isOpen ? 0 : slideOffset},
			transition: {duration: 0.14, ease: 'easeOut'},
		};
	}
	return {
		initial: {opacity: 0, scale: 0.98},
		animate: {opacity: isOpen ? 1 : 0, scale: isOpen ? 1 : 0.98, x: 0},
		transition: {duration: 0.25, ease: 'easeInOut'},
	};
}

const isTextEntryElement = (element: HTMLElement): boolean => {
	const ownerWindow = element.ownerDocument.defaultView;
	if (
		ownerWindow &&
		(element instanceof ownerWindow.HTMLInputElement || element instanceof ownerWindow.HTMLTextAreaElement)
	) {
		return true;
	}
	return element.isContentEditable;
};
const PopoutItem: React.FC<PopoutItemProps> = observer(
	({
		popoutKey,
		isTopmost,
		isClosingRequested,
		render,
		position,
		target,
		frozenTargetRect,
		zIndexBoost,
		shouldAutoUpdate = true,
		offsetMainAxis = 8,
		offsetCrossAxis = 0,
		animationType = 'smooth',
		constrainHeight = true,
		containerClass,
		onCloseRequest,
		returnFocusRef,
		returnFocusOnClose = true,
		keepOpenOnTargetUnmount = false,
		hoverMode,
		onContentMouseEnter,
		onContentMouseLeave,
	}) => {
		useContext(LinguiContext);
		const ownerDocument = target.ownerDocument;
		const ownerWindow = ownerDocument.defaultView;
		const positionReference = useMemo(
			() => (frozenTargetRect ? createFrozenReference(frozenTargetRect, target) : target),
			[frozenTargetRect, target],
		);
		const {
			ref: popoutRef,
			state,
			style,
			beginManualPositioning,
		} = useAntiShiftFloating(positionReference, true, {
			placement: position,
			offsetMainAxis,
			offsetCrossAxis,
			shouldAutoUpdate: frozenTargetRect ? false : shouldAutoUpdate,
			shouldObserveFloatingResize: !frozenTargetRect || !constrainHeight,
			constrainHeight,
			enableSmartBoundary: true,
		});
		const resizePositionController = useMemo(() => ({begin: beginManualPositioning}), [beginManualPositioning]);
		const {refs: focusRefs, context: focusContext} = useFloating({open: true});
		useLayoutEffect(() => {
			focusRefs.setReference(target);
		}, [focusRefs, target]);
		const mergedPopoutRef = useMergeRefs([popoutRef, focusRefs.setFloating]);
		const prefersReducedMotion = Accessibility.useReducedMotion;
		const [isVisible, setIsVisible] = useState(true);
		const [targetInDOM, setTargetInDOM] = useState(() => ownerDocument.contains(target));
		const hasFocusedInitialRef = useRef(false);
		const closeTimerRef = useRef<number | null>(null);
		const beginClose = useCallback(() => {
			if (closeTimerRef.current != null) return;
			setIsVisible(false);
			const closeDuration = getPopoutCloseDuration(animationType, prefersReducedMotion);
			if (ownerWindow == null) {
				PopoutCommands.finishClose(popoutKey);
				return;
			}
			closeTimerRef.current = ownerWindow.setTimeout(() => {
				closeTimerRef.current = null;
				PopoutCommands.finishClose(popoutKey);
			}, closeDuration);
		}, [animationType, ownerWindow, prefersReducedMotion, popoutKey]);
		useLayoutEffect(() => {
			if (!ownerDocument.contains(target)) {
				setTargetInDOM(false);
				if (!keepOpenOnTargetUnmount) {
					beginClose();
				}
			} else {
				setTargetInDOM(true);
			}
		}, [target, ownerDocument, keepOpenOnTargetUnmount, beginClose]);
		useEffect(() => {
			if (isClosingRequested) {
				beginClose();
			}
		}, [isClosingRequested, beginClose]);
		useEffect(() => {
			return () => {
				if (closeTimerRef.current != null && ownerWindow != null) {
					ownerWindow.clearTimeout(closeTimerRef.current);
					closeTimerRef.current = null;
				}
			};
		}, []);
		const hasUsableReference = keepOpenOnTargetUnmount || targetInDOM;
		useLayoutEffect(() => {
			if (!state.isReady || !isVisible || !hasUsableReference || hasFocusedInitialRef.current) {
				return;
			}
			const root = popoutRef.current;
			if (!root) return;
			let focusTarget = findInitialFocusTarget(root);
			if (focusTarget == null) focusTarget = root;
			if (focusTarget === root && !root.hasAttribute('tabindex')) {
				root.tabIndex = -1;
			}
			if (shouldDisableAutofocusOnMobile() && isTextEntryElement(focusTarget)) {
				hasFocusedInitialRef.current = true;
				return;
			}
			focusTarget.focus({preventScroll: true});
			hasFocusedInitialRef.current = true;
		}, [state.isReady, isVisible, hasUsableReference, popoutRef]);
		const isPositioned = animationType === 'none' ? true : state.isReady;
		const shouldShowOpenState = isVisible && !isClosingRequested;
		const motionOpen = shouldShowOpenState && hasUsableReference && isPositioned;
		const popoutMotion = useMemo(
			() => getPopoutMotion(animationType, !prefersReducedMotion, motionOpen),
			[animationType, motionOpen, prefersReducedMotion],
		);
		const transitionStyles = useMemo(() => {
			return {
				pointerEvents:
					isPositioned && hasUsableReference && shouldShowOpenState ? ('auto' as const) : ('none' as const),
				display: hasUsableReference ? undefined : ('none' as const),
			};
		}, [hasUsableReference, isPositioned, shouldShowOpenState]);
		const closeSelf = useCallback(() => {
			beginClose();
		}, [beginClose]);
		const getFocusManagerInsideElements = useCallback(() => {
			let returnFocusElement: HTMLElement | null = null;
			if (returnFocusRef != null) returnFocusElement = returnFocusRef.current;
			return getPopoutFocusManagerInsideElements(target, returnFocusElement);
		}, [target, returnFocusRef]);
		useEffect(() => {
			const el = popoutRef.current;
			const targetIsConnected = ownerDocument.contains(target);
			if (!targetIsConnected) {
				setTargetInDOM(false);
				if (!keepOpenOnTargetUnmount) {
					beginClose();
					return;
				}
			}
			const handleOutsideClick = (event: MouseEvent) => {
				if (isScrollbarDragActive()) {
					return;
				}
				if (LayerManager.hasType('contextmenu')) {
					return;
				}
				if (wasPointerDownInside(el)) {
					return;
				}
				const targetElement = event.target;
				const ownerWindow = ownerDocument.defaultView;
				if (!ownerWindow || !(targetElement instanceof ownerWindow.HTMLElement)) return;
				if (target.contains(targetElement)) {
					return;
				}
				if (
					targetElement.closest('[role="dialog"][aria-modal="true"]') ||
					targetElement.className.includes('backdrop') ||
					targetElement.closest('.focusLock') ||
					targetElement.closest('[data-native-titlebar]')
				) {
					return;
				}
				if (el && !el.contains(targetElement)) {
					const targetPopout = targetElement.closest<HTMLElement>('[data-popout-key]');
					const targetPopoutKey = targetPopout == null ? null : targetPopout.dataset.popoutKey;
					if (targetPopoutKey && PopoutState.isDependentOn(targetPopoutKey, popoutKey)) {
						return;
					}
					if (onCloseRequest && !onCloseRequest(event)) {
						return;
					}
					beginClose();
				}
			};
			const stopObservingTarget = targetIsConnected
				? observeTargetRemoval(target, () => {
						if (!ownerDocument.contains(target)) {
							setTargetInDOM(false);
							if (!keepOpenOnTargetUnmount) {
								beginClose();
							}
						}
					})
				: () => {};
			ownerDocument.addEventListener('click', handleOutsideClick, true);
			return () => {
				stopObservingTarget();
				ownerDocument.removeEventListener('click', handleOutsideClick, true);
			};
		}, [target, ownerDocument, onCloseRequest, popoutRef, keepOpenOnTargetUnmount, beginClose]);
		const handleMouseEnter = useCallback(() => {
			if (hoverMode && onContentMouseEnter) {
				onContentMouseEnter();
			}
		}, [hoverMode, onContentMouseEnter]);
		const handleMouseLeave = useCallback(() => {
			if (hoverMode && onContentMouseLeave) {
				onContentMouseLeave();
			}
		}, [hoverMode, onContentMouseLeave]);
		const handleHoverEventCapture = useCallback((event: React.SyntheticEvent) => {
			const eventRoot = (event.currentTarget as HTMLElement).ownerDocument.documentElement;
			if (canUseWindowFocusedHoverControls(eventRoot)) return;
			event.stopPropagation();
		}, []);
		const handlePopoutShellKeyDown = useCallback((event: React.KeyboardEvent<HTMLDivElement>) => {
			if (event.target !== event.currentTarget) return;
			if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp') return;
			if (!focusNestedMenuItem(event.currentTarget, event.key === 'ArrowUp' ? 'last' : 'first')) return;
			event.preventDefault();
			event.stopPropagation();
		}, []);
		return (
			<FloatingFocusManager
				context={focusContext}
				disabled={!isTopmost}
				returnFocus={returnFocusOnClose ? (returnFocusRef == null ? targetInDOM : returnFocusRef) : false}
				initialFocus={focusRefs.floating}
				getInsideElements={getFocusManagerInsideElements}
				data-flx="ui.popover.popouts.popout-item.floating-focus-manager"
			>
				<PopoutKeyContext.Provider value={popoutKey}>
					<PopoutResizePositionContext.Provider value={resizePositionController}>
						<motion.div
							id={popoutKey}
							ref={mergedPopoutRef}
							role="group"
							className={clsx(styles.popout, containerClass)}
							tabIndex={-1}
							data-popout-key={popoutKey}
							onMouseEnter={handleMouseEnter}
							onMouseLeave={handleMouseLeave}
							onKeyDown={handlePopoutShellKeyDown}
							onMouseMoveCapture={handleHoverEventCapture}
							onMouseOverCapture={handleHoverEventCapture}
							onPointerMoveCapture={handleHoverEventCapture}
							onPointerOverCapture={handleHoverEventCapture}
							style={{
								...style,
								zIndex: zIndexBoost != null ? 1000 + zIndexBoost : undefined,
								...transitionStyles,
								visibility: state.isReady && hasUsableReference ? 'visible' : 'hidden',
							}}
							initial={popoutMotion.initial}
							animate={popoutMotion.animate}
							transition={popoutMotion.transition}
							data-flx="ui.popover.popouts.popout-item.popout"
						>
							{render({
								popoutKey,
								onClose: closeSelf,
							})}
						</motion.div>
					</PopoutResizePositionContext.Provider>
				</PopoutKeyContext.Provider>
			</FloatingFocusManager>
		);
	},
);
interface PopoutsProps {
	ownerDocument?: Document;
}

export const Popouts: React.FC<PopoutsProps> = observer(({ownerDocument}) => {
	const prevPopoutKeysRef = useRef<Set<string>>(new Set());
	const portalHost = usePortalHost();
	let scopeDocument = document;
	if (portalHost != null) scopeDocument = portalHost.ownerDocument;
	if (ownerDocument != null) scopeDocument = ownerDocument;
	const popouts = PopoutState.getPopouts(scopeDocument);
	const topPopout = popouts.length ? popouts[popouts.length - 1] : null;
	const needsBackdrop = Boolean(topPopout && !topPopout.disableBackdrop);
	useEffect(() => {
		const currentKeys = new Set(popouts.map((popout) => popout.key.toString()));
		const prevKeys = prevPopoutKeysRef.current;
		currentKeys.forEach((key) => {
			if (!prevKeys.has(key)) {
				LayerManager.addLayer('popout', key);
			}
		});
		prevKeys.forEach((key) => {
			if (!currentKeys.has(key)) {
				LayerManager.removeLayer('popout', key);
			}
		});
		prevPopoutKeysRef.current = currentKeys;
	}, [popouts]);
	useEffect(() => {
		return () => {
			prevPopoutKeysRef.current.forEach((key) => {
				LayerManager.removeLayer('popout', key);
			});
			scheduleFloatingPortalSweep();
		};
	}, []);
	const handleBackdropPointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (isScrollbarDragActive()) {
				return;
			}
			if (event.pointerType === 'mouse' && event.button !== 0) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();
			PopoutCommands.closeAllForDocument(scopeDocument);
		},
		[scopeDocument],
	);
	const content = (
		<div
			className={styles.popouts}
			data-popouts-root
			data-overlay-pass-through="true"
			data-flx="ui.popover.popouts.popouts"
		>
			{needsBackdrop && (
				<div
					className={styles.backdrop}
					onPointerDown={handleBackdropPointerDown}
					aria-hidden="true"
					data-flx="ui.popover.popouts.backdrop"
				/>
			)}
			{popouts.map((popout) => (
				<PopoutItem
					data-flx="ui.popover.popouts.popout-item"
					{...popout}
					key={popout.key}
					popoutKey={popout.key.toString()}
					isTopmost={topPopout != null && topPopout.key === popout.key}
					isClosingRequested={PopoutState.isClosing(popout.key)}
				/>
			))}
		</div>
	);
	if (portalHost) {
		return createPortal(content, portalHost);
	}
	return content;
});
