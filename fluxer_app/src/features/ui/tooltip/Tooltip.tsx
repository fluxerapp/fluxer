// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import ContextMenuState from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import styles from '@app/features/ui/tooltip/Tooltip.module.css';
import {
	createTooltipSnapshot,
	getTooltipStateValue,
	selectTooltipModel,
	type TooltipEnvironment,
	type TooltipMachineEvent,
	tooltipSnapshotsAreEquivalent,
	transitionTooltipSnapshot,
} from '@app/features/ui/tooltip/TooltipStateMachine';
import {appZoomCssPx, appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {getReducedMotionProps, TOOLTIP_MOTION} from '@app/features/ui/utils/ReducedMotionAnimation';
import {
	canUseWindowFocusedHoverControls,
	subscribeWindowHoverControlsChange,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {elementSupportsRef} from '@app/lib/react';
import {FloatingPortal} from '@floating-ui/react';
import {arrow, autoUpdate, computePosition, flip, offset, shift} from '@floating-ui/react-dom';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useId, useLayoutEffect, useMemo, useRef, useState} from 'react';
import {useExclusiveTooltip} from './TooltipExclusivity';
import {getTooltipScrollSuppressRemainingMs, subscribeTooltipScrollHide} from './TooltipScrollCoordinator';

const logger = new Logger('Tooltip');

export type TooltipPosition = 'top' | 'left' | 'right' | 'bottom';
export type TooltipType = 'normal' | 'error';

const TOOLTIP_AUTO_UPDATE_OPTIONS = {
	ancestorScroll: false,
	ancestorResize: true,
	elementResize: true,
	layoutShift: true,
} as const;

const tooltipPortalRoots = new WeakMap<Document, HTMLElement>();

const getTooltipPortalRoot = (ownerDocument: Document): HTMLElement => {
	const existingRoot = tooltipPortalRoots.get(ownerDocument);
	if (existingRoot) {
		return existingRoot;
	}
	const root = ownerDocument.createElement('div');
	root.className = styles.tooltips;
	root.setAttribute('data-tooltip-portal-root', 'true');
	ownerDocument.body.appendChild(root);
	tooltipPortalRoots.set(ownerDocument, root);
	return root;
};
export const useTooltipPortalRoot = (enabled = true, targetDocument?: Document): HTMLElement | undefined => {
	const [root, setRoot] = useState<HTMLElement | undefined>(undefined);
	const portalHost = usePortalHost();
	const resolvedDocument = targetDocument ?? document;
	const scopedPortalHost = portalHost && portalHost.ownerDocument === resolvedDocument ? portalHost : null;
	useLayoutEffect(() => {
		if (!enabled) return;
		if (scopedPortalHost) {
			setRoot(undefined);
			return;
		}
		setRoot(getTooltipPortalRoot(resolvedDocument));
	}, [enabled, resolvedDocument, scopedPortalHost]);
	if (!enabled) return undefined;
	return scopedPortalHost ?? root;
};
const MAX_WIDTH_MAP = {
	default: remFromPx(190),
	xl: remFromPx(350),
	none: 'none' as const,
};
const TooltipPositionToStyle: Record<TooltipPosition, string> = {
	top: styles.tooltipTop,
	left: styles.tooltipLeft,
	right: styles.tooltipRight,
	bottom: styles.tooltipBottom,
};
const TooltipTypeToStyle: Record<TooltipType, string> = {
	normal: styles.tooltipPrimary,
	error: styles.tooltipRed,
};

interface TooltipProps {
	text: string | (() => React.ReactNode);
	type?: TooltipType;
	position?: TooltipPosition;
	align?: 'center' | 'top' | 'bottom' | 'left' | 'right';
	nudge?: number;
	delay?: number;
	padding?: number;
	maxWidth?: 'default' | 'xl' | 'none';
	size?: 'default' | 'large';
	openOnMountHover?: boolean;
	allowWhenContextMenuOpen?: boolean;
	children?: React.ReactElement;
}

type TooltipVisibilityDriver = 'hover' | 'focus';

const nodeHasAccessibleText = (node: React.ReactNode): boolean => {
	if (typeof node === 'string') return node.trim().length > 0;
	if (typeof node === 'number') return true;
	if (Array.isArray(node)) return node.some(nodeHasAccessibleText);
	if (!React.isValidElement(node)) return false;
	const props = node.props as {
		children?: React.ReactNode;
		'aria-hidden'?: boolean | 'true' | 'false';
	};
	if (props['aria-hidden'] === true || props['aria-hidden'] === 'true') return false;
	return nodeHasAccessibleText(props.children);
};

const tooltipHasRenderableContent = (text: TooltipProps['text']): boolean => {
	if (typeof text === 'function') return true;
	if (typeof text === 'string') return text.trim().length > 0;
	return !!text;
};

const TooltipContainer = React.forwardRef<
	HTMLDivElement,
	{
		text: string | (() => React.ReactNode);
		type: TooltipType;
		position: TooltipPosition;
		maxWidth: 'default' | 'xl' | 'none';
		size: 'default' | 'large';
		arrowRef: React.RefObject<HTMLDivElement | null> | React.RefObject<HTMLDivElement>;
		arrowX?: number;
		arrowY?: number;
	}
>(({text, type, position, maxWidth, size, arrowRef, arrowX, arrowY}, ref) => {
	const content = typeof text === 'function' ? text() : text;
	if (
		!content ||
		(Array.isArray(content) && content.length === 0) ||
		(typeof content === 'string' && content.trim().length === 0)
	) {
		return null;
	}
	const arrowStyle: React.CSSProperties = {};
	if (position === 'top' || position === 'bottom') {
		if (arrowX != null) {
			arrowStyle.left = appZoomCssPx(arrowX);
			arrowStyle.marginLeft = 0;
		}
	} else {
		if (arrowY != null) {
			arrowStyle.top = appZoomCssPx(arrowY);
			arrowStyle.marginTop = 0;
		}
	}
	return (
		<div
			ref={ref}
			className={clsx(styles.tooltip, TooltipPositionToStyle[position], TooltipTypeToStyle[type])}
			style={{maxWidth: MAX_WIDTH_MAP[maxWidth]}}
			data-flx="ui.tooltip.tooltip.tooltip-container.tooltip"
		>
			<div
				ref={arrowRef}
				className={clsx(styles.tooltipPointer, styles.tooltipPointerBg)}
				style={arrowStyle}
				data-flx="ui.tooltip.tooltip.tooltip-container.tooltip-pointer"
			/>
			<div
				className={clsx(styles.tooltipPointer)}
				style={arrowStyle}
				data-flx="ui.tooltip.tooltip.tooltip-container.tooltip-pointer--2"
			/>
			<div
				className={size === 'large' ? styles.tooltipContentLarge : styles.tooltipContent}
				data-flx="ui.tooltip.tooltip.tooltip-container.tooltip-content"
			>
				{content}
			</div>
		</div>
	);
});

TooltipContainer.displayName = 'TooltipContainer';

export const Tooltip = observer(
	({
		text,
		type = 'normal',
		position = 'top',
		nudge = 0,
		delay = 0,
		padding = 8,
		maxWidth = 'default',
		size = 'default',
		openOnMountHover = true,
		allowWhenContextMenuOpen = false,
		children,
	}: TooltipProps) => {
		const tooltipId = useId();
		const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
		const mobileLayout = MobileLayout;
		type TooltipChildProps = React.HTMLAttributes<HTMLElement> & {disabled?: boolean; ref?: React.Ref<HTMLElement>};
		const child = children && React.isValidElement<TooltipChildProps>(children) ? React.Children.only(children) : null;
		const childSupportsRef = child ? elementSupportsRef(child) : false;
		const childRef = childSupportsRef && child ? (child.props.ref ?? null) : null;
		const childProps = (child?.props ?? {}) as TooltipChildProps;
		const {
			onMouseEnter: childMouseEnter,
			onMouseLeave: childMouseLeave,
			onClick: childClick,
			onTouchStart: childTouchStart,
			onTouchEnd: childTouchEnd,
			onTouchCancel: childTouchCancel,
			onMouseDownCapture: childMouseDownCapture,
			onPointerDownCapture: childPointerDownCapture,
			onFocus: childFocus,
			onBlur: childBlur,
			onKeyDown: childKeyDown,
		} = childProps;
		const isDisabled = Boolean(childProps.disabled || childProps['aria-disabled']);
		const hasRenderableContent = tooltipHasRenderableContent(text);
		const targetRef = useRef<HTMLElement | null>(null);
		const getTooltipOwnerDocument = useCallback((): Document => targetRef.current?.ownerDocument ?? document, []);
		const getTooltipOwnerWindow = useCallback((): Window => {
			return getTooltipOwnerDocument().defaultView ?? window;
		}, [getTooltipOwnerDocument]);
		const canUseTooltipHoverControls = useCallback((): boolean => {
			return canUseWindowFocusedHoverControls(getTooltipOwnerDocument().documentElement);
		}, [getTooltipOwnerDocument]);
		const isTooltipContextMenuOpen = useCallback((): boolean => {
			if (allowWhenContextMenuOpen) return false;
			return ContextMenuState.getContextMenu(getTooltipOwnerDocument()) != null;
		}, [allowWhenContextMenuOpen, getTooltipOwnerDocument]);
		const contextMenuOpen = isTooltipContextMenuOpen();
		const tooltipEnvironment = useMemo(
			(): TooltipEnvironment => ({
				disabled: isDisabled,
				mobileEnabled: mobileLayout.enabled,
				contextMenuOpen,
				keyboardModeEnabled,
				hoverControlsEnabled: canUseTooltipHoverControls(),
				hasRenderableContent,
			}),
			[
				canUseTooltipHoverControls,
				contextMenuOpen,
				hasRenderableContent,
				isDisabled,
				keyboardModeEnabled,
				mobileLayout.enabled,
			],
		);
		const [tooltipSnapshot, setTooltipSnapshot] = useState(() => createTooltipSnapshot(tooltipEnvironment));
		const tooltipModel = selectTooltipModel(tooltipSnapshot);
		const tooltipStateValue = getTooltipStateValue(tooltipSnapshot);
		const shouldRenderTooltip = tooltipModel.shouldRender;
		const hasActiveVisibilityState = tooltipStateValue !== 'hidden' || tooltipModel.hasVisibilityDriver;
		const portalRoot = useTooltipPortalRoot(shouldRenderTooltip, getTooltipOwnerDocument());
		const timerRef = useRef<number | null>(null);
		const timerWindowRef = useRef<Window | null>(null);
		const pendingDelayMsRef = useRef(0);
		const longPressTimeoutRef = useRef<number | null>(null);
		const pointerFocusTimeoutRef = useRef<number | null>(null);
		const pointerFocusWindowRef = useRef<Window | null>(null);
		const ignoreNextFocusRef = useRef(false);
		const arrowRef = useRef<HTMLDivElement>(null);
		const tooltipRef = useRef<HTMLDivElement>(null);
		const cleanupRef = useRef<(() => void) | null>(null);
		const rafRef = useRef<number | null>(null);
		const rafWindowRef = useRef<Window | null>(null);
		const isCalculatingRef = useRef(false);
		const mountedRef = useRef(true);
		const positionRequestIdRef = useRef(0);
		const shouldRenderTooltipRef = useRef(shouldRenderTooltip);
		const tooltipSnapshotRef = useRef(tooltipSnapshot);
		const tooltipModelRef = useRef(tooltipModel);
		const environmentRef = useRef(tooltipEnvironment);
		shouldRenderTooltipRef.current = shouldRenderTooltip;
		tooltipSnapshotRef.current = tooltipSnapshot;
		tooltipModelRef.current = tooltipModel;
		environmentRef.current = tooltipEnvironment;
		const isCrossDocumentTooltip = portalRoot != null && portalRoot.ownerDocument !== document;
		const tooltipMotion = getReducedMotionProps(
			TOOLTIP_MOTION,
			Accessibility.useReducedMotion || isCrossDocumentTooltip,
		);
		const [tooltipState, setTooltipState] = useState({
			x: 0,
			y: 0,
			arrowX: 0,
			arrowY: 0,
			isReady: false,
			actualPlacement: position,
		});
		const sendTooltipEvent = useCallback((event: TooltipMachineEvent) => {
			if (!mountedRef.current) return;
			const prev = tooltipSnapshotRef.current;
			const next = transitionTooltipSnapshot(prev, event);
			if (tooltipSnapshotsAreEquivalent(prev, next)) return;
			tooltipSnapshotRef.current = next;
			setTooltipSnapshot(next);
		}, []);
		const refreshTooltipEnvironment = useCallback(() => {
			const environment = {
				...environmentRef.current,
				contextMenuOpen: isTooltipContextMenuOpen(),
				hoverControlsEnabled: canUseTooltipHoverControls(),
			};
			environmentRef.current = environment;
			sendTooltipEvent({type: 'tooltip.environmentChanged', environment});
		}, [canUseTooltipHoverControls, isTooltipContextMenuOpen, sendTooltipEvent]);
		const getTextString = useCallback((): string => {
			return typeof text === 'string' ? text : '';
		}, [text]);
		const clearDelayTimer = useCallback(() => {
			if (timerRef.current != null) {
				(timerWindowRef.current ?? window).clearTimeout(timerRef.current);
				timerRef.current = null;
				timerWindowRef.current = null;
			}
		}, []);
		const clearPointerFocusTimeout = useCallback(() => {
			if (pointerFocusTimeoutRef.current != null) {
				(pointerFocusWindowRef.current ?? window).clearTimeout(pointerFocusTimeoutRef.current);
				pointerFocusTimeoutRef.current = null;
				pointerFocusWindowRef.current = null;
			}
		}, []);
		const markPointerFocus = useCallback(() => {
			ignoreNextFocusRef.current = true;
			clearPointerFocusTimeout();
			const ownerWindow = getTooltipOwnerWindow();
			pointerFocusWindowRef.current = ownerWindow;
			pointerFocusTimeoutRef.current = ownerWindow.setTimeout(() => {
				pointerFocusTimeoutRef.current = null;
				pointerFocusWindowRef.current = null;
				ignoreNextFocusRef.current = false;
			}, 0);
		}, [clearPointerFocusTimeout, getTooltipOwnerWindow]);
		const cancelRaf = useCallback(() => {
			if (rafRef.current != null) {
				(rafWindowRef.current ?? window).cancelAnimationFrame(rafRef.current);
				rafRef.current = null;
				rafWindowRef.current = null;
			}
		}, []);
		const resetTooltipPosition = useCallback(() => {
			positionRequestIdRef.current += 1;
			setTooltipState((prev) =>
				prev.isReady || prev.actualPlacement !== position ? {...prev, isReady: false, actualPlacement: position} : prev,
			);
		}, [position]);
		const dismissTooltip = useCallback(() => {
			clearDelayTimer();
			cancelRaf();
			const model = tooltipModelRef.current;
			if (model.state === 'hidden' && !model.hasVisibilityDriver && !model.delayPending) return;
			sendTooltipEvent({type: 'tooltip.dismiss'});
			resetTooltipPosition();
		}, [cancelRaf, clearDelayTimer, resetTooltipPosition, sendTooltipEvent]);
		useExclusiveTooltip(shouldRenderTooltip, dismissTooltip);
		const updatePositionNow = useCallback(async () => {
			if (
				!shouldRenderTooltipRef.current ||
				!targetRef.current ||
				!tooltipRef.current ||
				isCalculatingRef.current ||
				!hasRenderableContent
			) {
				return;
			}
			const requestId = ++positionRequestIdRef.current;
			isCalculatingRef.current = true;
			try {
				const target = targetRef.current;
				const tooltip = tooltipRef.current;
				Object.assign(tooltip.style, {
					position: 'fixed',
					left: '-9999px',
					top: '-9999px',
				} as CSSStyleDeclaration);
				const middleware = [
					offset(padding + nudge),
					flip(),
					shift({padding: 8}),
					...(arrowRef.current ? [arrow({element: arrowRef.current})] : []),
				];
				const {x, y, placement, middlewareData} = await computePosition(target, tooltip, {
					placement: position,
					middleware,
				});
				if (
					!mountedRef.current ||
					!shouldRenderTooltipRef.current ||
					positionRequestIdRef.current !== requestId ||
					targetRef.current !== target ||
					tooltipRef.current !== tooltip
				) {
					return;
				}
				setTooltipState((prev) => {
					const next = {
						x,
						y,
						arrowX: middlewareData.arrow?.x ?? 0,
						arrowY: middlewareData.arrow?.y ?? 0,
						isReady: true,
						actualPlacement: placement.split('-')[0] as TooltipPosition,
					};
					return prev.x !== next.x ||
						prev.y !== next.y ||
						prev.arrowX !== next.arrowX ||
						prev.arrowY !== next.arrowY ||
						prev.isReady !== next.isReady ||
						prev.actualPlacement !== next.actualPlacement
						? next
						: prev;
				});
				Object.assign(tooltip.style, {left: appZoomCssPx(x), top: appZoomCssPx(y)} as CSSStyleDeclaration);
			} catch (error) {
				logger.error('Error positioning tooltip:', error);
				if (tooltipRef.current) {
					tooltipRef.current.style.visibility = 'visible';
				}
			} finally {
				isCalculatingRef.current = false;
			}
		}, [hasRenderableContent, nudge, padding, position]);
		const updatePosition = useCallback(() => {
			cancelRaf();
			const ownerWindow = getTooltipOwnerWindow();
			rafWindowRef.current = ownerWindow;
			rafRef.current = ownerWindow.requestAnimationFrame(() => {
				rafRef.current = null;
				rafWindowRef.current = null;
				void updatePositionNow();
			});
		}, [updatePositionNow, cancelRaf, getTooltipOwnerWindow]);
		const getVisibilityDriverDelayMs = useCallback(() => {
			const scrollSuppressRemainingMs = getTooltipScrollSuppressRemainingMs();
			switch (true) {
				case delay > 0 || scrollSuppressRemainingMs > 0:
					return Math.max(delay, scrollSuppressRemainingMs > 0 ? scrollSuppressRemainingMs + 1 : 0);
				default:
					return 0;
			}
		}, [delay]);
		const beginVisibilityDriver = useCallback(
			(driver: TooltipVisibilityDriver) => {
				refreshTooltipEnvironment();
				const pendingDelayMs = getVisibilityDriverDelayMs();
				pendingDelayMsRef.current = pendingDelayMs;
				switch (driver) {
					case 'hover':
						sendTooltipEvent({
							type: 'tooltip.hoverChanged',
							hovered: true,
							delay: pendingDelayMs > 0,
						});
						return;
					case 'focus':
						sendTooltipEvent({
							type: 'tooltip.focusChanged',
							focused: true,
							delay: pendingDelayMs > 0,
						});
						return;
				}
			},
			[getVisibilityDriverDelayMs, refreshTooltipEnvironment, sendTooltipEvent],
		);
		const endVisibilityDriver = useCallback(
			(driver: TooltipVisibilityDriver) => {
				switch (driver) {
					case 'hover':
						sendTooltipEvent({type: 'tooltip.hoverChanged', hovered: false, delay: false});
						return;
					case 'focus':
						sendTooltipEvent({type: 'tooltip.focusChanged', focused: false, delay: false});
						return;
				}
			},
			[sendTooltipEvent],
		);
		const showMobileToast = useCallback(() => {
			const textContent = getTextString();
			if (textContent) {
				ToastCommands.createToast({
					type: 'info',
					children: textContent,
					timeout: 3000,
				});
			}
		}, [getTextString]);
		const handleLongPressStart = useCallback(() => {
			if (!mobileLayout.enabled || !getTextString()) return;
			if (longPressTimeoutRef.current != null) {
				window.clearTimeout(longPressTimeoutRef.current);
			}
			longPressTimeoutRef.current = window.setTimeout(() => {
				showMobileToast();
			}, 500);
		}, [mobileLayout.enabled, showMobileToast, getTextString]);
		const handleLongPressEnd = useCallback(() => {
			if (longPressTimeoutRef.current != null) {
				window.clearTimeout(longPressTimeoutRef.current);
				longPressTimeoutRef.current = null;
			}
		}, []);
		useEffect(() => {
			sendTooltipEvent({type: 'tooltip.environmentChanged', environment: tooltipEnvironment});
		}, [sendTooltipEvent, tooltipEnvironment]);
		useEffect(() => {
			clearDelayTimer();
			if (tooltipStateValue !== 'delayed') return;
			const ownerWindow = getTooltipOwnerWindow();
			timerWindowRef.current = ownerWindow;
			timerRef.current = ownerWindow.setTimeout(() => {
				timerRef.current = null;
				timerWindowRef.current = null;
				pendingDelayMsRef.current = 0;
				sendTooltipEvent({type: 'tooltip.delayElapsed'});
			}, pendingDelayMsRef.current);
			return clearDelayTimer;
		}, [clearDelayTimer, getTooltipOwnerWindow, sendTooltipEvent, tooltipStateValue]);
		useLayoutEffect(() => {
			if (!shouldRenderTooltip || !targetRef.current || !tooltipRef.current) {
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
				return;
			}
			updatePosition();
			if (targetRef.current.ownerDocument.contains(targetRef.current)) {
				cleanupRef.current = autoUpdate(
					targetRef.current,
					tooltipRef.current,
					updatePosition,
					TOOLTIP_AUTO_UPDATE_OPTIONS,
				);
			}
			return () => {
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
			};
		}, [shouldRenderTooltip, updatePosition]);
		useEffect(() => {
			if (shouldRenderTooltip) return;
			cancelRaf();
			resetTooltipPosition();
			if (cleanupRef.current) {
				cleanupRef.current();
				cleanupRef.current = null;
			}
		}, [cancelRaf, resetTooltipPosition, shouldRenderTooltip]);
		useEffect(() => {
			mountedRef.current = true;
			return () => {
				mountedRef.current = false;
				positionRequestIdRef.current += 1;
				clearDelayTimer();
				if (longPressTimeoutRef.current != null) {
					window.clearTimeout(longPressTimeoutRef.current);
					longPressTimeoutRef.current = null;
				}
				clearPointerFocusTimeout();
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
				cancelRaf();
			};
		}, [cancelRaf, clearDelayTimer, clearPointerFocusTimeout]);
		useEffect(() => {
			if (!shouldRenderTooltip) return;
			return subscribeTooltipScrollHide(dismissTooltip);
		}, [dismissTooltip, shouldRenderTooltip]);
		useEffect(() => {
			if (!hasActiveVisibilityState) return;
			const closeIfHoverControlsDisabled = () => {
				refreshTooltipEnvironment();
				if (!canUseTooltipHoverControls()) {
					dismissTooltip();
				}
			};
			const ownerWindow = getTooltipOwnerWindow();
			const unsubscribe = subscribeWindowHoverControlsChange(closeIfHoverControlsDisabled, ownerWindow);
			ownerWindow.addEventListener('blur', dismissTooltip);
			return () => {
				unsubscribe();
				ownerWindow.removeEventListener('blur', dismissTooltip);
			};
		}, [
			canUseTooltipHoverControls,
			dismissTooltip,
			getTooltipOwnerWindow,
			hasActiveVisibilityState,
			refreshTooltipEnvironment,
		]);
		const mergedRef = useMergeRefs([targetRef, childSupportsRef ? childRef : null]);
		const wrapperRef = useMergeRefs([targetRef]);
		const hasChild = child != null;
		useEffect(() => {
			if (!openOnMountHover || !hasChild || mobileLayout.enabled || isDisabled) return;
			const target = targetRef.current;
			if (!target || !target.matches(':hover')) return;
			beginVisibilityDriver('hover');
		}, [beginVisibilityDriver, hasChild, isDisabled, mobileLayout.enabled, openOnMountHover]);
		useEffect(() => {
			if (!hasChild || mobileLayout.enabled) return;
			const target = targetRef.current;
			if (!target) return;
			if (target.ownerDocument === document) return;
			const handleNativePointerEnter = () => {
				if (isDisabled) {
					endVisibilityDriver('hover');
					return;
				}
				beginVisibilityDriver('hover');
			};
			const handleNativePointerLeave = () => {
				endVisibilityDriver('hover');
			};
			const handleNativeClick = () => {
				dismissTooltip();
			};
			target.addEventListener('pointerenter', handleNativePointerEnter);
			target.addEventListener('pointerleave', handleNativePointerLeave);
			target.addEventListener('click', handleNativeClick);
			return () => {
				target.removeEventListener('pointerenter', handleNativePointerEnter);
				target.removeEventListener('pointerleave', handleNativePointerLeave);
				target.removeEventListener('click', handleNativeClick);
			};
		}, [beginVisibilityDriver, dismissTooltip, endVisibilityDriver, hasChild, isDisabled, mobileLayout.enabled]);
		useEffect(() => {
			if (!tooltipModel.hovered || tooltipModel.dismissed || mobileLayout.enabled) return;
			const ownerWindow = getTooltipOwnerWindow();
			let frameId: number | null = null;
			const verifyTargetHover = () => {
				const target = targetRef.current;
				if (!target || !target.matches(':hover')) {
					endVisibilityDriver('hover');
					frameId = null;
					return;
				}
				frameId = ownerWindow.requestAnimationFrame(verifyTargetHover);
			};
			frameId = ownerWindow.requestAnimationFrame(verifyTargetHover);
			return () => {
				if (frameId != null) {
					ownerWindow.cancelAnimationFrame(frameId);
				}
			};
		}, [
			endVisibilityDriver,
			getTooltipOwnerWindow,
			mobileLayout.enabled,
			tooltipModel.dismissed,
			tooltipModel.hovered,
		]);
		if (!child) return null;
		const describedBy = shouldRenderTooltip
			? [childProps['aria-describedby'], tooltipId].filter(Boolean).join(' ')
			: childProps['aria-describedby'];
		const shouldUseTooltipAsName =
			typeof text === 'string' &&
			!childProps['aria-label'] &&
			!childProps['aria-labelledby'] &&
			!nodeHasAccessibleText(childProps.children);
		const tooltipNameProps = shouldUseTooltipAsName ? {'aria-label': text} : {};
		const mobileEventHandlers = mobileLayout.enabled
			? {
					onTouchStart: (event: React.TouchEvent<HTMLElement>) => {
						handleLongPressStart();
						childTouchStart?.(event);
					},
					onTouchEnd: (event: React.TouchEvent<HTMLElement>) => {
						handleLongPressEnd();
						childTouchEnd?.(event);
					},
					onTouchCancel: (event: React.TouchEvent<HTMLElement>) => {
						handleLongPressEnd();
						childTouchCancel?.(event);
					},
					onClick: (event: React.MouseEvent<HTMLElement>) => {
						childClick?.(event);
					},
				}
			: {};
		const desktopEventHandlers = !mobileLayout.enabled
			? {
					onMouseEnter: (event: React.MouseEvent<HTMLElement>) => {
						if (isDisabled) {
							endVisibilityDriver('hover');
							childMouseEnter?.(event);
							return;
						}
						beginVisibilityDriver('hover');
						childMouseEnter?.(event);
					},
					onMouseLeave: (event: React.MouseEvent<HTMLElement>) => {
						endVisibilityDriver('hover');
						childMouseLeave?.(event);
					},
					onClick: (event: React.MouseEvent<HTMLElement>) => {
						dismissTooltip();
						childClick?.(event);
					},
					onPointerDownCapture: (event: React.PointerEvent<HTMLElement>) => {
						markPointerFocus();
						childPointerDownCapture?.(event);
					},
					onMouseDownCapture: (event: React.MouseEvent<HTMLElement>) => {
						markPointerFocus();
						childMouseDownCapture?.(event);
					},
					onFocus: (event: React.FocusEvent<HTMLElement>) => {
						const shouldIgnoreFocus = ignoreNextFocusRef.current;
						ignoreNextFocusRef.current = false;
						if (shouldIgnoreFocus) {
							clearPointerFocusTimeout();
							endVisibilityDriver('focus');
						} else {
							beginVisibilityDriver('focus');
						}
						if (isDisabled || shouldIgnoreFocus) {
							dismissTooltip();
						}
						childFocus?.(event);
					},
					onBlur: (event: React.FocusEvent<HTMLElement>) => {
						endVisibilityDriver('focus');
						childBlur?.(event);
					},
					onKeyDown: (event: React.KeyboardEvent<HTMLElement>) => {
						if (event.key === 'Escape' && shouldRenderTooltip) {
							dismissTooltip();
						}
						childKeyDown?.(event);
					},
				}
			: {};
		const childWithHandlers = React.cloneElement(child, {
			...(childSupportsRef ? {ref: mergedRef} : {}),
			'aria-describedby': describedBy || undefined,
			...tooltipNameProps,
			...desktopEventHandlers,
			...mobileEventHandlers,
		});
		const triggerNode = childSupportsRef ? (
			childWithHandlers
		) : (
			<span className={styles.triggerWrapper} ref={wrapperRef} data-flx="ui.tooltip.tooltip.trigger-wrapper">
				{childWithHandlers}
			</span>
		);
		return (
			<>
				{triggerNode}
				{shouldRenderTooltip && (
					<FloatingPortal root={portalRoot || undefined} data-flx="ui.tooltip.tooltip.floating-portal">
						<AnimatePresence mode="wait" data-flx="ui.tooltip.tooltip.animate-presence">
							<motion.div
								key="tooltip"
								id={tooltipId}
								role="tooltip"
								ref={(node: HTMLDivElement | null) => {
									tooltipRef.current = node;
									if (node && targetRef.current) {
										updatePosition();
									}
								}}
								style={{
									position: 'fixed',
									left: appZoomLayoutPx(tooltipState.x),
									top: appZoomLayoutPx(tooltipState.y),
									zIndex: 'var(--z-index-tooltip)',
									visibility: tooltipState.isReady ? 'visible' : 'hidden',
								}}
								data-flx="ui.tooltip.tooltip.tooltip"
								{...tooltipMotion}
							>
								<TooltipContainer
									text={text}
									type={type}
									position={tooltipState.actualPlacement}
									maxWidth={maxWidth}
									size={size}
									arrowRef={arrowRef}
									arrowX={tooltipState.arrowX}
									arrowY={tooltipState.arrowY}
									data-flx="ui.tooltip.tooltip.tooltip-container"
								/>
							</motion.div>
						</AnimatePresence>
					</FloatingPortal>
				)}
			</>
		);
	},
);
