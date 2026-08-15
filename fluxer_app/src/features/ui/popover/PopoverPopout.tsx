// SPDX-License-Identifier: AGPL-3.0-or-later

import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {ComponentActionType} from '@app/features/platform/utils/ComponentBus';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {
	type PopoutAnimationType,
	type PopoutKey,
	type PopoutPosition,
	type PopoutReferenceRect,
	usePopoutKeyContext,
} from '@app/features/ui/popover';
import styles from '@app/features/ui/popover/PopoverPopout.module.css';
import {schedulePopoutPortalCleanup} from '@app/features/ui/popover/PopoverPortalCleanup';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import PopoutState from '@app/features/ui/state/Popout';
import type {TooltipPosition} from '@app/features/ui/tooltip/Tooltip';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {
	canUseWindowFocusedHoverControls,
	subscribeWindowHoverControlsChange,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {elementSupportsRef} from '@app/lib/react';
import {autorun} from 'mobx';
import React, {useCallback, useEffect, useRef, useState} from 'react';

let currentId = 1;
const CLICK_DRAG_TOLERANCE_PX = 3;
const MENU_ITEM_SELECTOR =
	'[data-roving-focus="true"], [role="menuitem"], [role="menuitemcheckbox"], [role="menuitemradio"]';

interface PopoutProps {
	children?: React.ReactNode;
	render?: (props: {popoutKey: PopoutKey; onClose: () => void}) => React.ReactNode;
	position?: PopoutPosition;
	dependsOn?: string | number;
	uniqueId?: string | number;
	tooltip?: string | (() => React.ReactNode);
	tooltipPosition?: TooltipPosition;
	tooltipAlign?: 'center' | 'top' | 'bottom' | 'left' | 'right';
	zIndexBoost?: number;
	shouldAutoUpdate?: boolean;
	freezePosition?: boolean;
	offsetMainAxis?: number;
	offsetCrossAxis?: number;
	animationType?: PopoutAnimationType;
	constrainHeight?: boolean;
	containerClass?: string;
	preventInvert?: boolean;
	hoverDelay?: number;
	hoverCloseDelay?: number;
	subscribeTo?: ComponentActionType;
	onOpen?: () => void;
	onClose?: () => void;
	onCloseRequest?: (event?: Event) => boolean;
	returnFocusRef?: React.RefObject<HTMLElement | null>;
	closeOnChildrenUnmount?: boolean;
	disableBackdrop?: boolean;
	keepOpenOnTargetUnmount?: boolean;
	shouldOpenOnClick?: (event: React.MouseEvent<HTMLElement>) => boolean;
}

interface OpenPopoutOptions extends Partial<PopoutProps> {
	hoverMode?: boolean;
	onContentMouseEnter?: () => void;
	onContentMouseLeave?: () => void;
}

interface ClickStart {
	x: number;
	y: number;
	button: number;
}

type PopoutOpenMode = 'click' | 'hover';

export const openPopout = (target: HTMLElement, props: OpenPopoutOptions, key: string | number, clickPos = 0) => {
	PopoutCommands.open({
		key: key || currentId++,
		dependsOn: props.dependsOn,
		position: props.position!,
		render: props.render as (props: {popoutKey: PopoutKey; onClose: () => void}) => React.ReactNode,
		target,
		frozenTargetRect: props.freezePosition ? getTargetRectSnapshot(target) : undefined,
		zIndexBoost: props.zIndexBoost,
		shouldAutoUpdate: props.shouldAutoUpdate,
		offsetMainAxis: props.offsetMainAxis,
		offsetCrossAxis: props.offsetCrossAxis,
		animationType: props.animationType,
		constrainHeight: props.constrainHeight,
		clickPos,
		containerClass: props.containerClass,
		preventInvert: props.preventInvert,
		onOpen: props.onOpen,
		onClose: props.onClose,
		onCloseRequest: props.onCloseRequest,
		returnFocusRef: props.returnFocusRef,
		disableBackdrop: props.disableBackdrop,
		keepOpenOnTargetUnmount: props.keepOpenOnTargetUnmount,
		hoverMode: props.hoverMode,
		onContentMouseEnter: props.onContentMouseEnter,
		onContentMouseLeave: props.onContentMouseLeave,
	});
};

function getTargetRectSnapshot(target: HTMLElement): PopoutReferenceRect {
	const rect = target.getBoundingClientRect();
	return {
		x: rect.x,
		y: rect.y,
		width: rect.width,
		height: rect.height,
		top: rect.top,
		right: rect.right,
		bottom: rect.bottom,
		left: rect.left,
	};
}

function hasActiveTextSelectionInsideTarget(target: HTMLElement): boolean {
	const selection = target.ownerDocument.getSelection();
	if (!selection || selection.isCollapsed || selection.rangeCount === 0) {
		return false;
	}
	for (let i = 0; i < selection.rangeCount; i++) {
		const range = selection.getRangeAt(i);
		if (range.collapsed) {
			continue;
		}
		try {
			if (range.intersectsNode(target)) {
				return true;
			}
		} catch {
			const anchorNode = selection.anchorNode;
			const focusNode = selection.focusNode;
			return Boolean((anchorNode && target.contains(anchorNode)) || (focusNode && target.contains(focusNode)));
		}
	}
	return false;
}

function focusPopoutMenuItem(popoutId: string, placement: 'first' | 'last', ownerDocument: Document): boolean {
	const root = ownerDocument.getElementById(popoutId);
	if (!root) return false;
	const menu = root.querySelector<HTMLElement>('[role="menu"]');
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
}

function schedulePopoutMenuItemFocus(popoutId: string, placement: 'first' | 'last', ownerDocument: Document): void {
	const focus = () => {
		if (focusPopoutMenuItem(popoutId, placement, ownerDocument)) return;
		window.setTimeout(() => focusPopoutMenuItem(popoutId, placement, ownerDocument), 0);
	};
	if (typeof window.requestAnimationFrame === 'function') {
		window.requestAnimationFrame(focus);
		return;
	}
	window.setTimeout(focus, 0);
}

export const Popout = React.forwardRef<HTMLElement, PopoutProps>((props, ref) => {
	interface PopoutState {
		id: string | number;
		isOpen: boolean;
		lastAction: 'open' | 'close' | null;
		lastValidChildren: React.ReactNode;
	}
	const [state, setState] = useState<PopoutState>({
		id: props.uniqueId || currentId++,
		isOpen: false,
		lastAction: null,
		lastValidChildren: null,
	});
	const parentPopoutKey = usePopoutKeyContext();
	const targetRef = useRef<HTMLElement | null>(null);
	const isTriggerHoveringRef = useRef(false);
	const isContentHoveringRef = useRef(false);
	const hoverTimerRef = useRef<NodeJS.Timeout | null>(null);
	const closeTimerRef = useRef<NodeJS.Timeout | null>(null);
	const clickStartRef = useRef<ClickStart | null>(null);
	const openModeRef = useRef<PopoutOpenMode | null>(null);
	useEffect(() => {
		if (props.children) {
			setState((prev) => ({...prev, lastValidChildren: props.children}));
		}
	}, [props.children]);
	const clearTimers = useCallback(() => {
		if (hoverTimerRef.current) {
			clearTimeout(hoverTimerRef.current);
			hoverTimerRef.current = null;
		}
		if (closeTimerRef.current) {
			clearTimeout(closeTimerRef.current);
			closeTimerRef.current = null;
		}
	}, []);
	useEffect(() => {
		const dispose = autorun(() => {
			const isOpenInState = Boolean(PopoutState.popouts[state.id]);
			if (!isOpenInState) {
				openModeRef.current = null;
			}
			setState((prev) => ({
				...prev,
				isOpen: isOpenInState,
				lastAction: null,
			}));
		});
		return () => dispose();
	}, [state.id]);
	useEffect(() => {
		return () => {
			clearTimers();
			if (state.isOpen) {
				PopoutCommands.close(state.id);
			}
			schedulePopoutPortalCleanup(state.id);
		};
	}, []);
	const close = useCallback(
		(event?: Event) => {
			if (props.onCloseRequest && !props.onCloseRequest(event)) {
				return;
			}
			openModeRef.current = null;
			clearTimers();
			const shouldReturnFocus = PopoutState.shouldReturnFocus(state.id);
			if (state.lastAction !== 'close') {
				setState((prev) => ({...prev, lastAction: 'close'}));
				PopoutCommands.close(state.id);
			}
			if (shouldReturnFocus && props.returnFocusRef?.current) {
				props.returnFocusRef.current.focus();
			}
		},
		[state.id, state.lastAction, props, clearTimers],
	);
	const scheduleClose = useCallback(() => {
		if (closeTimerRef.current) {
			clearTimeout(closeTimerRef.current);
		}
		closeTimerRef.current = setTimeout(() => {
			const hasActiveDependents = PopoutState.hasDependents(state.id);
			if (
				openModeRef.current === 'hover' &&
				!isTriggerHoveringRef.current &&
				!isContentHoveringRef.current &&
				!hasActiveDependents
			) {
				close();
			}
		}, props.hoverCloseDelay ?? 300);
	}, [close, state.id, props.hoverCloseDelay]);
	const handleContentMouseEnter = useCallback(() => {
		isContentHoveringRef.current = true;
		if (closeTimerRef.current) {
			clearTimeout(closeTimerRef.current);
			closeTimerRef.current = null;
		}
	}, []);
	const handleContentMouseLeave = useCallback(() => {
		isContentHoveringRef.current = false;
		if (props.hoverDelay != null) {
			scheduleClose();
		}
	}, [props.hoverDelay, scheduleClose]);
	const open = useCallback(
		(clickPos?: number, mode: PopoutOpenMode = 'click') => {
			if (!targetRef.current) return;
			const isAlreadyOpen = PopoutState.isOpen(state.id);
			if (mode === 'hover' && isAlreadyOpen) {
				return;
			}
			const hasHoverInteraction = props.hoverDelay != null;
			const isHoverMode = mode === 'hover' && hasHoverInteraction;
			openModeRef.current = isHoverMode ? 'hover' : 'click';
			if (state.lastAction !== 'open' || isAlreadyOpen) {
				setState((prev) => ({...prev, lastAction: 'open'}));
				const effectiveDependsOn = props.dependsOn ?? (parentPopoutKey != null ? parentPopoutKey : undefined);
				openPopout(
					targetRef.current,
					{
						...props,
						onOpen: isAlreadyOpen ? undefined : props.onOpen,
						dependsOn: effectiveDependsOn,
						hoverMode: isHoverMode,
						onContentMouseEnter: isHoverMode ? handleContentMouseEnter : undefined,
						onContentMouseLeave: isHoverMode ? handleContentMouseLeave : undefined,
						disableBackdrop: hasHoverInteraction ? true : props.disableBackdrop,
					},
					state.id,
					clickPos,
				);
				if (!isAlreadyOpen) {
					props.onOpen?.();
				}
			}
		},
		[state.id, state.lastAction, props, parentPopoutKey, handleContentMouseEnter, handleContentMouseLeave],
	);
	const toggle = useCallback(
		(clickPos?: number) => {
			if (PopoutState.isOpen(state.id)) {
				if (props.hoverDelay != null && openModeRef.current === 'hover') {
					clearTimers();
					open(clickPos, 'click');
				} else {
					close();
				}
			} else {
				open(clickPos, 'click');
			}
		},
		[state.id, props.hoverDelay, clearTimers, open, close],
	);
	const handleHover = useCallback(
		(isEntering: boolean) => {
			if (props.hoverDelay == null) return;
			clearTimers();
			isTriggerHoveringRef.current = isEntering;
			if (isEntering) {
				if (openModeRef.current === 'click') {
					return;
				}
				if (!canUseWindowFocusedHoverControls()) {
					scheduleClose();
					return;
				}
				hoverTimerRef.current = setTimeout(() => {
					if (canUseWindowFocusedHoverControls()) {
						open(undefined, 'hover');
					}
				}, props.hoverDelay);
			} else {
				if (openModeRef.current === 'click') {
					return;
				}
				scheduleClose();
			}
		},
		[props.hoverDelay, open, clearTimers, scheduleClose],
	);
	useEffect(() => {
		if (props.hoverDelay == null) return;
		const closeIfHoverControlsDisabled = () => {
			if (!canUseWindowFocusedHoverControls() && openModeRef.current === 'hover') {
				isTriggerHoveringRef.current = false;
				isContentHoveringRef.current = false;
				clearTimers();
				close();
			}
		};
		const unsubscribe = subscribeWindowHoverControlsChange(closeIfHoverControlsDisabled);
		window.addEventListener('blur', closeIfHoverControlsDisabled);
		return () => {
			unsubscribe();
			window.removeEventListener('blur', closeIfHoverControlsDisabled);
		};
	}, [props.hoverDelay, clearTimers, close]);
	useEffect(() => {
		if (!props.subscribeTo) return;
		const handler = () => toggle();
		ComponentDispatch.subscribe(props.subscribeTo, handler);
		return () => ComponentDispatch.unsubscribe(props.subscribeTo!, handler);
	}, [props.subscribeTo, toggle]);
	const childToRender =
		(props.children as React.ReactNode) || (!props.closeOnChildrenUnmount ? state.lastValidChildren : null);
	type PopoutChildProps = React.HTMLAttributes<HTMLElement> & {ref?: React.Ref<HTMLElement>};
	const child =
		childToRender && React.isValidElement<PopoutChildProps>(childToRender) ? React.Children.only(childToRender) : null;
	const childSupportsRef = child ? elementSupportsRef(child) : false;
	const childRef = childSupportsRef && child ? (child.props.ref ?? null) : null;
	const mergedChildRef = useMergeRefs(childSupportsRef ? [ref, targetRef, childRef] : [ref, targetRef]);
	const wrapperRef = useMergeRefs([ref, targetRef]);
	const popoutId = String(state.id);
	if (!props.children) {
		return state.isOpen && props.render ? props.render({popoutKey: state.id, onClose: close}) : null;
	}
	if (!childToRender || !child) {
		if (state.isOpen) {
			close();
		}
		return null;
	}
	const childProps = child.props as React.HTMLAttributes<HTMLElement>;
	const {onClick, onMouseDown, onMouseEnter, onMouseLeave, onKeyDown, onFocus, onBlur} = childProps;
	const popoutTriggerProps = {
		'aria-expanded': state.isOpen,
		'aria-controls': state.isOpen ? popoutId : childProps['aria-controls'],
		'aria-haspopup': childProps['aria-haspopup'] ?? true,
		...(typeof props.tooltip === 'string' && !childProps['aria-label'] ? {'aria-label': props.tooltip} : {}),
	};
	const handleKeyboardToggle = (event: React.KeyboardEvent<HTMLElement>) => {
		if (event.defaultPrevented) return;
		const key = event.key;
		if (!isKeyboardActivationKey(key)) return;
		if (event.metaKey || event.altKey || event.ctrlKey || event.shiftKey) return;
		const target = event.currentTarget;
		const ownerWindow = target.ownerDocument.defaultView;
		if (ownerWindow && target instanceof ownerWindow.HTMLElement) {
			const nativeTag = target.tagName;
			if (
				nativeTag === 'BUTTON' ||
				nativeTag === 'A' ||
				nativeTag === 'INPUT' ||
				nativeTag === 'TEXTAREA' ||
				nativeTag === 'SUMMARY'
			) {
				return;
			}
		}
		event.preventDefault();
		event.stopPropagation();
		toggle();
	};
	const handleMenuArrowKey = (event: React.KeyboardEvent<HTMLElement>) => {
		if (event.defaultPrevented) return false;
		if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp') return false;
		if (event.metaKey || event.altKey || event.ctrlKey || event.shiftKey) return false;
		if (popoutTriggerProps['aria-haspopup'] !== 'menu') return false;
		event.preventDefault();
		event.stopPropagation();
		if (!state.isOpen) {
			open(undefined, 'click');
		}
		schedulePopoutMenuItemFocus(
			popoutId,
			event.key === 'ArrowUp' ? 'last' : 'first',
			event.currentTarget.ownerDocument,
		);
		return true;
	};
	const handleMouseDown = (event: React.MouseEvent<HTMLElement>) => {
		clickStartRef.current = {
			x: event.clientX,
			y: event.clientY,
			button: event.button,
		};
		onMouseDown?.(event);
	};
	const consumeDragClick = (event: React.MouseEvent<HTMLElement>) => {
		const clickStart = clickStartRef.current;
		clickStartRef.current = null;
		if (!clickStart || clickStart.button !== event.button) {
			return false;
		}
		return (
			Math.abs(event.clientX - clickStart.x) > CLICK_DRAG_TOLERANCE_PX ||
			Math.abs(event.clientY - clickStart.y) > CLICK_DRAG_TOLERANCE_PX
		);
	};
	const enhancedChild = React.cloneElement(child, {
		...popoutTriggerProps,
		onClick: (event: React.MouseEvent<HTMLElement>) => {
			const shouldOpen =
				!consumeDragClick(event) &&
				!hasActiveTextSelectionInsideTarget(event.currentTarget) &&
				(props.shouldOpenOnClick?.(event) ?? true);
			if (shouldOpen) {
				const clickPos = event.pageX - event.currentTarget.getBoundingClientRect().left;
				event.preventDefault();
				event.stopPropagation();
				toggle(clickPos);
			}
			onClick?.(event);
		},
		onMouseDown: handleMouseDown,
		onMouseEnter: (event: React.MouseEvent<HTMLElement>) => {
			handleHover(true);
			onMouseEnter?.(event);
		},
		onMouseLeave: (event: React.MouseEvent<HTMLElement>) => {
			handleHover(false);
			onMouseLeave?.(event);
		},
		onKeyDown: (event: React.KeyboardEvent<HTMLElement>) => {
			if (handleMenuArrowKey(event)) {
				return;
			}
			handleKeyboardToggle(event);
			if (event.key === 'Escape' && state.isOpen) {
				event.preventDefault();
				event.stopPropagation();
				close();
			}
			onKeyDown?.(event);
		},
		onFocus: (event: React.FocusEvent<HTMLElement>) => {
			if (props.hoverDelay != null && KeyboardMode.keyboardModeEnabled) {
				handleHover(true);
			}
			onFocus?.(event);
		},
		onBlur: (event: React.FocusEvent<HTMLElement>) => {
			if (props.hoverDelay != null && !event.currentTarget.contains(event.relatedTarget as Node | null)) {
				handleHover(false);
			}
			onBlur?.(event);
		},
		...(childSupportsRef ? {ref: mergedChildRef} : {}),
	});
	const trigger = childSupportsRef ? (
		enhancedChild
	) : (
		<span className={styles.triggerWrapper} ref={wrapperRef} data-flx="ui.popover.popout.trigger-wrapper">
			{enhancedChild}
		</span>
	);
	return props.tooltip ? (
		<Tooltip
			text={props.tooltip}
			position={props.tooltipPosition}
			align={props.tooltipAlign}
			data-flx="ui.popover.popout.tooltip"
		>
			{trigger}
		</Tooltip>
	) : (
		trigger
	);
});
