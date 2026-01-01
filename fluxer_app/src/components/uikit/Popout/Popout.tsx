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

import {autorun} from 'mobx';
import React from 'react';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import {type PopoutKey, type PopoutPosition, usePopoutKeyContext} from '~/components/uikit/Popout';
import type {TooltipPosition} from '~/components/uikit/Tooltip';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import type {ComponentActionType} from '~/lib/ComponentDispatch';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import PopoutStore from '~/stores/PopoutStore';
import {getExtendedWindow, supportsRequestIdleCallback} from '~/types/browser';
import {elementSupportsRef} from '~/utils/react';
import styles from './Popout.module.css';

let currentId = 1;

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
	offsetMainAxis?: number;
	offsetCrossAxis?: number;
	animationType?: 'smooth' | 'none';
	containerClass?: string;
	preventInvert?: boolean;
	hoverDelay?: number;
	toggleClose?: boolean;
	subscribeTo?: ComponentActionType;
	onOpen?: () => void;
	onClose?: () => void;
	onCloseRequest?: (event?: Event) => boolean;
	returnFocusRef?: React.RefObject<HTMLElement | null>;
	closeOnChildrenUnmount?: boolean;
	disableBackdrop?: boolean;
}

interface OpenPopoutOptions extends Partial<PopoutProps> {
	hoverMode?: boolean;
	onContentMouseEnter?: () => void;
	onContentMouseLeave?: () => void;
}

export const openPopout = (target: HTMLElement, props: OpenPopoutOptions, key: string | number, clickPos = 0) => {
	PopoutActionCreators.open({
		key: key || currentId++,
		dependsOn: props.dependsOn,
		position: props.position!,
		render: props.render as (props: {popoutKey: PopoutKey; onClose: () => void}) => React.ReactNode,
		target,
		zIndexBoost: props.zIndexBoost,
		shouldAutoUpdate: props.shouldAutoUpdate,
		offsetMainAxis: props.offsetMainAxis,
		offsetCrossAxis: props.offsetCrossAxis,
		animationType: props.animationType,
		clickPos,
		containerClass: props.containerClass,
		preventInvert: props.preventInvert,
		onOpen: props.onOpen,
		onClose: props.onClose,
		onCloseRequest: props.onCloseRequest,
		returnFocusRef: props.returnFocusRef,
		disableBackdrop: props.disableBackdrop,
		hoverMode: props.hoverMode,
		onContentMouseEnter: props.onContentMouseEnter,
		onContentMouseLeave: props.onContentMouseLeave,
	});
};

export const Popout = React.forwardRef<HTMLElement, PopoutProps>((props, ref) => {
	const [state, setState] = React.useState({
		id: props.uniqueId || currentId++,
		isOpen: false,
		lastAction: null as 'open' | 'close' | null,
		lastValidChildren: null as React.ReactNode,
	});

	const parentPopoutKey = usePopoutKeyContext();

	const targetRef = React.useRef<HTMLElement | null>(null);
	const isTriggerHoveringRef = React.useRef(false);
	const isContentHoveringRef = React.useRef(false);
	const hoverTimerRef = React.useRef<NodeJS.Timeout | null>(null);
	const closeTimerRef = React.useRef<NodeJS.Timeout | null>(null);

	React.useEffect(() => {
		if (props.children) {
			setState((prev) => ({...prev, lastValidChildren: props.children}));
		}
	}, [props.children]);

	const clearTimers = React.useCallback(() => {
		if (hoverTimerRef.current) {
			clearTimeout(hoverTimerRef.current);
			hoverTimerRef.current = null;
		}
		if (closeTimerRef.current) {
			clearTimeout(closeTimerRef.current);
			closeTimerRef.current = null;
		}
	}, []);

	React.useEffect(() => {
		const dispose = autorun(() => {
			const isOpenInStore = Boolean(PopoutStore.popouts[state.id]);
			setState((prev) => ({
				...prev,
				isOpen: isOpenInStore,
				lastAction: null,
			}));
		});

		return () => dispose();
	}, [state.id]);

	React.useEffect(() => {
		return () => {
			clearTimers();

			if (state.isOpen) {
				PopoutActionCreators.close(state.id);
			}

			const cleanupPortals = () => {
				const portals = document.querySelectorAll(`[data-floating-ui-portal][aria-describedby="${state.id}"]`);
				if (!portals.length) return;
				for (let i = 0; i < portals.length; i++) {
					const portal = portals[i];
					portal.parentNode?.removeChild(portal);
				}
			};

			if (supportsRequestIdleCallback(window)) {
				const extendedWindow = getExtendedWindow();
				extendedWindow.requestIdleCallback?.(cleanupPortals, {timeout: 200});
			} else {
				requestAnimationFrame(cleanupPortals);
			}
		};
	}, []);

	const close = React.useCallback(
		(event?: Event) => {
			if (props.onCloseRequest && !props.onCloseRequest(event)) {
				return;
			}

			if (state.lastAction !== 'close') {
				setState((prev) => ({...prev, lastAction: 'close'}));
				PopoutActionCreators.close(state.id);
				props.onClose?.();
			}

			if (props.returnFocusRef?.current) {
				props.returnFocusRef.current.focus();
			}
		},
		[state.id, state.lastAction, props],
	);

	const scheduleClose = React.useCallback(() => {
		if (closeTimerRef.current) {
			clearTimeout(closeTimerRef.current);
		}
		closeTimerRef.current = setTimeout(() => {
			const hasActiveDependents = PopoutStore.hasDependents(state.id);
			if (!isTriggerHoveringRef.current && !isContentHoveringRef.current && !hasActiveDependents) {
				close();
			}
		}, 300);
	}, [close, state.id]);

	const handleContentMouseEnter = React.useCallback(() => {
		isContentHoveringRef.current = true;
		if (closeTimerRef.current) {
			clearTimeout(closeTimerRef.current);
			closeTimerRef.current = null;
		}
	}, []);

	const handleContentMouseLeave = React.useCallback(() => {
		isContentHoveringRef.current = false;
		if (props.hoverDelay != null) {
			scheduleClose();
		}
	}, [props.hoverDelay, scheduleClose]);

	const open = React.useCallback(
		(clickPos?: number) => {
			if (!targetRef.current) return;

			if (state.lastAction !== 'open') {
				setState((prev) => ({...prev, lastAction: 'open'}));
				const isHoverMode = props.hoverDelay != null;
				const effectiveDependsOn = props.dependsOn ?? (parentPopoutKey != null ? parentPopoutKey : undefined);
				openPopout(
					targetRef.current,
					{
						...props,
						dependsOn: effectiveDependsOn,
						hoverMode: isHoverMode,
						onContentMouseEnter: isHoverMode ? handleContentMouseEnter : undefined,
						onContentMouseLeave: isHoverMode ? handleContentMouseLeave : undefined,
						disableBackdrop: isHoverMode ? true : props.disableBackdrop,
					},
					state.id,
					clickPos,
				);
				props.onOpen?.();
			}
		},
		[state.id, state.lastAction, props, parentPopoutKey, handleContentMouseEnter, handleContentMouseLeave],
	);

	const toggle = React.useCallback(
		(clickPos?: number) => {
			if (PopoutStore.isOpen(state.id)) {
				close();
			} else {
				open(clickPos);
			}
		},
		[state.id, open, close],
	);

	const handleHover = React.useCallback(
		(isEntering: boolean) => {
			if (props.hoverDelay == null) return;

			clearTimers();
			isTriggerHoveringRef.current = isEntering;

			if (isEntering) {
				hoverTimerRef.current = setTimeout(() => {
					open();
				}, props.hoverDelay);
			} else {
				scheduleClose();
			}
		},
		[props.hoverDelay, open, clearTimers, scheduleClose],
	);

	React.useEffect(() => {
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
	const {onClick, onMouseEnter, onMouseLeave, onKeyDown} = childProps;

	const handleKeyboardToggle = (event: React.KeyboardEvent<HTMLElement>) => {
		if (event.defaultPrevented) return;
		const key = event.key;
		if (key !== 'Enter' && key !== ' ' && key !== 'Spacebar') return;
		if (event.metaKey || event.altKey || event.ctrlKey || event.shiftKey) return;
		const target = event.currentTarget;
		if (target instanceof HTMLElement) {
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

	const enhancedChild = React.cloneElement(child, {
		'aria-describedby': popoutId,
		'aria-expanded': state.isOpen,
		'aria-controls': state.isOpen ? popoutId : undefined,
		'aria-haspopup': true,
		'aria-label': typeof props.tooltip === 'string' ? props.tooltip : undefined,
		onClick: (event: React.MouseEvent<HTMLElement>) => {
			const clickPos = event.pageX - event.currentTarget.getBoundingClientRect().left;
			event.preventDefault();
			event.stopPropagation();
			toggle(clickPos);
			onClick?.(event);
		},
		onMouseEnter: (event: React.MouseEvent<HTMLElement>) => {
			handleHover(true);
			onMouseEnter?.(event);
		},
		onMouseLeave: (event: React.MouseEvent<HTMLElement>) => {
			handleHover(false);
			onMouseLeave?.(event);
		},
		onKeyDown: (event: React.KeyboardEvent<HTMLElement>) => {
			handleKeyboardToggle(event);
			onKeyDown?.(event);
		},
		...(childSupportsRef ? {ref: mergedChildRef} : {}),
	});
	const trigger = childSupportsRef ? (
		enhancedChild
	) : (
		<span className={styles.triggerWrapper} ref={wrapperRef}>
			{enhancedChild}
		</span>
	);

	return props.tooltip ? (
		<Tooltip text={props.tooltip} position={props.tooltipPosition} align={props.tooltipAlign}>
			{trigger}
		</Tooltip>
	) : (
		trigger
	);
});
