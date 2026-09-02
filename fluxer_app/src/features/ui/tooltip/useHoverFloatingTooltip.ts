// SPDX-License-Identifier: AGPL-3.0-or-later

import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import type {Placement, ReferenceType} from '@floating-ui/react';
import {
	autoUpdate,
	flip,
	offset,
	safePolygon,
	shift,
	useFloating,
	useFocus,
	useHover,
	useInteractions,
} from '@floating-ui/react';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {useExclusiveTooltip} from './TooltipExclusivity';
import {isTooltipHandoffWarm, markTooltipOpen} from './TooltipHandoff';
import {subscribeTooltipScrollHide} from './TooltipScrollCoordinator';

export interface HoverFloatingTooltipState {
	x: number;
	y: number;
	isOpen: boolean;
	isReady: boolean;
}

export interface HoverFloatingTooltipResult {
	targetRef: React.RefCallback<HTMLElement>;
	floatingRef: React.RefCallback<HTMLDivElement>;
	state: HoverFloatingTooltipState;
	updatePosition: () => void;
	show: () => void;
	hide: () => void;
	referenceProps: Record<string, unknown>;
	floatingProps: Record<string, unknown>;
}

const CLOSE_DELAY_MS = 150;
const SAFE_POLYGON_BUFFER_PX = 8;
const HOVER_FLOATING_AUTO_UPDATE_OPTIONS = {
	ancestorScroll: false,
	ancestorResize: true,
	elementResize: true,
	layoutShift: true,
} as const;

const hoverFloatingAutoUpdate = (reference: ReferenceType, floating: HTMLElement, update: () => void): (() => void) =>
	autoUpdate(reference, floating, update, HOVER_FLOATING_AUTO_UPDATE_OPTIONS);

function omitRef<T extends {ref?: unknown}>(props: T): Omit<T, 'ref'> {
	const {ref, ...rest} = props;
	void ref;
	return rest;
}

export function useHoverFloatingTooltip(hoverDelay = 500, placement: Placement = 'top'): HoverFloatingTooltipResult {
	const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
	const [isOpen, setIsOpen] = useState(false);
	const middleware = useMemo(() => [offset(8), flip(), shift({padding: 8})], []);
	const {x, y, refs, context, update, isPositioned} = useFloating({
		open: isOpen,
		onOpenChange: setIsOpen,
		placement,
		strategy: 'fixed',
		middleware,
		whileElementsMounted: hoverFloatingAutoUpdate,
	});
	const hoverDelayConfig = useCallback(
		() => ({open: isTooltipHandoffWarm() ? 0 : hoverDelay, close: CLOSE_DELAY_MS}),
		[hoverDelay],
	);
	const hoverSafePolygon = useMemo(() => safePolygon({buffer: SAFE_POLYGON_BUFFER_PX, requireIntent: false}), []);
	const hover = useHover(context, {
		delay: hoverDelayConfig,
		handleClose: hoverSafePolygon,
		mouseOnly: true,
	});
	const focus = useFocus(context, {enabled: keyboardModeEnabled});
	const {getReferenceProps, getFloatingProps} = useInteractions([hover, focus]);
	const state = useMemo(
		(): HoverFloatingTooltipState => ({
			x: x == null ? 0 : appZoomLayoutPx(x),
			y: y == null ? 0 : appZoomLayoutPx(y),
			isOpen,
			isReady: isOpen && isPositioned && x != null && y != null,
		}),
		[isOpen, isPositioned, x, y],
	);
	const updatePosition = useCallback(() => {
		void update();
	}, [update]);
	const show = useCallback(() => {
		setIsOpen(true);
	}, []);
	const hide = useCallback(() => {
		setIsOpen(false);
	}, []);
	useExclusiveTooltip(isOpen, hide);
	useEffect(() => {
		if (!isOpen) return;
		return markTooltipOpen();
	}, [isOpen]);
	useEffect(() => {
		if (!isOpen) return;
		return subscribeTooltipScrollHide(hide);
	}, [hide, isOpen]);
	useEffect(() => {
		const referenceHovered = refs.domReference.current?.matches(':hover') ?? false;
		const floatingHovered = refs.floating.current?.matches(':hover') ?? false;
		if (isOpen && !keyboardModeEnabled && !referenceHovered && !floatingHovered) {
			hide();
		}
	}, [hide, isOpen, keyboardModeEnabled, refs.domReference, refs.floating]);
	return {
		targetRef: refs.setReference as React.RefCallback<HTMLElement>,
		floatingRef: refs.setFloating as React.RefCallback<HTMLDivElement>,
		state,
		updatePosition,
		show,
		hide,
		referenceProps: omitRef(getReferenceProps() as React.HTMLProps<HTMLElement>) as Record<string, unknown>,
		floatingProps: omitRef(getFloatingProps() as React.HTMLProps<HTMLDivElement>) as Record<string, unknown>,
	};
}
