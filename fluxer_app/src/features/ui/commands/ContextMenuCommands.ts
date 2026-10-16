// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ContextMenu, ContextMenuConfig, ContextMenuTargetElement} from '@app/features/ui/state/ContextMenu';
import ContextMenuState from '@app/features/ui/state/ContextMenu';
import type React from 'react';

const nativeContextMenuTarget: ContextMenuTargetElement = {
	tagName: 'ReactNativeContextMenu',
	isConnected: true,
	focus: (): void => undefined,
	addEventListener: (
		_type: string,
		_listener: EventListenerOrEventListenerObject | null,
		_options?: boolean | AddEventListenerOptions,
	) => undefined,
	removeEventListener: (
		_type: string,
		_listener: EventListenerOrEventListenerObject | null,
		_options?: boolean | EventListenerOptions,
	) => undefined,
};
const CONTEXT_MENU_OFFSET = 4;
const ELEMENT_CONTEXT_MENU_GUTTER = 8;
const makeId = (prefix: string) => `${prefix}-${Date.now()}-${Math.random()}`;
const getViewportCenterForElement = (el: Element) => {
	const rect = el.getBoundingClientRect();
	return {x: rect.left + rect.width / 2, y: rect.top + rect.height / 2};
};
const isElementLike = (value: unknown): value is Element => {
	if (!value) return false;
	const ownerDocument = (value as Element).ownerDocument;
	const ownerWindow = ownerDocument?.defaultView;
	if (ownerWindow) {
		return value instanceof ownerWindow.Element;
	}
	return value instanceof Element;
};
const isHTMLElementLike = (value: unknown): value is HTMLElement => {
	if (!value) return false;
	const ownerDocument = (value as HTMLElement).ownerDocument;
	const ownerWindow = ownerDocument?.defaultView;
	if (ownerWindow) {
		return value instanceof ownerWindow.HTMLElement;
	}
	return value instanceof HTMLElement;
};
const toHTMLElement = (value: unknown): HTMLElement | null => {
	if (!value) return null;
	if (isHTMLElementLike(value)) return value;
	if (isElementLike(value)) {
		return (value.closest('button,[role="button"],a,[data-contextmenu-anchor="true"]') as HTMLElement | null) ?? null;
	}
	return null;
};

export function close(): void {
	ContextMenuState.close();
}

type RenderFn = (props: {onClose: () => void}) => React.ReactNode;

export function openAtPoint(
	point: {
		x: number;
		y: number;
	},
	render: RenderFn,
	config?: ContextMenuConfig,
	target: ContextMenuTargetElement = nativeContextMenuTarget,
): void {
	const contextMenu: ContextMenu = {
		id: makeId('context-menu'),
		target: {x: point.x, y: point.y, target},
		render,
		config: {noBlurEvent: true, ...config},
	};
	ContextMenuState.open(contextMenu);
}

export function openForElement(
	element: HTMLElement,
	render: RenderFn,
	options?: {
		point?: {
			x: number;
			y: number;
		};
		config?: ContextMenuConfig;
	},
): void {
	const point = options?.point ?? getViewportCenterForElement(element);
	openAtPoint(point, render, {trackDynamicPosition: true, ...options?.config}, element);
}

export function openFromEvent(
	event: React.MouseEvent | MouseEvent,
	render: RenderFn,
	config?: ContextMenuConfig,
): void {
	event.preventDefault?.();
	event.stopPropagation?.();
	const nativeEvent = 'nativeEvent' in event ? event.nativeEvent : event;
	const currentTarget = 'currentTarget' in event ? toHTMLElement(event.currentTarget) : null;
	const target = 'target' in event ? toHTMLElement(event.target) : null;
	const anchor = currentTarget ?? target;
	const hasPointerCoords = !(
		event.clientX === 0 &&
		event.clientY === 0 &&
		nativeEvent.detail === 0 &&
		nativeEvent.button === 0
	);
	const point = hasPointerCoords
		? {x: event.clientX + 2, y: event.clientY + 2}
		: anchor
			? (() => {
					const c = getViewportCenterForElement(anchor);
					return {x: c.x + 2, y: c.y + 2};
				})()
			: {x: 0, y: 0};
	openAtPoint(point, render, config, anchor ?? nativeContextMenuTarget);
}

export function openFromElementBottomRight(
	event: React.MouseEvent | MouseEvent,
	render: RenderFn,
	config?: ContextMenuConfig,
): void {
	event.preventDefault?.();
	event.stopPropagation?.();
	const currentTarget = 'currentTarget' in event ? toHTMLElement(event.currentTarget) : null;
	const target = 'target' in event ? toHTMLElement(event.target) : null;
	const anchor = currentTarget ?? target;
	if (!anchor) {
		openFromEvent(event, render, config);
		return;
	}
	const rect = anchor.getBoundingClientRect();
	const point = {x: rect.right, y: rect.bottom + 4};
	openAtPoint(point, render, {align: 'top-right', trackDynamicPosition: true, ...config}, anchor);
}

export function openAboveElementBottomRight(
	event: React.MouseEvent | MouseEvent,
	render: RenderFn,
	options?: {
		gutter?: number;
		config?: ContextMenuConfig;
	},
): void {
	event.preventDefault?.();
	event.stopPropagation?.();
	const gutter = options?.gutter ?? 8;
	const currentTarget = 'currentTarget' in event ? toHTMLElement(event.currentTarget) : null;
	const target = 'target' in event ? toHTMLElement(event.target) : null;
	const anchor = currentTarget ?? target;
	if (!anchor) {
		openFromEvent(event, render, options?.config);
		return;
	}
	const rect = anchor.getBoundingClientRect();
	const point = {x: rect.right, y: rect.top - gutter};
	openAtPoint(point, render, {align: 'bottom-right', trackDynamicPosition: true, ...options?.config}, anchor);
}

export function openAboveElementBottomLeft(
	event: React.MouseEvent | MouseEvent,
	render: RenderFn,
	options?: {
		gutter?: number;
		config?: ContextMenuConfig;
	},
): void {
	event.preventDefault?.();
	event.stopPropagation?.();
	const gutter = options?.gutter ?? 8;
	const currentTarget = 'currentTarget' in event ? toHTMLElement(event.currentTarget) : null;
	const target = 'target' in event ? toHTMLElement(event.target) : null;
	const anchor = currentTarget ?? target;
	if (!anchor) {
		openFromEvent(event, render, options?.config);
		return;
	}
	const rect = anchor.getBoundingClientRect();
	const point = {x: rect.left, y: rect.top - gutter};
	openAtPoint(point, render, {align: 'bottom-left', trackDynamicPosition: true, ...options?.config}, anchor);
}

export function openFromElementLeftStart(
	event: React.MouseEvent | React.KeyboardEvent | MouseEvent,
	render: RenderFn,
	options?: {
		gutter?: number;
		config?: ContextMenuConfig;
	},
): void {
	event.preventDefault?.();
	event.stopPropagation?.();
	const gutter = options?.gutter ?? ELEMENT_CONTEXT_MENU_GUTTER;
	const currentTarget = 'currentTarget' in event ? toHTMLElement(event.currentTarget) : null;
	const target = 'target' in event ? toHTMLElement(event.target) : null;
	const anchor = currentTarget ?? target;
	if (!anchor) {
		if ('clientX' in event) {
			openFromEvent(event, render, options?.config);
			return;
		}
		openAtPoint({x: 0, y: 0}, render, options?.config);
		return;
	}
	const rect = anchor.getBoundingClientRect();
	const point = {x: rect.left - gutter, y: rect.top - CONTEXT_MENU_OFFSET};
	openAtPoint(point, render, {align: 'top-right', trackDynamicPosition: true, ...options?.config}, anchor);
}
