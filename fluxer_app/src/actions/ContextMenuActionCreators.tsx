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

import type React from 'react';
import type {ContextMenu, ContextMenuConfig, ContextMenuTargetElement} from '~/stores/ContextMenuStore';
import ContextMenuStore from '~/stores/ContextMenuStore';

const nativeContextMenuTarget: ContextMenuTargetElement = {
	tagName: 'ReactNativeContextMenu',
	isConnected: true,
	focus: (): void => undefined,
	addEventListener: (..._args: Parameters<HTMLElement['addEventListener']>) => undefined,
	removeEventListener: (..._args: Parameters<HTMLElement['removeEventListener']>) => undefined,
};

const makeId = (prefix: string) => `${prefix}-${Date.now()}-${Math.random()}`;

const getViewportCenterForElement = (el: Element) => {
	const rect = el.getBoundingClientRect();
	const scrollX = window.scrollX || window.pageXOffset || 0;
	const scrollY = window.scrollY || window.pageYOffset || 0;
	return {x: rect.left + rect.width / 2 + scrollX, y: rect.top + rect.height / 2 + scrollY};
};

const toHTMLElement = (value: unknown): HTMLElement | null => {
	if (!value) return null;
	if (value instanceof HTMLElement) return value;
	if (value instanceof Element) {
		return (value.closest('button,[role="button"],a,[data-contextmenu-anchor="true"]') as HTMLElement | null) ?? null;
	}
	return null;
};

export const close = (): void => {
	ContextMenuStore.close();
};

type RenderFn = (props: {onClose: () => void}) => React.ReactNode;

export const openAtPoint = (
	point: {x: number; y: number},
	render: RenderFn,
	config?: ContextMenuConfig,
	target: ContextMenuTargetElement = nativeContextMenuTarget,
): void => {
	const contextMenu: ContextMenu = {
		id: makeId('context-menu'),
		target: {x: point.x, y: point.y, target},
		render,
		config: {noBlurEvent: true, ...config},
	};

	ContextMenuStore.open(contextMenu);
};

export const openForElement = (
	element: HTMLElement,
	render: RenderFn,
	options?: {point?: {x: number; y: number}; config?: ContextMenuConfig},
): void => {
	const point = options?.point ?? getViewportCenterForElement(element);
	openAtPoint(point, render, options?.config, element);
};

export const openFromEvent = (
	event: React.MouseEvent | MouseEvent,
	render: RenderFn,
	config?: ContextMenuConfig,
): void => {
	event.preventDefault?.();
	event.stopPropagation?.();

	const nativeEvent = 'nativeEvent' in event ? event.nativeEvent : event;

	const currentTarget = 'currentTarget' in event ? toHTMLElement(event.currentTarget) : null;
	const target = 'target' in event ? toHTMLElement(event.target) : null;
	const anchor = currentTarget ?? target;

	const hasPointerCoords = !(event.pageX === 0 && event.pageY === 0 && nativeEvent.detail === 0);
	const point = hasPointerCoords
		? {x: event.pageX + 2, y: event.pageY + 2}
		: anchor
			? (() => {
					const c = getViewportCenterForElement(anchor);
					return {x: c.x + 2, y: c.y + 2};
				})()
			: {x: 0, y: 0};

	openAtPoint(point, render, config, anchor ?? nativeContextMenuTarget);
};

export const openFromElementBottomRight = (
	event: React.MouseEvent | MouseEvent,
	render: RenderFn,
	config?: ContextMenuConfig,
): void => {
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
	const scrollX = window.scrollX || window.pageXOffset || 0;
	const scrollY = window.scrollY || window.pageYOffset || 0;
	const point = {x: rect.right + scrollX, y: rect.bottom + scrollY + 4};

	openAtPoint(point, render, {align: 'top-right', ...config}, anchor);
};

export const openNativeContextMenu = (render: RenderFn, config?: ContextMenu['config']): void => {
	const contextMenu: ContextMenu = {
		id: makeId('native-context-menu'),
		target: {
			x: 0,
			y: 0,
			target: nativeContextMenuTarget,
		},
		render,
		config: {
			returnFocus: false,
			...config,
		},
	};

	ContextMenuStore.open(contextMenu);
};
