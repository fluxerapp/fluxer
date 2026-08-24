// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FOCUS_RING_COLOR_CSS_PROPERTY,
	FOCUS_RING_RADIUS_CSS_PROPERTY,
	type FocusRingAncestry,
	type FocusRingShowOpts,
	type FocusRingStyleProperties,
	type Offset,
} from '@app/features/ui/focus_ring/FocusRingTypes';
import * as React from 'react';

export let ACTIVE_RING_CONTEXT_MANAGER: FocusRingContextManager | undefined;

function setActiveRingContextManager(manager: FocusRingContextManager) {
	if (manager !== ACTIVE_RING_CONTEXT_MANAGER) {
		ACTIVE_RING_CONTEXT_MANAGER?.hide();
		ACTIVE_RING_CONTEXT_MANAGER = manager;
	}
}

function parseBorderRadius(radius: string | undefined) {
	if (radius) {
		return parseInt(radius, 10) > 0 ? radius : undefined;
	}
	return undefined;
}

export class FocusRingContextManager {
	targetElement?: Element;
	ancestorChain?: FocusRingAncestry;
	boundingBox?: DOMRect;
	className?: string;
	offset: Offset | number = 0;
	zIndex?: number;
	container: Element | null = null;
	invalidate: () => void = () => null;

	setContainer(element: Element | null) {
		this.container = element;
	}

	showForElement(element: Element, opts: FocusRingShowOpts = {}) {
		this.targetElement = element;
		this.ancestorChain = this.collectAncestorChain(this.targetElement);
		this.boundingBox = undefined;
		this.className = opts.className;
		this.offset = opts.offset ?? 0;
		this.zIndex = opts.zIndex;
		setActiveRingContextManager(this);
		this.invalidate();
	}

	hide() {
		this.targetElement = undefined;
		this.ancestorChain = undefined;
		this.boundingBox = undefined;
		this.className = undefined;
		this.offset = 0;
		this.zIndex = undefined;
		this.invalidate();
	}

	get visible() {
		return this.targetElement != null || this.boundingBox != null;
	}

	private collectAncestorChain(element?: Element): FocusRingAncestry {
		if (element == null) return {elements: [], styles: []};
		const elements: Array<Element> = [];
		const styles: Array<CSSStyleDeclaration> = [];
		let current: Element | null = element;
		while (current != null) {
			elements.push(current);
			styles.push(window.getComputedStyle(current));
			current = current.parentElement;
		}
		return {elements, styles};
	}

	private resolveStackingLayer(ancestry: FocusRingAncestry) {
		for (let i = 0; i < ancestry.elements.length; i++) {
			const element = ancestry.elements[i];
			const style = ancestry.styles[i];
			const zIndex = parseInt(style.zIndex, 10);
			if (!Number.isNaN(zIndex)) return zIndex + 1;
			if (element === this.container) break;
		}
		return undefined;
	}

	private readCornerRadii(ancestry: FocusRingAncestry) {
		const topLeft = parseBorderRadius(ancestry.styles[0]?.borderTopLeftRadius) ?? '0';
		const topRight = parseBorderRadius(ancestry.styles[0]?.borderTopRightRadius) ?? '0';
		const bottomRight = parseBorderRadius(ancestry.styles[0]?.borderBottomRightRadius) ?? '0';
		const bottomLeft = parseBorderRadius(ancestry.styles[0]?.borderBottomLeftRadius) ?? '0';
		if (topLeft === '0' && topRight === '0' && bottomRight === '0' && bottomLeft === '0') {
			return undefined;
		}
		return `${topLeft} ${topRight} ${bottomRight} ${bottomLeft}`;
	}

	private computeRingBox(rect: DOMRect) {
		if (this.container == null) return {};
		const containerRect = this.container.getBoundingClientRect();
		const {scrollTop, scrollLeft} = this.container;
		let top = 0;
		let right = 0;
		let bottom = 0;
		let left = 0;
		if (typeof this.offset === 'number') {
			top = this.offset;
			right = this.offset;
			bottom = this.offset;
			left = this.offset;
		} else {
			top = this.offset.top ?? 0;
			right = this.offset.right ?? 0;
			bottom = this.offset.bottom ?? 0;
			left = this.offset.left ?? 0;
		}
		return {
			top: scrollTop + rect.top - containerRect.top + top,
			width: rect.width - (right + left),
			height: rect.height - (bottom + top),
			left: scrollLeft + rect.left - containerRect.left + left,
		};
	}

	getStyle(): FocusRingStyleProperties {
		let styles = {};
		if (this.boundingBox != null) {
			styles = {
				...this.computeRingBox(this.boundingBox),
				zIndex: this.zIndex,
				[FOCUS_RING_COLOR_CSS_PROPERTY]: 'var(--focus-primary)',
			};
		}
		if (this.targetElement != null && this.ancestorChain != null) {
			styles = {
				...this.computeRingBox(this.targetElement.getBoundingClientRect()),
				zIndex: this.zIndex ?? this.resolveStackingLayer(this.ancestorChain),
				[FOCUS_RING_COLOR_CSS_PROPERTY]: 'var(--focus-primary)',
				[FOCUS_RING_RADIUS_CSS_PROPERTY]: this.readCornerRadii(this.ancestorChain),
			};
		}
		return styles;
	}
}

const GLOBAL_FOCUS_RING_CONTEXT = new FocusRingContextManager();

GLOBAL_FOCUS_RING_CONTEXT.setContainer(document.body);

const FocusRingContext = React.createContext(GLOBAL_FOCUS_RING_CONTEXT);

export default FocusRingContext;
