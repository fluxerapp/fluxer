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

import * as React from 'react';

export interface TextareaAutosizeProps extends React.TextareaHTMLAttributes<HTMLTextAreaElement> {
	minRows?: number;
	maxRows?: number;
	onHeightChange?: (height: number, meta: {rowHeight: number}) => void;
}

function getLineHeight(style: CSSStyleDeclaration): number {
	const lh = Number.parseFloat(style.lineHeight);
	if (Number.isFinite(lh)) return lh;
	const fs = Number.parseFloat(style.fontSize);
	return Number.isFinite(fs) ? fs * 1.2 : 16 * 1.2;
}

function getNumber(v: string): number {
	const n = Number.parseFloat(v);
	return Number.isFinite(n) ? n : 0;
}

function computeRowConstraints(el: HTMLTextAreaElement, minRows?: number, maxRows?: number) {
	const cs = window.getComputedStyle(el);
	const lineHeight = getLineHeight(cs);
	const paddingBlock = getNumber(cs.paddingTop) + getNumber(cs.paddingBottom);
	const borderBlock = getNumber(cs.borderTopWidth) + getNumber(cs.borderBottomWidth);
	const extra = cs.boxSizing === 'border-box' ? paddingBlock + borderBlock : 0;

	return {
		minHeight: minRows != null ? lineHeight * minRows + extra : undefined,
		maxHeight: maxRows != null ? lineHeight * maxRows + extra : undefined,
		lineHeight,
	};
}

function supportsFieldSizingContent(): boolean {
	try {
		return typeof CSS !== 'undefined' && typeof CSS.supports === 'function' && CSS.supports('field-sizing: content');
	} catch {
		return false;
	}
}

function normalizeValueForMeasurement(value: string): string {
	if (!value.includes('\n')) return value;
	const parts = value.split('\n');
	for (let i = 0; i < parts.length; i++) {
		if (parts[i] === '') parts[i] = '\u200b';
	}
	return parts.join('\n');
}

function ensureMeasureEl(): HTMLTextAreaElement | null {
	if (typeof document === 'undefined') return null;
	const el = document.createElement('textarea');
	el.setAttribute('aria-hidden', 'true');
	el.tabIndex = -1;
	el.style.position = 'absolute';
	el.style.top = '0';
	el.style.left = '-9999px';
	el.style.height = '0px';
	el.style.overflow = 'hidden';
	el.style.visibility = 'hidden';
	el.style.pointerEvents = 'none';
	el.style.zIndex = '-1';
	document.body.appendChild(el);
	return el;
}

function syncMeasureStyles(target: HTMLTextAreaElement, measure: HTMLTextAreaElement) {
	const cs = window.getComputedStyle(target);

	measure.style.boxSizing = cs.boxSizing;
	measure.style.width = `${target.getBoundingClientRect().width}px`;

	measure.style.font = cs.font;
	measure.style.fontFamily = cs.fontFamily;
	measure.style.fontSize = cs.fontSize;
	measure.style.fontWeight = cs.fontWeight;
	measure.style.fontStyle = cs.fontStyle;
	measure.style.letterSpacing = cs.letterSpacing;
	measure.style.textTransform = cs.textTransform;
	measure.style.textRendering = cs.textRendering as any;

	measure.style.lineHeight = cs.lineHeight;
	measure.style.whiteSpace = cs.whiteSpace;
	measure.style.wordBreak = cs.wordBreak;
	measure.style.overflowWrap = (cs as any).overflowWrap ?? 'normal';
	measure.style.tabSize = (cs as any).tabSize ?? '8';

	measure.style.paddingTop = cs.paddingTop;
	measure.style.paddingBottom = cs.paddingBottom;
	measure.style.paddingLeft = cs.paddingLeft;
	measure.style.paddingRight = cs.paddingRight;

	measure.style.borderTopWidth = cs.borderTopWidth;
	measure.style.borderBottomWidth = cs.borderBottomWidth;
	measure.style.borderLeftWidth = cs.borderLeftWidth;
	measure.style.borderRightWidth = cs.borderRightWidth;
	measure.style.borderTopStyle = cs.borderTopStyle;
	measure.style.borderBottomStyle = cs.borderBottomStyle;
	measure.style.borderLeftStyle = cs.borderLeftStyle;
	measure.style.borderRightStyle = cs.borderRightStyle;
	measure.style.borderTopColor = cs.borderTopColor;
	measure.style.borderBottomColor = cs.borderBottomColor;
	measure.style.borderLeftColor = cs.borderLeftColor;
	measure.style.borderRightColor = cs.borderRightColor;

	measure.style.borderRadius = cs.borderRadius;
}

export const TextareaAutosize = React.forwardRef<HTMLTextAreaElement, TextareaAutosizeProps>((props, forwardedRef) => {
	const {minRows: minRowsProp, maxRows, style, onHeightChange, rows, onInput, ...rest} = props;

	const resolvedRows = rows ?? 1;
	const minRows = minRowsProp ?? (typeof resolvedRows === 'number' ? resolvedRows : undefined);

	const nativeFieldSizing = supportsFieldSizingContent();

	const elRef = React.useRef<HTMLTextAreaElement | null>(null);
	const measureRef = React.useRef<HTMLTextAreaElement | null>(null);

	const onHeightChangeRef = React.useRef(onHeightChange);
	const lastWidthRef = React.useRef<number | null>(null);
	const lastEmittedHeightRef = React.useRef<number | null>(null);
	const resizeScheduledRef = React.useRef(false);

	const setRef = React.useCallback(
		(node: HTMLTextAreaElement | null) => {
			elRef.current = node;
			if (typeof forwardedRef === 'function') forwardedRef(node);
			else if (forwardedRef) (forwardedRef as React.MutableRefObject<HTMLTextAreaElement | null>).current = node;
		},
		[forwardedRef],
	);

	React.useEffect(() => {
		onHeightChangeRef.current = onHeightChange;
	}, [onHeightChange]);

	React.useEffect(() => {
		if (nativeFieldSizing) return;
		const measure = ensureMeasureEl();
		measureRef.current = measure;
		return () => {
			measureRef.current?.remove();
			measureRef.current = null;
		};
	}, [nativeFieldSizing]);

	const emitHeightIfChanged = React.useCallback(() => {
		const el = elRef.current;
		if (!el) return;

		const cs = window.getComputedStyle(el);
		const lineHeight = getLineHeight(cs);
		const height = Math.round(el.getBoundingClientRect().height);

		if (lastEmittedHeightRef.current !== height) {
			lastEmittedHeightRef.current = height;
			onHeightChangeRef.current?.(height, {rowHeight: lineHeight});
		}
	}, []);

	React.useLayoutEffect(() => {
		const el = elRef.current;
		if (!el || (minRows == null && maxRows == null)) return;

		const {minHeight, maxHeight} = computeRowConstraints(el, minRows, maxRows);

		if (minHeight != null) el.style.minHeight = `${minHeight}px`;
		if (maxHeight != null) el.style.maxHeight = `${maxHeight}px`;
	}, [minRows, maxRows]);

	const resize = React.useCallback(() => {
		const el = elRef.current;
		if (!el) return;

		if (nativeFieldSizing) {
			emitHeightIfChanged();
			return;
		}

		const measure = measureRef.current;
		if (!measure) return;

		const cs = window.getComputedStyle(el);
		const {minHeight, maxHeight, lineHeight} = computeRowConstraints(el, minRows, maxRows);
		const borderBlock = getNumber(cs.borderTopWidth) + getNumber(cs.borderBottomWidth);
		const isBorderBox = cs.boxSizing === 'border-box';

		syncMeasureStyles(el, measure);

		const measuredValue = normalizeValueForMeasurement(el.value);
		if (measure.value !== measuredValue) {
			measure.value = measuredValue;
		}

		let nextHeight = measure.scrollHeight + (isBorderBox ? borderBlock : 0);
		if (minHeight != null) nextHeight = Math.max(nextHeight, minHeight);
		if (maxHeight != null) nextHeight = Math.min(nextHeight, maxHeight);

		const heightPx = `${Math.round(nextHeight)}px`;
		if (el.style.height !== heightPx) {
			el.style.height = heightPx;
		}

		const emittedHeight = Math.round(nextHeight);
		if (lastEmittedHeightRef.current !== emittedHeight) {
			lastEmittedHeightRef.current = emittedHeight;
			onHeightChangeRef.current?.(emittedHeight, {rowHeight: lineHeight});
		}
	}, [emitHeightIfChanged, maxRows, minRows, nativeFieldSizing]);

	const scheduleResize = React.useCallback(() => {
		if (resizeScheduledRef.current) return;
		resizeScheduledRef.current = true;

		requestAnimationFrame(() => {
			resizeScheduledRef.current = false;
			resize();
		});
	}, [resize]);

	React.useEffect(() => {
		const el = elRef.current;
		if (!el || typeof ResizeObserver === 'undefined') return;

		const ro = new ResizeObserver((entries) => {
			const entry = entries[0];
			if (!entry) return;

			const width = (entry as any).borderBoxSize?.[0]?.inlineSize ?? el.getBoundingClientRect().width;

			if (width !== lastWidthRef.current) {
				lastWidthRef.current = width;
				scheduleResize();
				return;
			}

			if (nativeFieldSizing) {
				emitHeightIfChanged();
			}
		});

		ro.observe(el);
		return () => ro.disconnect();
	}, [emitHeightIfChanged, nativeFieldSizing, scheduleResize]);

	const computedStyle = React.useMemo(
		(): React.CSSProperties => ({
			overflow: maxRows ? 'auto' : 'hidden',
			...style,
		}),
		[maxRows, style],
	);

	const handleInput = React.useCallback(
		(event: React.FormEvent<HTMLTextAreaElement>) => {
			scheduleResize();
			onInput?.(event);
		},
		[onInput, scheduleResize],
	);

	React.useLayoutEffect(() => {
		scheduleResize();
	}, [scheduleResize, props.value, props.defaultValue, rows, minRows, maxRows, nativeFieldSizing]);

	return <textarea {...rest} ref={setRef} rows={resolvedRows} style={computedStyle} onInput={handleInput} />;
});

TextareaAutosize.displayName = 'TextareaAutosize';
