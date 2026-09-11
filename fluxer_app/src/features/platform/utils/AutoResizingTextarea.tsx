// SPDX-License-Identifier: AGPL-3.0-or-later

import {replaceSelectedText} from '@app/features/messaging/utils/TextInputEditUtils';
import * as React from 'react';
import {useCallback, useEffect, useMemo, useRef} from 'react';

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

function computeRowConstraintsFromStyle(style: CSSStyleDeclaration, minRows?: number, maxRows?: number) {
	const lineHeight = getLineHeight(style);
	const paddingBlock = getNumber(style.paddingTop) + getNumber(style.paddingBottom);
	const borderBlock = getNumber(style.borderTopWidth) + getNumber(style.borderBottomWidth);
	const extra = style.boxSizing === 'border-box' ? paddingBlock + borderBlock : 0;
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

function ensureMeasureEl(): HTMLTextAreaElement {
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

function syncMeasureStyles(measure: HTMLTextAreaElement, style: CSSStyleDeclaration, width: number) {
	const typedStyle = style as CSSStyleDeclaration & {
		overflowWrap?: string;
		tabSize?: string;
		textRendering?: string;
	};
	measure.style.boxSizing = style.boxSizing;
	measure.style.width = `${width}px`;
	measure.style.font = style.font;
	measure.style.fontFamily = style.fontFamily;
	measure.style.fontSize = style.fontSize;
	measure.style.fontWeight = style.fontWeight;
	measure.style.fontStyle = style.fontStyle;
	measure.style.letterSpacing = style.letterSpacing;
	measure.style.textTransform = style.textTransform;
	measure.style.textRendering = typedStyle.textRendering ?? '';
	measure.style.lineHeight = style.lineHeight;
	measure.style.whiteSpace = style.whiteSpace;
	measure.style.wordBreak = style.wordBreak;
	measure.style.overflowWrap = typedStyle.overflowWrap || 'normal';
	measure.style.tabSize = typedStyle.tabSize || '8';
	measure.style.paddingTop = style.paddingTop;
	measure.style.paddingBottom = style.paddingBottom;
	measure.style.paddingLeft = style.paddingLeft;
	measure.style.paddingRight = style.paddingRight;
	measure.style.borderTopWidth = style.borderTopWidth;
	measure.style.borderBottomWidth = style.borderBottomWidth;
	measure.style.borderLeftWidth = style.borderLeftWidth;
	measure.style.borderRightWidth = style.borderRightWidth;
	measure.style.borderTopStyle = style.borderTopStyle;
	measure.style.borderBottomStyle = style.borderBottomStyle;
	measure.style.borderLeftStyle = style.borderLeftStyle;
	measure.style.borderRightStyle = style.borderRightStyle;
	measure.style.borderTopColor = style.borderTopColor;
	measure.style.borderBottomColor = style.borderBottomColor;
	measure.style.borderLeftColor = style.borderLeftColor;
	measure.style.borderRightColor = style.borderRightColor;
	measure.style.borderRadius = style.borderRadius;
}

export const TextareaAutosize = React.forwardRef<HTMLTextAreaElement, TextareaAutosizeProps>((props, forwardedRef) => {
	const {minRows: minRowsProp, maxRows, style, onHeightChange, rows, onInput, onPaste, wrap, ...rest} = props;
	const resolvedRows = rows ?? 1;
	const minRows = minRowsProp ?? (typeof resolvedRows === 'number' ? resolvedRows : undefined);
	const nativeFieldSizing = supportsFieldSizingContent();
	const elRef = useRef<HTMLTextAreaElement | null>(null);
	const measureRef = useRef<HTMLTextAreaElement | null>(null);
	const onHeightChangeRef = useRef(onHeightChange);
	const lastWidthRef = useRef<number | null>(null);
	const lastEmittedHeightRef = useRef<number | null>(null);
	const resizeScheduledRef = useRef(false);
	const resizeRafRef = useRef<number | null>(null);
	const setRef = useCallback(
		(node: HTMLTextAreaElement | null) => {
			elRef.current = node;
			if (typeof forwardedRef === 'function') forwardedRef(node);
			else if (forwardedRef) forwardedRef.current = node;
		},
		[forwardedRef],
	);
	useEffect(() => {
		onHeightChangeRef.current = onHeightChange;
	}, [onHeightChange]);
	useEffect(() => {
		if (nativeFieldSizing) return;
		const measure = ensureMeasureEl();
		measureRef.current = measure;
		return () => {
			measureRef.current?.remove();
			measureRef.current = null;
		};
	}, [nativeFieldSizing]);
	const emitHeightIfChanged = useCallback(() => {
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
	const applyRowConstraints = useCallback(() => {
		const el = elRef.current;
		if (!el || (minRows == null && maxRows == null)) return;
		const style = window.getComputedStyle(el);
		const {minHeight, maxHeight} = computeRowConstraintsFromStyle(style, minRows, maxRows);
		if (minHeight != null) el.style.minHeight = `${minHeight}px`;
		if (maxHeight != null) el.style.maxHeight = `${maxHeight}px`;
	}, [maxRows, minRows]);
	useEffect(() => {
		applyRowConstraints();
	}, [applyRowConstraints]);
	useEffect(() => {
		const el = elRef.current;
		if (!el) return;
		if (nativeFieldSizing) {
			el.style.setProperty('field-sizing', 'content');
		} else {
			el.style.removeProperty('field-sizing');
		}
	}, [nativeFieldSizing]);
	const resize = useCallback(() => {
		const el = elRef.current;
		if (!el) return;
		if (nativeFieldSizing) {
			emitHeightIfChanged();
			return;
		}
		const measure = measureRef.current;
		if (!measure) return;
		const cs = window.getComputedStyle(el);
		const {minHeight, maxHeight, lineHeight} = computeRowConstraintsFromStyle(cs, minRows, maxRows);
		const borderBlock = getNumber(cs.borderTopWidth) + getNumber(cs.borderBottomWidth);
		const isBorderBox = cs.boxSizing === 'border-box';
		const width = lastWidthRef.current ?? el.getBoundingClientRect().width;
		syncMeasureStyles(measure, cs, width);
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
	const scheduleResize = useCallback(() => {
		if (resizeScheduledRef.current) return;
		resizeScheduledRef.current = true;
		resizeRafRef.current = requestAnimationFrame(() => {
			resizeRafRef.current = null;
			resizeScheduledRef.current = false;
			resize();
		});
	}, [resize]);
	useEffect(() => {
		return () => {
			if (resizeRafRef.current != null) {
				cancelAnimationFrame(resizeRafRef.current);
				resizeRafRef.current = null;
			}
		};
	}, []);
	useEffect(() => {
		const el = elRef.current;
		if (!el || typeof ResizeObserver === 'undefined') return;
		const ro = new ResizeObserver((entries) => {
			const entry = entries[0];
			if (!entry) return;
			const borderBoxSize = entry.borderBoxSize;
			const width = borderBoxSize?.[0]?.inlineSize ?? entry.contentRect.width;
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
	const computedStyle = useMemo(
		(): React.CSSProperties => ({
			minWidth: 0,
			maxWidth: '100%',
			overflowX: 'hidden',
			overflowY: maxRows ? 'auto' : 'hidden',
			whiteSpace: 'pre-wrap',
			overflowWrap: 'anywhere',
			wordBreak: 'break-word',
			...style,
		}),
		[maxRows, style],
	);
	const handleInput = useCallback(
		(event: React.InputEvent<HTMLTextAreaElement>) => {
			scheduleResize();
			onInput?.(event);
		},
		[onInput, scheduleResize],
	);
	const handlePaste = useCallback(
		(event: React.ClipboardEvent<HTMLTextAreaElement>) => {
			onPaste?.(event);
			if (!event.defaultPrevented) {
				const pastedText = event.clipboardData?.getData('text/plain');
				if (pastedText?.includes('\t')) {
					event.preventDefault();
					replaceSelectedText(event.currentTarget, pastedText.replace(/\t/g, '    '));
				}
			}
		},
		[onPaste],
	);
	useEffect(() => {
		scheduleResize();
	}, [scheduleResize, props.value, props.defaultValue, rows, minRows, maxRows, nativeFieldSizing]);
	return (
		<textarea
			data-flx="platform.auto-resizing-textarea.textarea-autosize.textarea.input"
			{...rest}
			ref={setRef}
			rows={resolvedRows}
			wrap={wrap ?? 'soft'}
			style={computedStyle}
			onInput={handleInput}
			onPaste={handlePaste}
		/>
	);
});

TextareaAutosize.displayName = 'TextareaAutosize';
