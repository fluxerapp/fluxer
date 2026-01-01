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

export const TextareaAutosize = React.forwardRef<HTMLTextAreaElement, TextareaAutosizeProps>((props, forwardedRef) => {
	const {minRows: minRowsProp, maxRows, style, onHeightChange, rows, ...rest} = props;

	const minRows = minRowsProp ?? (typeof rows === 'number' ? rows : undefined);

	const elRef = React.useRef<HTMLTextAreaElement | null>(null);
	const onHeightChangeRef = React.useRef(onHeightChange);
	const lastHeightRef = React.useRef<number | null>(null);

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

	React.useLayoutEffect(() => {
		const el = elRef.current;
		if (!el || (minRows == null && maxRows == null)) return;

		const {minHeight, maxHeight} = computeRowConstraints(el, minRows, maxRows);

		if (minHeight != null) {
			el.style.minHeight = `${minHeight}px`;
		}
		if (maxHeight != null) {
			el.style.maxHeight = `${maxHeight}px`;
			el.style.overflowY = 'auto';
		}
	}, [minRows, maxRows]);

	React.useEffect(() => {
		const el = elRef.current;
		if (!el || typeof ResizeObserver === 'undefined') return;

		const ro = new ResizeObserver((entries) => {
			const entry = entries[0];
			if (!entry) return;

			const height = entry.borderBoxSize?.[0]?.blockSize ?? el.getBoundingClientRect().height;
			if (height !== lastHeightRef.current) {
				lastHeightRef.current = height;
				const cs = window.getComputedStyle(el);
				onHeightChangeRef.current?.(height, {rowHeight: getLineHeight(cs)});
			}
		});

		ro.observe(el);
		return () => ro.disconnect();
	}, []);

	const computedStyle = React.useMemo(
		(): React.CSSProperties => ({
			fieldSizing: 'content',
			...style,
		}),
		[style],
	);

	return <textarea {...rest} ref={setRef} rows={rows} style={computedStyle} />;
});

TextareaAutosize.displayName = 'TextareaAutosize';
