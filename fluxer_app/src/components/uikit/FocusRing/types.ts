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

import type * as React from 'react';

export const FOCUS_RING_COLOR_CSS_PROPERTY = '--focus-ring-color';
export const FOCUS_RING_RADIUS_CSS_PROPERTY = '--focus-ring-radius';

export interface Offset {
	top?: number;
	right?: number;
	bottom?: number;
	left?: number;
}

export interface FocusRingShowOpts {
	className?: string;
	offset?: number | Offset;
	zIndex?: number;
}

export interface FocusRingAncestry {
	elements: Array<Element>;
	styles: Array<CSSStyleDeclaration>;
}

export interface FocusRingStyleProperties extends React.CSSProperties {
	[FOCUS_RING_COLOR_CSS_PROPERTY]?: string;
	[FOCUS_RING_RADIUS_CSS_PROPERTY]?: string;
}

export interface ThemeOptions {
	focusColor?: string;
	lightColor?: string;
	darkColor?: string;
	threshold?: number;
}

export interface FocusRingProps {
	within?: boolean;
	enabled?: boolean;
	focused?: boolean;
	offset?: number | Offset;
	focusTarget?: React.RefObject<Element | null>;
	ringTarget?: React.RefObject<Element | null>;
	ringClassName?: string;
	focusClassName?: string;
	focusWithinClassName?: string;
}
