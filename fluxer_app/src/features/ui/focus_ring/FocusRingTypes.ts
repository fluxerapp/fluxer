// SPDX-License-Identifier: AGPL-3.0-or-later

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
