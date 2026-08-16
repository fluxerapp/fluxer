// SPDX-License-Identifier: AGPL-3.0-or-later

export const Axis = Object.freeze({
	HORIZONTAL: 'horizontal',
	VERTICAL: 'vertical',
} as const);

export type Axis = (typeof Axis)[keyof typeof Axis];

export const AxisOrientation = Object.freeze({
	HORIZONTAL: Axis.HORIZONTAL,
	VERTICAL: Axis.VERTICAL,
	BOTH: 'both',
} as const);

export type AxisOrientation = (typeof AxisOrientation)[keyof typeof AxisOrientation];

export const Edge = Object.freeze({
	LEFT: 'left',
	RIGHT: 'right',
	TOP: 'top',
	BOTTOM: 'bottom',
} as const);

export type Edge = (typeof Edge)[keyof typeof Edge];
export type HorizontalEdge = typeof Edge.LEFT | typeof Edge.RIGHT;
export type VerticalEdge = typeof Edge.TOP | typeof Edge.BOTTOM;
