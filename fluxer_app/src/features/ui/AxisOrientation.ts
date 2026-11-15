// SPDX-License-Identifier: AGPL-3.0-or-later

export const Edge = Object.freeze({
	LEFT: 'left',
	RIGHT: 'right',
	TOP: 'top',
	BOTTOM: 'bottom',
} as const);

export type Edge = (typeof Edge)[keyof typeof Edge];
export type VerticalEdge = typeof Edge.TOP | typeof Edge.BOTTOM;
