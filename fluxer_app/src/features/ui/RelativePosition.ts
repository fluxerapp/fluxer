// SPDX-License-Identifier: AGPL-3.0-or-later

export const RelativePosition = Object.freeze({
	BEFORE: 'before',
	AFTER: 'after',
} as const);

export type RelativePosition = (typeof RelativePosition)[keyof typeof RelativePosition];
