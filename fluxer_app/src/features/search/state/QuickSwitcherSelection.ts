// SPDX-License-Identifier: AGPL-3.0-or-later

import type {QuickSwitcherResult} from '@app/features/search/state/QuickSwitcherTypes';

export function resolveRecomputedSelectedIndex(
	previous: QuickSwitcherResult | undefined,
	results: ReadonlyArray<QuickSwitcherResult>,
	fallbackIndex: number,
): number {
	if (!previous) {
		return fallbackIndex;
	}
	const restored = results.findIndex((result) => result.type === previous.type && result.id === previous.id);
	return restored >= 0 ? restored : fallbackIndex;
}
