// SPDX-License-Identifier: AGPL-3.0-or-later

import type {QuickSwitcherResult} from '@app/features/search/state/QuickSwitcherTypes';

function getResultSubtitle(result: QuickSwitcherResult): string | undefined {
	return 'subtitle' in result ? result.subtitle : undefined;
}

export function hasSameResultIdentity(
	previous: ReadonlyArray<QuickSwitcherResult>,
	next: ReadonlyArray<QuickSwitcherResult>,
): boolean {
	if (previous.length !== next.length) {
		return false;
	}
	for (let i = 0; i < previous.length; i += 1) {
		if (previous[i].type !== next[i].type || previous[i].id !== next[i].id) {
			return false;
		}
		if (previous[i].title !== next[i].title) {
			return false;
		}
		if (getResultSubtitle(previous[i]) !== getResultSubtitle(next[i])) {
			return false;
		}
	}
	return true;
}

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
