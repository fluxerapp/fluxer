// SPDX-License-Identifier: AGPL-3.0-or-later

const SCROLLABLE_OVERFLOW_VALUES = new Set(['auto', 'scroll', 'overlay']);

export function resolveEnclosingScrollSurface(node: Element | null): Element | null {
	let candidate = node?.parentElement ?? null;
	while (candidate) {
		const computedStyle = window.getComputedStyle(candidate);
		if (
			SCROLLABLE_OVERFLOW_VALUES.has(computedStyle.overflowY) ||
			SCROLLABLE_OVERFLOW_VALUES.has(computedStyle.overflow)
		) {
			return candidate;
		}
		candidate = candidate.parentElement;
	}
	return null;
}
