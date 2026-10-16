// SPDX-License-Identifier: AGPL-3.0-or-later

const TOOLTIP_HANDOFF_GRACE_MS = 300;

let openCount = 0;
let warmUntil = 0;
let graceTimer: ReturnType<typeof setTimeout> | null = null;

function now(): number {
	return typeof performance === 'undefined' ? Date.now() : performance.now();
}

function clearGraceTimer(): void {
	if (graceTimer == null) return;
	clearTimeout(graceTimer);
	graceTimer = null;
}

function armGraceTimer(): void {
	clearGraceTimer();
	const remaining = warmUntil - now();
	if (remaining <= 0) return;
	graceTimer = setTimeout(() => {
		graceTimer = null;
		warmUntil = 0;
	}, remaining);
}

export function markTooltipOpen(): () => void {
	openCount += 1;
	warmUntil = 0;
	clearGraceTimer();
	let released = false;
	return () => {
		if (released) return;
		released = true;
		openCount = Math.max(0, openCount - 1);
		if (openCount === 0) {
			warmUntil = now() + TOOLTIP_HANDOFF_GRACE_MS;
			armGraceTimer();
		}
	};
}

export function isTooltipHandoffWarm(): boolean {
	return openCount > 0 || now() < warmUntil;
}
