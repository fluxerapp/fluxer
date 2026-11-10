// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {afterBootReveal, isBootRevealHeld} from '@app/features/boot_state/BootReveal';
import {runInAction} from 'mobx';

let phase: 'idle' | 'visible' | 'complete' = 'idle';
let generation = 0;
let remaining: Generator<void> | null = null;
let discard: (() => void) | null = null;
let cancelScheduled: (() => void) | null = null;

export function bootHydrationPhase(): typeof phase {
	return phase;
}

function detach(): void {
	cancelScheduled?.();
	cancelScheduled = null;
	if (typeof window !== 'undefined') {
		window.removeEventListener('pointerdown', flushBootHydration, true);
		window.removeEventListener('keydown', flushBootHydration, true);
		window.removeEventListener('pointermove', flushBootHydration, true);
	}
}

export function resetBootHydration(): void {
	generation += 1;
	detach();
	remaining?.return(undefined);
	remaining = null;
	discard?.();
	discard = null;
	phase = 'idle';
}

function advance(deadline: number): void {
	const current = generation;
	try {
		runInAction(() => {
			while (phase === 'visible' && current === generation && remaining !== null) {
				if (remaining.next().done) {
					remaining = null;
					discard = null;
					phase = 'complete';
					detach();
					return;
				}
				if (performance.now() >= deadline) return;
			}
		});
	} catch (error) {
		resetBootHydration();
		throw error;
	}
}

export function flushBootHydration(): void {
	if (phase !== 'visible') return;
	cancelScheduled?.();
	cancelScheduled = null;
	advance(Number.POSITIVE_INFINITY);
}

function scheduleSlice(current: number): void {
	if (current !== generation || phase !== 'visible') return;
	const run = (): void => {
		if (current !== generation || phase !== 'visible') return;
		cancelScheduled = null;
		advance(performance.now() + 8);
		scheduleSlice(current);
	};
	if (typeof requestIdleCallback === 'function') {
		const id = requestIdleCallback(run, {timeout: 1000});
		cancelScheduled = () => cancelIdleCallback(id);
	} else {
		const id = setTimeout(run, 0);
		cancelScheduled = () => clearTimeout(id);
	}
}

export function beginBootHydration(rest: Generator<void>, onDiscard: () => void): void {
	resetBootHydration();
	remaining = rest;
	discard = onDiscard;
	phase = 'visible';
	const current = generation;
	window.addEventListener('pointerdown', flushBootHydration, true);
	window.addEventListener('keydown', flushBootHydration, true);
	const afterReveal = (): void => {
		if (current !== generation || phase !== 'visible') return;
		window.addEventListener('pointermove', flushBootHydration, true);
		scheduleSlice(current);
	};
	if (isBootRevealHeld()) {
		cancelScheduled = afterBootReveal(afterReveal);
	} else {
		afterReveal();
	}
}

ResettableStates.register(resetBootHydration, resetBootHydration);
