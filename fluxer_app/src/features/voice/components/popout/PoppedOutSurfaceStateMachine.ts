// SPDX-License-Identifier: AGPL-3.0-or-later
export const POPPED_OUT_TRANSITION_FALLBACK_MS = 480;

type PoppedOutSurfacePhase = 'live' | 'popping-out' | 'popped' | 'restoring';

export interface PoppedOutSurfaceSnapshot {
	phase: PoppedOutSurfacePhase;
}

export type PoppedOutSurfaceEvent = {type: 'popout.update'; isPoppedOut: boolean} | {type: 'popout.transition-end'};

export function createPoppedOutSurfaceSnapshot(isPoppedOut: boolean): PoppedOutSurfaceSnapshot {
	return {phase: isPoppedOut ? 'popped' : 'live'};
}

export function transitionPoppedOutSurfaceSnapshot(
	snapshot: PoppedOutSurfaceSnapshot,
	event: PoppedOutSurfaceEvent,
): PoppedOutSurfaceSnapshot {
	switch (event.type) {
		case 'popout.update': {
			if (event.isPoppedOut) {
				if (snapshot.phase === 'popped' || snapshot.phase === 'popping-out') return snapshot;
				return {phase: 'popping-out'};
			}
			if (snapshot.phase === 'live' || snapshot.phase === 'restoring') return snapshot;
			return {phase: 'restoring'};
		}
		case 'popout.transition-end': {
			if (snapshot.phase === 'popping-out') return {phase: 'popped'};
			if (snapshot.phase === 'restoring') return {phase: 'live'};
			return snapshot;
		}
	}
}

export function shouldRenderPoppedOutOverlay(snapshot: PoppedOutSurfaceSnapshot): boolean {
	return snapshot.phase !== 'live';
}

export function isPoppedOutSurfaceTransitioning(snapshot: PoppedOutSurfaceSnapshot): boolean {
	return snapshot.phase === 'popping-out' || snapshot.phase === 'restoring';
}

export type PoppedOutOverlayTransition = 'enter' | 'static' | 'exit';

export function selectPoppedOutOverlayTransition(snapshot: PoppedOutSurfaceSnapshot): PoppedOutOverlayTransition {
	if (snapshot.phase === 'popping-out') return 'enter';
	if (snapshot.phase === 'restoring') return 'exit';
	return 'static';
}
