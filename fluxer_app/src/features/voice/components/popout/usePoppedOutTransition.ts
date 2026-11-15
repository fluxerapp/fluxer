// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	createPoppedOutSurfaceSnapshot,
	isPoppedOutSurfaceTransitioning,
	POPPED_OUT_TRANSITION_FALLBACK_MS,
	type PoppedOutSurfaceSnapshot,
	transitionPoppedOutSurfaceSnapshot,
} from '@app/features/voice/components/popout/PoppedOutSurfaceStateMachine';
import {useCallback, useEffect, useState} from 'react';

export interface PoppedOutTransition {
	snapshot: PoppedOutSurfaceSnapshot;
	handleTransitionEnd: () => void;
}

export function usePoppedOutTransition(isPoppedOut: boolean): PoppedOutTransition {
	const [snapshot, setSnapshot] = useState(() => createPoppedOutSurfaceSnapshot(isPoppedOut));
	useEffect(() => {
		setSnapshot((current) => transitionPoppedOutSurfaceSnapshot(current, {type: 'popout.update', isPoppedOut}));
	}, [isPoppedOut]);
	const handleTransitionEnd = useCallback(() => {
		setSnapshot((current) => transitionPoppedOutSurfaceSnapshot(current, {type: 'popout.transition-end'}));
	}, []);
	useEffect(() => {
		if (!isPoppedOutSurfaceTransitioning(snapshot)) return;
		const delay = Accessibility.useReducedMotion ? 0 : POPPED_OUT_TRANSITION_FALLBACK_MS;
		const timeout = window.setTimeout(handleTransitionEnd, delay);
		return () => window.clearTimeout(timeout);
	}, [snapshot, handleTransitionEnd]);
	return {snapshot, handleTransitionEnd};
}
