// SPDX-License-Identifier: AGPL-3.0-or-later

import {type FocusEvent, useCallback, useEffect, useRef} from 'react';

const TAB_KEY_RECENCY_THRESHOLD_MS = 100;

export function useTabKeyFocusGuard(): (event: FocusEvent<HTMLElement>) => void {
	const lastTabKeyAtRef = useRef(0);
	useEffect(() => {
		const onKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Tab') lastTabKeyAtRef.current = performance.now();
		};
		document.addEventListener('keydown', onKeyDown, true);
		return () => document.removeEventListener('keydown', onKeyDown, true);
	}, []);
	return useCallback((event: FocusEvent<HTMLElement>) => {
		if (performance.now() - lastTabKeyAtRef.current > TAB_KEY_RECENCY_THRESHOLD_MS) {
			event.currentTarget.blur();
		}
	}, []);
}
