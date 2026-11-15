// SPDX-License-Identifier: AGPL-3.0-or-later

import Idle from '@app/features/ui/state/Idle';
import {useCallback, useRef} from 'react';

const THROTTLE_INTERVAL = 2000;

export const useActivityRecorder = () => {
	const lastActivityRef = useRef(0);
	return useCallback((force = false) => {
		const now = Date.now();
		if (force || now - lastActivityRef.current > THROTTLE_INTERVAL) {
			lastActivityRef.current = now;
			Idle.recordActivity();
		}
	}, []);
};
