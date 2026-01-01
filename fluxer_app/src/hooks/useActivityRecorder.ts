/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import IdleStore from '~/stores/IdleStore';

const DEFAULT_THROTTLE_INTERVAL = 2000;

interface UseActivityRecorderOptions {
	throttleInterval?: number;
}

export const useActivityRecorder = (options?: UseActivityRecorderOptions) => {
	const throttleInterval = options?.throttleInterval ?? DEFAULT_THROTTLE_INTERVAL;
	const lastActivityRef = React.useRef(0);

	return React.useCallback(
		(force = false) => {
			const now = Date.now();
			if (force || now - lastActivityRef.current > throttleInterval) {
				lastActivityRef.current = now;
				IdleStore.recordActivity();
			}
		},
		[throttleInterval],
	);
};
