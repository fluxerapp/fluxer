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

import * as React from 'react';

export const useMergeRefs = <T>(
	refs: Array<React.MutableRefObject<T | null> | React.LegacyRef<T> | undefined | null>,
): React.RefCallback<T> => {
	const latestRefs = React.useRef(refs);
	latestRefs.current = refs;

	return React.useCallback((value: T | null) => {
		for (const ref of latestRefs.current) {
			if (typeof ref === 'function') {
				ref(value);
			} else if (ref != null) {
				(ref as React.MutableRefObject<T | null>).current = value;
			}
		}
	}, []);
};
