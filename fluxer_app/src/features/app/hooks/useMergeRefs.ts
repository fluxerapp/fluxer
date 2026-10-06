// SPDX-License-Identifier: AGPL-3.0-or-later

import {type Ref, type RefCallback, useCallback, useRef} from 'react';

export function useMergeRefs<T>(refs: Array<Ref<T> | undefined>): RefCallback<T> {
	const latestRefs = useRef(refs);
	latestRefs.current = refs;
	return useCallback((value: T | null) => {
		for (const ref of latestRefs.current) {
			if (typeof ref === 'function') {
				ref(value);
			} else if (ref != null) {
				ref.current = value;
			}
		}
	}, []);
}
