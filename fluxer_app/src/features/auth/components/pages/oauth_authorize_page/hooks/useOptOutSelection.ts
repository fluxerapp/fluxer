// SPDX-License-Identifier: AGPL-3.0-or-later

import {useCallback, useMemo, useState} from 'react';

const neverLocked = () => false;

export function useOptOutSelection(requested: ReadonlyArray<string>, isLocked: (key: string) => boolean = neverLocked) {
	const [overrides, setOverrides] = useState<ReadonlyMap<string, boolean>>(() => new Map());
	const selected = useMemo(() => {
		const next = new Set<string>();
		for (const key of requested) {
			if (overrides.get(key) === false && !isLocked(key)) continue;
			next.add(key);
		}
		return next;
	}, [requested, overrides, isLocked]);
	const toggle = useCallback(
		(key: string) => {
			if (isLocked(key)) return;
			setOverrides((prev) => {
				const next = new Map(prev);
				if (prev.get(key) === false) next.delete(key);
				else next.set(key, false);
				return next;
			});
		},
		[isLocked],
	);
	const adjusted = requested.length > 0 && selected.size < requested.length;
	return {selected, toggle, isLocked, adjusted};
}
