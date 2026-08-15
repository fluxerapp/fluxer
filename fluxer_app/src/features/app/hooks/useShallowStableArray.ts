// SPDX-License-Identifier: AGPL-3.0-or-later

import {useRef} from 'react';

export function useShallowStableArray<T, Values extends ReadonlyArray<T>>(values: Values): Values {
	const stableValuesRef = useRef(values);
	const stableValues = stableValuesRef.current;
	if (stableValues.length !== values.length || values.some((value, index) => !Object.is(value, stableValues[index]))) {
		stableValuesRef.current = values;
	}
	return stableValuesRef.current;
}
