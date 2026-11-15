// SPDX-License-Identifier: AGPL-3.0-or-later

import {TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {useCallback, useRef} from 'react';

export function useTextareaSegments() {
	const segmentManagerRef = useRef(new TextareaSegmentManager());
	const previousValueRef = useRef('');
	const displayToActual = useCallback(
		(displayText: string) => segmentManagerRef.current.displayToActual(displayText),
		[],
	);
	const clearSegments = useCallback(() => {
		segmentManagerRef.current.clear();
		previousValueRef.current = '';
	}, []);
	return {segmentManagerRef, previousValueRef, displayToActual, clearSegments};
}
