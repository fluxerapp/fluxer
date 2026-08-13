// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import type {RefObject, UIEvent} from 'react';
import {useCallback, useEffect, useRef} from 'react';

export interface PreservedScrollerPosition {
	scrollerRef: RefObject<ScrollerHandle | null>;
	handleScroll: (event: UIEvent<HTMLDivElement>) => void;
}

export function usePreservedScrollerPosition(active: boolean): PreservedScrollerPosition {
	const scrollPositionRef = useRef(0);
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const handleScroll = useCallback((event: UIEvent<HTMLDivElement>) => {
		scrollPositionRef.current = event.currentTarget.scrollTop;
	}, []);
	useEffect(() => {
		if (!active) return;
		const scroller = scrollerRef.current;
		if (scroller == null) return;
		const target = scrollPositionRef.current;
		if (target === 0) return;
		scroller.scrollTo({to: target, animate: false});
	}, [active]);
	return {scrollerRef, handleScroll};
}
