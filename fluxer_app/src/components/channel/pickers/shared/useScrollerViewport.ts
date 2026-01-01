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
import type {ScrollerHandle} from '~/components/uikit/Scroller';

type ResizeType = 'container' | 'content';

export function useScrollerViewport(scrollerRef: React.RefObject<ScrollerHandle | null>) {
	const [viewportSize, setViewportSize] = React.useState({width: 0, height: 0});
	const [scrollTop, setScrollTop] = React.useState(0);

	const handleScroll = React.useCallback((event: React.UIEvent<HTMLDivElement>) => {
		setScrollTop(event.currentTarget.scrollTop);
	}, []);

	const handleResize = React.useCallback((entry: ResizeObserverEntry, type: ResizeType) => {
		if (type !== 'container') return;
		const {width, height} = entry.contentRect;

		setViewportSize((prev) => {
			if (prev.width === width && prev.height === height) return prev;
			return {width, height};
		});
	}, []);

	React.useLayoutEffect(() => {
		if (viewportSize.width > 0 && viewportSize.height > 0) return;

		const node = scrollerRef.current?.getScrollerNode();
		if (!node) return;

		const rect = node.getBoundingClientRect();
		if (rect.width === 0 || rect.height === 0) return;

		setViewportSize({width: rect.width, height: rect.height});
	}, [scrollerRef, viewportSize.width, viewportSize.height]);

	const scrollToTop = React.useCallback(() => {
		scrollerRef.current?.scrollTo({to: 0, animate: false});
		setScrollTop(0);
	}, [scrollerRef]);

	return {
		viewportSize,
		scrollTop,
		setScrollTop,
		handleScroll,
		handleResize,
		scrollToTop,
	};
}
