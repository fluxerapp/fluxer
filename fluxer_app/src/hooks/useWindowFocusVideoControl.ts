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

import type React from 'react';
import {useEffect, useRef} from 'react';
import type {ScrollerHandle} from '~/components/uikit/Scroller';

interface VideoPoolControl {
	pauseAll: () => void;
	resumeAll: () => void;
}

interface UseWindowFocusVideoControlOptions {
	scrollerRef: React.RefObject<ScrollerHandle | null>;
	videoPool: VideoPoolControl;
	gifAutoPlay?: boolean;
}

export function useWindowFocusVideoControl({
	scrollerRef,
	videoPool,
	gifAutoPlay = true,
}: UseWindowFocusVideoControlOptions): void {
	const poolRef = useRef(videoPool);
	poolRef.current = videoPool;

	const scrollerRefRef = useRef(scrollerRef);
	scrollerRefRef.current = scrollerRef;

	const gifAutoPlayRef = useRef(gifAutoPlay);
	gifAutoPlayRef.current = gifAutoPlay;

	useEffect(() => {
		if (!gifAutoPlay) {
			poolRef.current.pauseAll();
		} else if (!document.hidden && document.hasFocus()) {
			poolRef.current.resumeAll();
		}
	}, [gifAutoPlay]);

	useEffect(() => {
		const handleBlur = () => {
			const node = scrollerRefRef.current.current?.getScrollerNode();

			if (document.activeElement instanceof HTMLElement && node?.contains(document.activeElement)) {
				const scrollTop = node.scrollTop;
				document.activeElement.blur();
				node.scrollTop = scrollTop;
			}

			poolRef.current.pauseAll();
		};

		const handleFocus = () => {
			if (gifAutoPlayRef.current) {
				poolRef.current.resumeAll();
			}
		};

		const handleVisibilityChange = () => {
			if (document.hidden) {
				handleBlur();
			} else {
				handleFocus();
			}
		};

		window.addEventListener('blur', handleBlur);
		window.addEventListener('focus', handleFocus);
		document.addEventListener('visibilitychange', handleVisibilityChange);

		return () => {
			window.removeEventListener('blur', handleBlur);
			window.removeEventListener('focus', handleFocus);
			document.removeEventListener('visibilitychange', handleVisibilityChange);
		};
	}, []);
}
