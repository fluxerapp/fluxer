// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const SKELETON_FADE_OUT_CLEANUP_MS = 200;

export interface LoadingSkeletonRetentionRequest {
	readonly shouldShowLoadingSkeleton: boolean;
}

export interface LoadingSkeletonRetention {
	readonly isLoadingSkeletonRetained: boolean;
	readonly handleLoadingSkeletonTransitionEnd: React.TransitionEventHandler<HTMLElement>;
}

function completesLoadingSkeletonFadeOut(event: React.TransitionEvent<HTMLElement>): boolean {
	if (event.target !== event.currentTarget) {
		return false;
	}
	return event.propertyName === 'opacity';
}

export function useLoadingSkeletonRetention({
	shouldShowLoadingSkeleton,
}: LoadingSkeletonRetentionRequest): LoadingSkeletonRetention {
	const [isLoadingSkeletonRetained, setIsLoadingSkeletonRetained] = useState(shouldShowLoadingSkeleton);

	useEffect(() => {
		if (shouldShowLoadingSkeleton) {
			setIsLoadingSkeletonRetained(true);
			return;
		}
		if (!isLoadingSkeletonRetained) {
			return;
		}
		const timeoutId = window.setTimeout(() => {
			setIsLoadingSkeletonRetained(false);
		}, SKELETON_FADE_OUT_CLEANUP_MS);
		return () => window.clearTimeout(timeoutId);
	}, [isLoadingSkeletonRetained, shouldShowLoadingSkeleton]);

	const handleLoadingSkeletonTransitionEnd = useCallback(
		(event: React.TransitionEvent<HTMLElement>) => {
			if (shouldShowLoadingSkeleton) {
				return;
			}
			if (!completesLoadingSkeletonFadeOut(event)) {
				return;
			}
			setIsLoadingSkeletonRetained(false);
		},
		[shouldShowLoadingSkeleton],
	);

	return {isLoadingSkeletonRetained, handleLoadingSkeletonTransitionEnd};
}
