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

import {useEffect, useState} from 'react';

interface ImageDimensions {
	width: number;
	height: number;
}

interface PatternImageLoaderResult {
	patternReady: boolean;
}

interface SplashImageLoaderResult {
	loaded: boolean;
	dimensions: ImageDimensions | null;
}

interface AuthBackgroundResult {
	patternReady: boolean;
	splashLoaded: boolean;
	splashDimensions: ImageDimensions | null;
}

export function usePatternImageLoader(patternUrl: string): PatternImageLoaderResult {
	const [patternReady, setPatternReady] = useState(false);

	useEffect(() => {
		const img = new Image();
		const handleLoad = () => setPatternReady(true);
		img.addEventListener('load', handleLoad, {once: true});
		img.src = patternUrl;
		return () => img.removeEventListener('load', handleLoad);
	}, [patternUrl]);

	return {patternReady};
}

export function useSplashImageLoader(imageUrl: string | null): SplashImageLoaderResult {
	const [loaded, setLoaded] = useState(false);
	const [dimensions, setDimensions] = useState<ImageDimensions | null>(null);

	useEffect(() => {
		if (!imageUrl) {
			setLoaded(false);
			setDimensions(null);
			return;
		}

		let isMounted = true;
		const img = new Image();
		const handleLoad = () => {
			if (!isMounted) return;
			setLoaded(true);
			setDimensions({
				width: img.naturalWidth,
				height: img.naturalHeight,
			});
		};

		img.addEventListener('load', handleLoad, {once: true});
		img.src = imageUrl;

		return () => {
			isMounted = false;
		};
	}, [imageUrl]);

	return {loaded, dimensions};
}

export function useAuthBackground(splashUrl: string | null, patternUrl: string): AuthBackgroundResult {
	const {patternReady} = usePatternImageLoader(patternUrl);
	const {loaded: splashLoaded, dimensions: splashDimensions} = useSplashImageLoader(splashUrl);

	return {
		patternReady,
		splashLoaded,
		splashDimensions,
	};
}
