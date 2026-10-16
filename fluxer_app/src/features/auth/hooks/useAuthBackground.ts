// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {useEffect, useState} from 'react';

interface ImageDimensions {
	width: number;
	height: number;
}

interface PatternImageLoaderResult {
	patternReady: boolean;
}

interface SplashImageLoaderResult {
	dimensions: ImageDimensions | null;
}

interface AuthBackgroundResult {
	patternReady: boolean;
	splashDimensions: ImageDimensions | null;
}

export function usePatternImageLoader(patternUrl: string): PatternImageLoaderResult {
	const [patternReady, setPatternReady] = useState(() => ImageCacheUtils.hasImage(patternUrl));
	useEffect(() => {
		if (ImageCacheUtils.hasImage(patternUrl)) {
			setPatternReady(true);
			return;
		}
		setPatternReady(false);
		return ImageCacheUtils.loadImage(patternUrl, () => setPatternReady(true));
	}, [patternUrl]);
	return {patternReady};
}

export function useSplashImageLoader(imageUrl: string | null): SplashImageLoaderResult {
	const [dimensions, setDimensions] = useState<ImageDimensions | null>(
		() => ImageCacheUtils.getImageSize(imageUrl) ?? null,
	);
	useEffect(() => {
		const cached = ImageCacheUtils.getImageSize(imageUrl);
		if (cached) {
			setDimensions(cached);
			return;
		}
		setDimensions(null);
		if (!imageUrl) {
			return;
		}
		return ImageCacheUtils.loadImage(imageUrl, () => {
			setDimensions(ImageCacheUtils.getImageSize(imageUrl) ?? null);
		});
	}, [imageUrl]);
	return {dimensions};
}

export function useAuthBackground(splashUrl: string | null, patternUrl: string): AuthBackgroundResult {
	const {patternReady} = usePatternImageLoader(patternUrl);
	const {dimensions: splashDimensions} = useSplashImageLoader(splashUrl);
	return {
		patternReady,
		splashDimensions,
	};
}
