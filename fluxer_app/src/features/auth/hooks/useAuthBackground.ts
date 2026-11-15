// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {useEffect, useState} from 'react';

interface ImageDimensions {
	width: number;
	height: number;
}

interface SplashImageLoaderResult {
	dimensions: ImageDimensions | null;
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
