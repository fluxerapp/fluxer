// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {useCallback, useEffect, useState} from 'react';

export interface ImageRecoveryState {
	readonly failed: boolean;
	readonly reportError: () => void;
}

export function useImageRecovery(src: string): ImageRecoveryState {
	const [failedSrc, setFailedSrc] = useState<string | null>(() => (ImageCacheUtils.hasFailedImage(src) ? src : null));
	const failed = src.length > 0 && failedSrc === src;
	useEffect(() => {
		if (ImageCacheUtils.hasFailedImage(src)) setFailedSrc(src);
	}, [src]);
	useEffect(() => {
		if (!failed) return;
		return ImageCacheUtils.awaitImage(src, () => {
			setFailedSrc((current) => (current === src ? null : current));
		});
	}, [failed, src]);
	const reportError = useCallback(() => {
		ImageCacheUtils.reportImageError(src);
		setFailedSrc(src);
	}, [src]);
	return {failed, reportError};
}

export function useRecoveringBackgroundImageURL(src: string): string | null {
	const {failed, reportError} = useImageRecovery(src);
	useEffect(() => {
		if (failed || src.length === 0) return;
		return ImageCacheUtils.loadImage(src, () => {}, reportError);
	}, [failed, reportError, src]);
	return failed || src.length === 0 ? null : src;
}
