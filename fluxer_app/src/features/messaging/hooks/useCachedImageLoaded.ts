// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {useEffect, useState} from 'react';

export function useCachedImageLoaded(src: string | null | undefined): boolean {
	const resolved = src ?? null;
	const [readySrc, setReadySrc] = useState<string | null>(() =>
		resolved !== null && ImageCacheUtils.hasImage(resolved) ? resolved : null,
	);
	useEffect(() => {
		if (resolved === null) {
			return;
		}
		let active = true;
		const markReady = (): void => {
			if (active) setReadySrc(resolved);
		};
		const cleanup = ImageCacheUtils.loadImage(resolved, markReady, markReady);
		return () => {
			active = false;
			cleanup();
		};
	}, [resolved]);
	return resolved !== null && readySrc === resolved;
}
