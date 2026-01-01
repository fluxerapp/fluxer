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

import {useEffect, useMemo, useState} from 'react';
import {thumbHashToDataURL} from 'thumbhash';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import * as ImageCacheUtils from '~/utils/ImageCacheUtils';

interface MediaLoadingState {
	loaded: boolean;
	error: boolean;
	thumbHashURL?: string;
}

export function useMediaLoading(src: string, placeholder?: string): MediaLoadingState {
	const [loadingState, setLoadingState] = useState<Omit<MediaLoadingState, 'thumbHashURL'>>({
		loaded: ImageCacheUtils.hasImage(src),
		error: false,
	});

	const thumbHashURL = useMemo(() => {
		if (!placeholder) return;
		try {
			const bytes = Uint8Array.from(atob(placeholder), (c) => c.charCodeAt(0));
			return thumbHashToDataURL(bytes);
		} catch {
			return;
		}
	}, [placeholder]);

	useEffect(() => {
		if (DeveloperOptionsStore.forceRenderPlaceholders || DeveloperOptionsStore.forceMediaLoading) {
			return;
		}

		ImageCacheUtils.loadImage(
			src,
			() => setLoadingState({loaded: true, error: false}),
			() => setLoadingState({loaded: false, error: true}),
		);
	}, [src]);

	return {...loadingState, thumbHashURL};
}
