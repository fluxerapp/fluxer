// SPDX-License-Identifier: AGPL-3.0-or-later

import AttachmentUrlRefresher from '@app/features/messaging/state/AttachmentUrlRefresher';
import {useCallback, useRef} from 'react';

export function useAttachmentRefreshOnError(src: string | undefined): () => void {
	const refreshedSourceRef = useRef<string | null>(null);
	return useCallback(() => {
		if (src === undefined || src.length === 0) return;
		if (refreshedSourceRef.current === src) return;
		refreshedSourceRef.current = src;
		void AttachmentUrlRefresher.refresh(src, {force: true});
	}, [src]);
}
