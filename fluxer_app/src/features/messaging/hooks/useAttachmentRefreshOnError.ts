// SPDX-License-Identifier: AGPL-3.0-or-later

import AttachmentUrlRefresher from '@app/features/messaging/state/AttachmentUrlRefresher';
import {isApiAttachmentUrl} from '@app/features/messaging/utils/ApiAttachmentUrl';
import {unwrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopResourceUrl';
import {type SyntheticEvent, useCallback, useRef} from 'react';

type MediaErrorEvent = SyntheticEvent<HTMLImageElement | HTMLVideoElement>;

export function useAttachmentRefreshOnError(src: string | undefined): (event?: MediaErrorEvent) => void {
	const refreshedSourceRef = useRef<string | null>(null);
	return useCallback(
		(event?: MediaErrorEvent) => {
			if (src === undefined || src.length === 0) return;
			const failedSource =
				event !== undefined && isApiAttachmentUrl(unwrapDesktopLocalResourceURL(src))
					? event.currentTarget.currentSrc || src
					: src;
			if (refreshedSourceRef.current === failedSource) return;
			refreshedSourceRef.current = failedSource;
			void AttachmentUrlRefresher.refresh(src, {force: true});
		},
		[src],
	);
}
