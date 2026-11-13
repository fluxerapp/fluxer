// SPDX-License-Identifier: AGPL-3.0-or-later

import apiAttachmentFileTokens, {isFreshFileToken} from '@app/features/messaging/state/ApiAttachmentFileTokens';
import apiAttachmentMedia, {fetchedApiAttachmentKey} from '@app/features/messaging/state/ApiAttachmentMedia';
import {withAttachmentToken} from '@app/features/messaging/utils/ApiAttachmentUrl';
import {useEffect, useRef, useSyncExternalStore} from 'react';

function useFetchedApiAttachmentKey(src: string | undefined): string | null {
	useSyncExternalStore(apiAttachmentMedia.subscribe, apiAttachmentMedia.isHeaderMode);
	return src === undefined || src.length === 0 ? null : fetchedApiAttachmentKey(src);
}

export function useApiAttachmentSrc(src: string, enabled = true): string | undefined {
	const key = useFetchedApiAttachmentKey(src);
	const target = enabled ? key : null;
	useEffect(() => (target === null ? undefined : apiAttachmentMedia.retain(target)), [target]);
	const state = useSyncExternalStore(apiAttachmentMedia.subscribe, () =>
		target === null ? null : apiAttachmentMedia.read(target),
	);
	if (target === null || state === null) return src;
	if (state.status === 'ready') return state.objectUrl;
	return state.status === 'failed' ? target : undefined;
}

export function useApiAttachmentPlaybackSrc(src: string | undefined): string | undefined {
	const target = useFetchedApiAttachmentKey(src);
	const issued = useSyncExternalStore(apiAttachmentFileTokens.subscribe, () =>
		target === null ? null : apiAttachmentFileTokens.read(target),
	);
	const attachedRef = useRef<{target: string; url: string} | null>(null);
	useEffect(() => {
		if (target !== null) apiAttachmentFileTokens.ensure(target);
	}, [target, issued]);
	if (target === null) return src;
	const attached = attachedRef.current?.target === target ? attachedRef.current.url : null;
	if (issued === 'failed') return attached ?? target;
	if (issued === null || (!isFreshFileToken(issued) && attached === null)) return attached ?? undefined;
	const url = withAttachmentToken(target, issued.token);
	attachedRef.current = {target, url};
	return url;
}
