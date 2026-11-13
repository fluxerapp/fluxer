// SPDX-License-Identifier: AGPL-3.0-or-later

import apiAttachmentFileTokens, {isFreshFileToken} from '@app/features/messaging/state/ApiAttachmentFileTokens';
import apiAttachmentMedia, {fetchedApiAttachmentKey} from '@app/features/messaging/state/ApiAttachmentMedia';
import {withAttachmentToken} from '@app/features/messaging/utils/ApiAttachmentUrl';
import {useEffect, useRef, useState, useSyncExternalStore} from 'react';

interface ApiAttachmentSource {
	src: string | undefined;
	failed: boolean;
}

interface AttachedPlaybackSource {
	target: string;
	epoch: number;
	url: string;
}

function useFetchedApiAttachmentKey(src: string | undefined): string | null {
	useSyncExternalStore(apiAttachmentMedia.subscribe, apiAttachmentMedia.isHeaderMode);
	return src === undefined || src.length === 0 ? null : fetchedApiAttachmentKey(src);
}

export function useApiAttachmentSource(src: string, enabled = true): ApiAttachmentSource {
	const key = useFetchedApiAttachmentKey(src);
	const target = enabled ? key : null;
	const generation = useSyncExternalStore(apiAttachmentMedia.subscribe, apiAttachmentMedia.currentGeneration);
	useEffect(() => (target === null ? undefined : apiAttachmentMedia.retain(target)), [target, generation]);
	const state = useSyncExternalStore(apiAttachmentMedia.subscribe, () =>
		target === null ? null : apiAttachmentMedia.read(target),
	);
	if (key === null) return {src, failed: false};
	if (state === null) return {src: undefined, failed: false};
	if (state.status === 'ready') return {src: state.objectUrl, failed: false};
	return {src: undefined, failed: state.status === 'failed'};
}

export function useApiAttachmentSrc(src: string, enabled = true): string | undefined {
	return useApiAttachmentSource(src, enabled).src;
}

export function useApiAttachmentPlaybackSrc(src: string | undefined): string | undefined {
	const target = useFetchedApiAttachmentKey(src);
	const issued = useSyncExternalStore(apiAttachmentFileTokens.subscribe, () =>
		target === null ? null : apiAttachmentFileTokens.read(target),
	);
	const epoch = useSyncExternalStore(apiAttachmentFileTokens.subscribe, () =>
		target === null ? 0 : apiAttachmentFileTokens.epochOf(target),
	);
	const attachedRef = useRef<AttachedPlaybackSource | null>(null);
	const [retryRound, setRetryRound] = useState(0);
	const attached = attachedRef.current;
	const attachedUrl = attached !== null && attached.target === target && attached.epoch === epoch ? attached.url : null;
	const usableToken = issued !== null && issued !== 'failed' && isFreshFileToken(issued) ? issued : null;
	const needsToken = target !== null && attachedUrl === null && usableToken === null;
	useEffect(() => {
		if (!needsToken || target === null) return;
		apiAttachmentFileTokens.ensure(target);
		if (issued !== 'failed') return;
		const timer = setTimeout(() => setRetryRound((round) => round + 1), apiAttachmentFileTokens.retryInMs(target));
		return () => clearTimeout(timer);
	}, [needsToken, target, issued, retryRound]);
	if (target === null) return src;
	if (attachedUrl !== null) return attachedUrl;
	if (usableToken === null) return undefined;
	const url = withAttachmentToken(target, usableToken.token);
	attachedRef.current = {target, epoch, url};
	return url;
}
