// SPDX-License-Identifier: AGPL-3.0-or-later

import {stripAttachmentSignature} from '@app/features/messaging/utils/AttachmentCdnUrl';
import Permission from '@app/features/permissions/state/Permission';
import UserSettings from '@app/features/user/state/UserSettings';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {RenderSpoilers} from '@fluxer/constants/src/UserConstants';
import type React from 'react';
import {createContext, createElement, useCallback, useContext, useMemo, useState} from 'react';

const SPOILER_REGEX = /\|\|([\s\S]*?)\|\|/g;
const URL_REGEX = /https?:\/\/[^\s<>"']+/gi;
const YOUTUBE_HOSTS = new Set(['www.youtube.com', 'youtube.com', 'm.youtube.com', 'music.youtube.com', 'youtu.be']);

function extractYouTubeVideoId(parsed: URL): string | null {
	if (!YOUTUBE_HOSTS.has(parsed.hostname)) return null;
	if (parsed.hostname === 'youtu.be') {
		const id = parsed.pathname.slice(1).split('/')[0];
		return id || null;
	}
	if (parsed.pathname.startsWith('/shorts/')) {
		return parsed.pathname.split('/shorts/')[1]?.split('/')[0] || null;
	}
	if (parsed.pathname.startsWith('/v/')) {
		return parsed.pathname.split('/v/')[1]?.split('/')[0] || null;
	}
	if (parsed.pathname.startsWith('/embed/')) {
		return parsed.pathname.split('/embed/')[1]?.split('/')[0] || null;
	}
	if (parsed.pathname === '/watch') {
		return parsed.searchParams.get('v');
	}
	return null;
}

export function normalizeUrl(url: string): string | null {
	try {
		const parsed = new URL(url);
		return parsed.href.replace(/\/$/, '');
	} catch {
		return null;
	}
}

export function canonicalizeMediaUrl(url: string | null | undefined): string | null {
	if (!url) return null;
	try {
		const parsed = new URL(url);
		const youtubeId = extractYouTubeVideoId(parsed);
		if (youtubeId) {
			return `youtube:${youtubeId}`;
		}
		return stripAttachmentSignature(parsed.href).replace(/\/$/, '');
	} catch {
		return null;
	}
}

const getRenderSpoilersSetting = (): number => UserSettings.renderSpoilers;
const canAutoRevealForModerators = (channelId?: string): boolean => {
	if (!channelId) return false;
	const channelPermissions = Permission.getChannelPermissions(channelId);
	return channelPermissions ? (channelPermissions & Permissions.MANAGE_MESSAGES) !== 0n : false;
};

export function extractSpoileredUrls(content: string | null | undefined): Set<string> {
	const spoileredUrls = new Set<string>();
	if (!content) return spoileredUrls;
	for (const match of content.matchAll(SPOILER_REGEX)) {
		const spoilerBody = match[1];
		if (!spoilerBody) continue;
		for (const urlMatch of spoilerBody.matchAll(URL_REGEX)) {
			const canonical = canonicalizeMediaUrl(urlMatch[0]);
			if (canonical) {
				spoileredUrls.add(canonical);
			}
		}
	}
	return spoileredUrls;
}

interface SpoilerSyncContextValue {
	isRevealed: (keys: Array<string>) => boolean;
	reveal: (keys: Array<string>) => void;
}

const SpoilerSyncContext = createContext<SpoilerSyncContextValue | null>(null);
export const SpoilerSyncProvider: React.FC<{
	children: React.ReactNode;
}> = ({children}) => {
	const [revealedKeys, setRevealedKeys] = useState<Set<string>>(new Set());
	const reveal = useCallback((keys: Array<string>) => {
		if (keys.length === 0) return;
		setRevealedKeys((prev) => {
			let changed = false;
			const next = new Set(prev);
			for (const key of keys) {
				if (!next.has(key)) {
					next.add(key);
					changed = true;
				}
			}
			return changed ? next : prev;
		});
	}, []);
	const isRevealed = useCallback(
		(keys: Array<string>) => {
			if (keys.length === 0) return false;
			for (const key of keys) {
				if (revealedKeys.has(key)) return true;
			}
			return false;
		},
		[revealedKeys],
	);
	const value = useMemo(() => ({isRevealed, reveal}), [isRevealed, reveal]);
	return createElement(SpoilerSyncContext.Provider, {value}, children);
};

export function useSpoilerState(
	isSpoiler: boolean,
	channelId?: string,
	syncKeys: ReadonlyArray<string> = [],
): {
	hidden: boolean;
	reveal: () => void;
	autoRevealed: boolean;
} {
	const [manuallyRevealed, setManuallyRevealed] = useState(false);
	const spoilerSync = useContext(SpoilerSyncContext);
	const renderSpoilersSetting = getRenderSpoilersSetting();
	const autoReveal = useMemo(() => {
		if (!isSpoiler) return true;
		switch (renderSpoilersSetting) {
			case RenderSpoilers.ALWAYS:
				return true;
			case RenderSpoilers.IF_MODERATOR:
				return canAutoRevealForModerators(channelId);
			default:
				return false;
		}
	}, [channelId, isSpoiler, renderSpoilersSetting]);
	const normalizedKeys = useMemo(() => Array.from(new Set(syncKeys)), [syncKeys]);
	const sharedRevealed = useMemo(() => spoilerSync?.isRevealed(normalizedKeys) ?? false, [spoilerSync, normalizedKeys]);
	const hidden = isSpoiler && !autoReveal && !manuallyRevealed && !sharedRevealed;
	const reveal = useCallback(() => {
		if (!manuallyRevealed) {
			setManuallyRevealed(true);
		}
		if (normalizedKeys.length > 0) {
			spoilerSync?.reveal(normalizedKeys);
		}
	}, [manuallyRevealed, normalizedKeys, spoilerSync]);
	return {hidden, reveal, autoRevealed: autoReveal || sharedRevealed};
}
