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

import type React from 'react';
import {createContext, createElement, useCallback, useContext, useMemo, useState} from 'react';
import {Permissions, RenderSpoilers} from '~/Constants';
import PermissionStore from '~/stores/PermissionStore';
import UserSettingsStore from '~/stores/UserSettingsStore';

const SPOILER_REGEX = /\|\|([\s\S]*?)\|\|/g;
const URL_REGEX = /https?:\/\/[^\s<>"']+/gi;

export const normalizeUrl = (url: string): string | null => {
	try {
		const parsed = new URL(url);
		return parsed.href.replace(/\/$/, '');
	} catch {
		return null;
	}
};

const getRenderSpoilersSetting = (): number => UserSettingsStore.renderSpoilers;

const canAutoRevealForModerators = (channelId?: string): boolean => {
	if (!channelId) return false;
	const channelPermissions = PermissionStore.getChannelPermissions(channelId);
	return channelPermissions ? (channelPermissions & Permissions.MANAGE_MESSAGES) !== 0n : false;
};

export const extractSpoileredUrls = (content: string | null | undefined): Set<string> => {
	const spoileredUrls = new Set<string>();
	if (!content) return spoileredUrls;

	for (const match of content.matchAll(SPOILER_REGEX)) {
		const spoilerBody = match[1];
		if (!spoilerBody) continue;

		for (const urlMatch of spoilerBody.matchAll(URL_REGEX)) {
			const normalized = normalizeUrl(urlMatch[0]);
			if (normalized) {
				spoileredUrls.add(normalized);
			}
		}
	}

	return spoileredUrls;
};

interface SpoilerSyncContextValue {
	isRevealed: (keys: Array<string>) => boolean;
	reveal: (keys: Array<string>) => void;
}

const SpoilerSyncContext = createContext<SpoilerSyncContextValue | null>(null);

export const SpoilerSyncProvider: React.FC<{children: React.ReactNode}> = ({children}) => {
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

export const useSpoilerState = (
	isSpoiler: boolean,
	channelId?: string,
	syncKeys: Array<string> = [],
): {hidden: boolean; reveal: () => void; autoRevealed: boolean} => {
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
};
