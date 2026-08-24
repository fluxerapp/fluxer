// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {useEffect, useRef, useState} from 'react';

type QuickReactionSnapshot = ReadonlyArray<FlatEmoji>;
type EmojiState = typeof import('@app/features/emoji/state/Emoji')['default'];
type EmojiPickerState = typeof import('@app/features/emoji/state/EmojiPicker')['default'];
type PermissionState = typeof import('@app/features/permissions/state/Permission')['default'];
type ComponentBusState = typeof import('@app/features/platform/utils/ComponentBus')['ComponentBus'];

interface QuickReactionDependencies {
	Emoji: EmojiState;
	EmojiPicker: EmojiPickerState;
	Permission: PermissionState;
	ComponentBus: ComponentBusState;
}

const EMPTY_QUICK_REACTIONS: QuickReactionSnapshot = Object.freeze([]);
const MAX_CACHE_ENTRIES = 80;

let dependencies: QuickReactionDependencies | null = null;
let dependenciesPromise: Promise<QuickReactionDependencies> | null = null;
let cacheEpoch = 0;
const snapshotCache = new Map<string, QuickReactionSnapshot>();

function loadQuickReactionDependencies(): Promise<QuickReactionDependencies> {
	if (!dependenciesPromise) {
		dependenciesPromise = loadLazyModule(() =>
			Promise.all([
				import('@app/features/emoji/state/Emoji'),
				import('@app/features/emoji/state/EmojiPicker'),
				import('@app/features/permissions/state/Permission'),
				import('@app/features/platform/utils/ComponentBus'),
			]).then(([emojiModule, emojiPickerModule, permissionModule, componentBusModule]) => {
				const loaded: QuickReactionDependencies = {
					Emoji: emojiModule.default,
					EmojiPicker: emojiPickerModule.default,
					Permission: permissionModule.default,
					ComponentBus: componentBusModule.ComponentBus,
				};
				if (!dependencies) {
					dependencies = loaded;
					loaded.ComponentBus.subscribe('EMOJI_PICKER_RERENDER', () => {
						cacheEpoch += 1;
					});
					loaded.Permission.subscribe(() => {
						cacheEpoch += 1;
					});
				}
				return dependencies;
			}),
		).catch((error) => {
			dependenciesPromise = null;
			throw error;
		});
	}
	return dependenciesPromise;
}

function getCacheKey(channel: Channel | null, count: number, rankingVersion: number): string {
	const channelScope = channel ? `${channel.guildId ?? 'dm'}:${channel.id}` : 'global';
	return `${rankingVersion}:${cacheEpoch}:${channelScope}:${count}`;
}

function computeQuickReactions(channel: Channel | null, count: number): QuickReactionSnapshot {
	if (!dependencies || count <= 0) {
		return EMPTY_QUICK_REACTIONS;
	}
	const rankingVersion = dependencies.EmojiPicker.getRanking().version;
	const key = getCacheKey(channel, count, rankingVersion);
	const cached = snapshotCache.get(key);
	if (cached) {
		return cached;
	}
	const snapshot = Object.freeze(dependencies.Emoji.getQuickReactionEmojis(channel, count));
	snapshotCache.set(key, snapshot);
	while (snapshotCache.size > MAX_CACHE_ENTRIES) {
		const oldestKey = snapshotCache.keys().next().value;
		if (oldestKey === undefined) break;
		snapshotCache.delete(oldestKey);
	}
	return snapshot;
}

export function useQuickReactionEmojis(
	channel: Channel | null,
	count: number,
	enabled: boolean,
): QuickReactionSnapshot {
	const [ready, setReady] = useState(dependencies != null);
	useEffect(() => {
		if (!enabled || ready) {
			return undefined;
		}
		let live = true;
		void loadQuickReactionDependencies().then(() => {
			if (live) {
				setReady(true);
			}
		});
		return () => {
			live = false;
		};
	}, [enabled, ready]);
	const normalizedCount = Math.max(0, Math.floor(count));
	const active = enabled && ready && normalizedCount > 0;
	const pinKey = `${active}:${channel?.id ?? ''}:${normalizedCount}`;
	const pinned = useRef<{key: string; value: QuickReactionSnapshot} | null>(null);
	if (pinned.current?.key !== pinKey) {
		pinned.current = {
			key: pinKey,
			value: active ? computeQuickReactions(channel, normalizedCount) : EMPTY_QUICK_REACTIONS,
		};
	}
	return pinned.current.value;
}
