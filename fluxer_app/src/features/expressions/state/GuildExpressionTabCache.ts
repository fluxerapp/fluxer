// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	GuildEmoji,
	GuildEmojiWithUser,
	GuildSticker,
	GuildStickerWithUser,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {sortBySnowflakeDesc} from '@fluxer/snowflake/src/SnowflakeUtils';

type EmojiUpdateListener = (emojis: ReadonlyArray<GuildEmojiWithUser>) => void;
type StickerUpdateListener = (stickers: ReadonlyArray<GuildStickerWithUser>) => void;

const MAX_GUILD_EXPRESSION_TAB_CACHE_ENTRIES = 50;

const emojiCache = new Map<string, ReadonlyArray<GuildEmojiWithUser>>();
const stickerCache = new Map<string, ReadonlyArray<GuildStickerWithUser>>();
const emojiAccessSequence = new Set<string>();
const stickerAccessSequence = new Set<string>();
const emojiListeners = new Map<string, Set<EmojiUpdateListener>>();
const stickerListeners = new Map<string, Set<StickerUpdateListener>>();

function freezeList<T>(items: ReadonlyArray<T>): ReadonlyArray<T> {
	return Object.freeze([...items]);
}

function notifyListeners<T>(
	listeners: Map<string, Set<(items: ReadonlyArray<T>) => void>>,
	guildId: string,
	value: ReadonlyArray<T>,
) {
	const listenersForGuild = listeners.get(guildId);
	if (!listenersForGuild) return;
	for (const listener of listenersForGuild) {
		listener(value);
	}
}

function setCache<
	T extends {
		id: string;
	},
>(
	cache: Map<string, ReadonlyArray<T>>,
	accessSequence: Set<string>,
	listeners: Map<string, Set<(items: ReadonlyArray<T>) => void>>,
	guildId: string,
	value: ReadonlyArray<T>,
	shouldNotify: boolean,
) {
	const frozen = freezeList(sortBySnowflakeDesc(value));
	cache.set(guildId, frozen);
	accessSequence.delete(guildId);
	accessSequence.add(guildId);
	evictCacheIfNeeded(cache, accessSequence);
	if (shouldNotify) {
		notifyListeners(listeners, guildId, frozen);
	}
}

function evictCacheIfNeeded<T>(cache: Map<string, T>, accessSequence: Set<string>) {
	for (const guildId of accessSequence) {
		if (!cache.has(guildId)) {
			accessSequence.delete(guildId);
		}
	}
	while (cache.size > MAX_GUILD_EXPRESSION_TAB_CACHE_ENTRIES) {
		const guildId = accessSequence.values().next().value;
		if (guildId == null) {
			break;
		}
		cache.delete(guildId);
		accessSequence.delete(guildId);
	}
}

export function seedGuildEmojiCache(guildId: string, emojis: ReadonlyArray<GuildEmojiWithUser>): void {
	setCache(emojiCache, emojiAccessSequence, emojiListeners, guildId, emojis, false);
}

export function seedGuildStickerCache(guildId: string, stickers: ReadonlyArray<GuildStickerWithUser>): void {
	setCache(stickerCache, stickerAccessSequence, stickerListeners, guildId, stickers, false);
}

function subscribe<T>(
	listeners: Map<string, Set<(items: ReadonlyArray<T>) => void>>,
	guildId: string,
	listener: (items: ReadonlyArray<T>) => void,
): () => void {
	let listenersForGuild = listeners.get(guildId);
	if (!listenersForGuild) {
		listenersForGuild = new Set();
		listeners.set(guildId, listenersForGuild);
	}
	listenersForGuild.add(listener);
	return () => {
		listenersForGuild.delete(listener);
		if (listenersForGuild.size === 0) listeners.delete(guildId);
	};
}

function patchFromGateway<T extends {id: string; user?: UserPartial}>(
	cache: Map<string, ReadonlyArray<T & {user: UserPartial}>>,
	accessSequence: Set<string>,
	listeners: Map<string, Set<(items: ReadonlyArray<T & {user: UserPartial}>) => void>>,
	guildId: string,
	updates: ReadonlyArray<T>,
): void {
	const previous = cache.get(guildId) ?? [];
	const previousUserById = new Map(previous.map((item) => [item.id, item.user]));
	const next = updates
		.map((item) => {
			const user = item.user ?? previousUserById.get(item.id);
			return user ? {...item, user} : null;
		})
		.filter((entry): entry is T & {user: UserPartial} => Boolean(entry));
	setCache(cache, accessSequence, listeners, guildId, next, true);
}

export function subscribeToGuildEmojiUpdates(guildId: string, listener: EmojiUpdateListener): () => void {
	return subscribe(emojiListeners, guildId, listener);
}
export function subscribeToGuildStickerUpdates(guildId: string, listener: StickerUpdateListener): () => void {
	return subscribe(stickerListeners, guildId, listener);
}
export function patchGuildEmojiCacheFromGateway(guildId: string, updates: ReadonlyArray<GuildEmoji>): void {
	patchFromGateway(emojiCache, emojiAccessSequence, emojiListeners, guildId, updates);
}
export function patchGuildStickerCacheFromGateway(guildId: string, updates: ReadonlyArray<GuildSticker>): void {
	patchFromGateway(stickerCache, stickerAccessSequence, stickerListeners, guildId, updates);
}
