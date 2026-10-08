// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import {
	THREAD_CHANNEL_TYPES,
	THREAD_FEATURE_CHANNEL_TYPES,
	userActive,
	viewerActive,
	viewerFromCtx,
} from '@app/api/experiment/ChannelThreadsGate';
import type {ReadStateMarker} from '@app/api/read_state/IReadStateRepository';
import {Channels} from '@app/api/Tables';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {ReadStateFlags} from '@fluxer/constants/src/ThreadConstants';
import type {Context} from 'hono';

export interface ReadStateChannelHint {
	type: number;
	guildId: GuildID | null;
}

const CHANNEL_META_CACHE_MAX_ENTRIES = 50_000;

const FETCH_CHANNEL_META_CQL = Channels.selectCql({
	columns: ['channel_id', 'guild_id', 'type'],
	where: [Channels.where.eq('channel_id'), Channels.where.eq('soft_deleted')],
	limit: 1,
});

const channelMetaCache = new Map<string, ReadStateChannelHint>();

function rememberChannelMeta(channelId: ChannelID, meta: ReadStateChannelHint): void {
	const key = channelId.toString();
	channelMetaCache.delete(key);
	channelMetaCache.set(key, meta);
	if (channelMetaCache.size > CHANNEL_META_CACHE_MAX_ENTRIES) {
		const oldest = channelMetaCache.keys().next().value;
		if (oldest !== undefined) channelMetaCache.delete(oldest);
	}
}

async function loadChannelMeta(channelId: ChannelID): Promise<ReadStateChannelHint | null> {
	const cached = channelMetaCache.get(channelId.toString());
	if (cached !== undefined) return cached;
	const row = await fetchOne<Pick<ChannelRow, 'channel_id' | 'guild_id' | 'type'>>(FETCH_CHANNEL_META_CQL, {
		channel_id: channelId,
		soft_deleted: false,
	});
	if (!row) return null;
	const meta = {type: row.type, guildId: row.guild_id ?? null};
	rememberChannelMeta(channelId, meta);
	return meta;
}

export function readStateCapable(ctx: Context<HonoEnv>): boolean {
	const viewer = viewerFromCtx(ctx);
	return viewer.kind === 'user' && viewer.capable;
}

export function readStateMarkerFor(channel: ReadStateChannelHint): ReadStateMarker | null {
	if (channel.guildId === null || !THREAD_FEATURE_CHANNEL_TYPES.has(channel.type)) return null;
	const flags =
		ReadStateFlags.IS_GUILD_CHANNEL | (THREAD_CHANNEL_TYPES.has(channel.type) ? ReadStateFlags.IS_THREAD : 0);
	return {flags, guildId: channel.guildId};
}

export async function resolveReadStateMarker(params: {
	userId: UserID;
	channelId: ChannelID;
	capable: boolean;
	channel?: ReadStateChannelHint | null;
}): Promise<ReadStateMarker | null> {
	const {userId, channelId, capable} = params;
	if (!capable || !userActive(userId)) return null;
	const channel = params.channel ?? (await loadChannelMeta(channelId));
	if (!channel) return null;
	const marker = readStateMarkerFor(channel);
	if (!marker) return null;
	return viewerActive({kind: 'user', userId, bot: false, capable}, marker.guildId) ? marker : null;
}

export function clearReadStateChannelMetaCacheForTesting(): void {
	channelMetaCache.clear();
}
