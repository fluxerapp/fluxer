// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {executeConditional, fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {GuildThreadStateRow} from '@app/api/database/types/ThreadTypes';
import {GuildThreadState} from '@app/api/Tables';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {CHANNEL_THREADS_CLIENT_FEATURE} from '@app/api/utils/featureUtils';
import {
	type ChannelThreadsConfig,
	everyoneChannelThreadsConfig,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {Context} from 'hono';

export {
	THREAD_CHANNEL_TYPES,
	THREAD_FEATURE_CHANNEL_TYPES,
	THREAD_ONLY_CHANNEL_TYPES,
	THREAD_PARENT_CHANNEL_TYPES,
} from '@fluxer/constants/src/ThreadConstants';

export type ThreadViewer = {kind: 'user'; userId: UserID; bot: boolean; capable: boolean} | {kind: 'system'};

export const SYSTEM_THREAD_VIEWER: ThreadViewer = {kind: 'system'};

const TAINT_CACHE_TTL_MS = 60_000;
const TAINT_CACHE_MAX_ENTRIES = 10_000;

const FETCH_GUILD_THREAD_STATE_CQL = GuildThreadState.selectCql({
	columns: ['guild_id'],
	where: GuildThreadState.where.eq('guild_id'),
	limit: 1,
});

const taintCache = new Map<string, {tainted: boolean; checkedAt: number}>();

let currentConfig: ChannelThreadsConfig = everyoneChannelThreadsConfig(0);

export function syncChannelThreadsConfig(
	raw: string | null,
	parse: (raw: string | null) => ChannelThreadsConfig,
): ChannelThreadsConfig {
	currentConfig = everyoneChannelThreadsConfig(parse(raw).config_version);
	return currentConfig;
}

export function getChannelThreadsConfig(): ChannelThreadsConfig {
	return currentConfig;
}

export function userViewerActive(viewer: ThreadViewer): boolean {
	return viewer.kind === 'system' || viewer.bot || viewer.capable;
}

export function viewerActive(viewer: ThreadViewer, _guildId: GuildID | bigint | string): boolean {
	return userViewerActive(viewer);
}

export function viewerFromCtx(ctx: Context<HonoEnv>): ThreadViewer {
	const user = ctx.get('user');
	const bot = user.isBot;
	const capable =
		bot || (ctx.get('authTokenType') === 'session' && ctx.get('clientFeatures').has(CHANNEL_THREADS_CLIENT_FEATURE));
	return {kind: 'user', userId: user.id, bot, capable};
}

function rememberTaint(key: string, tainted: boolean): void {
	taintCache.delete(key);
	taintCache.set(key, {tainted, checkedAt: Date.now()});
	if (taintCache.size > TAINT_CACHE_MAX_ENTRIES) {
		const oldest = taintCache.keys().next().value;
		if (oldest !== undefined) taintCache.delete(oldest);
	}
}

export async function isTainted(guildId: GuildID, opts?: {fresh?: boolean}): Promise<boolean> {
	const key = guildId.toString();
	const cached = taintCache.get(key);
	if (
		cached !== undefined &&
		(cached.tainted || opts?.fresh !== true) &&
		Date.now() - cached.checkedAt < TAINT_CACHE_TTL_MS
	) {
		return cached.tainted;
	}
	const row = await fetchOne<Pick<GuildThreadStateRow, 'guild_id'>>(FETCH_GUILD_THREAD_STATE_CQL, {guild_id: guildId});
	const tainted = row !== null;
	rememberTaint(key, tainted);
	return tainted;
}

export function noteGuildThreadMarker(guildId: GuildID): void {
	rememberTaint(guildId.toString(), true);
}

export async function insertGuildThreadMarker(guildId: GuildID, permsSeededAt: Date | null): Promise<boolean> {
	const created = await executeConditional(
		GuildThreadState.insertIfNotExists({
			guild_id: guildId,
			first_active_at: new Date(),
			perms_seeded_at: permsSeededAt,
			search_backfilled_at: null,
		}),
	);
	noteGuildThreadMarker(guildId);
	return created;
}

export async function ensureActiveGuildTainted(guildId: GuildID): Promise<void> {
	if (await isTainted(guildId)) return;
	await insertGuildThreadMarker(guildId, null);
}

export function clearChannelThreadsTaintCacheForTesting(): void {
	taintCache.clear();
}
