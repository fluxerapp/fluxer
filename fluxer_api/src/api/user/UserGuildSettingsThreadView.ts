// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {ThreadOnlyChannelsByGuildRow} from '@app/api/database/types/ThreadTypes';
import {
	everEnabled,
	isTainted,
	type ThreadViewer,
	viewerActive,
	viewerFromCtx,
} from '@app/api/experiment/ChannelThreadsGate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import {ThreadOnlyChannelsByGuild} from '@app/api/Tables';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {mapUserGuildSettingsToResponse, type UserGuildSettingsThreadView} from '@app/api/user/UserMappers';
import type {UserGuildSettingsResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {Context} from 'hono';

const FETCH_THREAD_ONLY_CHANNEL_IDS_CQL = ThreadOnlyChannelsByGuild.selectCql({
	columns: ['channel_id'],
	where: ThreadOnlyChannelsByGuild.where.eq('guild_id'),
});

const DM_SETTINGS_GUILD_ID = createGuildID(0n);

export const SYSTEM_USER_GUILD_SETTINGS_VIEW: UserGuildSettingsThreadView = {flags: true};

export async function listThreadOnlyChannelIds(guildId: GuildID): Promise<Set<string>> {
	const rows = await fetchMany<Pick<ThreadOnlyChannelsByGuildRow, 'channel_id'>>(FETCH_THREAD_ONLY_CHANNEL_IDS_CQL, {
		guild_id: guildId,
	});
	return new Set(rows.map((row) => row.channel_id.toString()));
}

export async function controlUserGuildSettingsView(
	settings: UserGuildSettings,
): Promise<UserGuildSettingsThreadView | undefined> {
	if (settings.guildId === DM_SETTINGS_GUILD_ID || settings.channelOverrides.size === 0 || !everEnabled()) {
		return undefined;
	}
	if (!(await isTainted(settings.guildId))) return undefined;
	const forumIds = await listThreadOnlyChannelIds(settings.guildId);
	const hiddenChannelIds = new Set(
		Array.from(settings.channelOverrides.keys())
			.map((channelId) => channelId.toString())
			.filter((channelId) => forumIds.has(channelId)),
	);
	return hiddenChannelIds.size > 0 ? {flags: false, hiddenChannelIds} : undefined;
}

export async function resolveUserGuildSettingsView(
	settings: UserGuildSettings,
	viewer: ThreadViewer,
): Promise<UserGuildSettingsThreadView | undefined> {
	if (viewer.kind === 'system') return SYSTEM_USER_GUILD_SETTINGS_VIEW;
	if (settings.guildId !== DM_SETTINGS_GUILD_ID && viewerActive(viewer, settings.guildId)) {
		return SYSTEM_USER_GUILD_SETTINGS_VIEW;
	}
	return controlUserGuildSettingsView(settings);
}

export async function mapUserGuildSettingsForViewer(
	settings: UserGuildSettings,
	viewer: ThreadViewer,
): Promise<UserGuildSettingsResponse> {
	return mapUserGuildSettingsToResponse(settings, await resolveUserGuildSettingsView(settings, viewer));
}

export async function dispatchUserGuildSettingsUpdate(
	gatewayService: IGatewayService,
	userId: UserID,
	settings: UserGuildSettings,
): Promise<void> {
	const payload = mapUserGuildSettingsToResponse(settings, await controlUserGuildSettingsView(settings));
	if (payload.guild_id === null) {
		await gatewayService.dispatchPresence({userId, event: 'USER_GUILD_SETTINGS_UPDATE', data: payload});
		await gatewayService.syncPushUserGuildSettings({userId, guildId: settings.guildId, settings: payload});
		return;
	}
	const full = mapUserGuildSettingsToResponse(settings, SYSTEM_USER_GUILD_SETTINGS_VIEW);
	if (JSON.stringify(full) === JSON.stringify(payload)) {
		await gatewayService.dispatchPresence({userId, event: 'USER_GUILD_SETTINGS_UPDATE', data: payload});
	} else {
		await gatewayService.dispatchPresence({
			userId,
			event: 'USER_GUILD_SETTINGS_UPDATE',
			data: {...payload, __thread_unscoped: payload.guild_id},
		});
		await gatewayService.dispatchPresence({
			userId,
			event: 'USER_GUILD_SETTINGS_UPDATE',
			data: {...full, __thread_scoped: payload.guild_id},
		});
	}
	await gatewayService.syncPushUserGuildSettings({userId, guildId: settings.guildId, settings: full});
}

export function gateChannelOverrideFlags(raw: unknown, ctx: Context<HonoEnv>): unknown {
	const guildId = ctx.req.param('guild_id');
	return guildId !== undefined && viewerActive(viewerFromCtx(ctx), guildId) ? raw : stripChannelOverrideFlags(raw);
}

function stripChannelOverrideFlags(raw: unknown): unknown {
	if (!raw || typeof raw !== 'object' || Array.isArray(raw)) return raw;
	const overrides = (raw as {channel_overrides?: unknown}).channel_overrides;
	if (!overrides || typeof overrides !== 'object' || Array.isArray(overrides)) return raw;
	const entries = Object.entries(overrides);
	if (!entries.some(([, override]) => override !== null && typeof override === 'object' && 'flags' in override)) {
		return raw;
	}
	return {
		...raw,
		channel_overrides: Object.fromEntries(
			entries.map(([channelId, override]) => {
				if (override === null || typeof override !== 'object' || !('flags' in override)) return [channelId, override];
				const {flags: _flags, ...rest} = override as Record<string, unknown>;
				return [channelId, rest];
			}),
		),
	};
}
