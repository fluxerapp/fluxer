// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import {
	ensureActiveGuildTainted,
	everEnabled,
	guildActive,
	isTainted,
	type ThreadViewer,
	userActive,
	userExcluded,
	userViewerActive,
	viewerActive,
} from '@app/api/experiment/ChannelThreadsGate';
import {
	CHANNEL_THREADS_CLIENT_FEATURE,
	type ProtectedBitActor,
	type ThreadPermissionMode,
} from '@app/api/utils/featureUtils';
import {THREAD_PERMISSIONS} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

export async function resolveThreadPermissionMode(guildId: GuildID): Promise<ThreadPermissionMode> {
	if (guildActive(guildId)) return 'active';
	return (await isTainted(guildId)) ? 'retired' : 'control';
}

export async function resolveProtectedBitActor(params: {
	guildId: GuildID;
	userId: UserID;
	clientFeatures: ReadonlySet<string>;
	viewer?: ThreadViewer;
	isBot: () => Promise<boolean>;
}): Promise<ProtectedBitActor> {
	const mode = await resolveThreadPermissionMode(params.guildId);
	if (mode === 'control') return {clientFeatures: params.clientFeatures};
	if (mode === 'active') await ensureActiveGuildTainted(params.guildId);
	return {
		clientFeatures: params.clientFeatures,
		threadBits: {mode, writer: mode === 'active' && (await threadBitWriter(params))},
	};
}

async function threadBitWriter(params: {
	userId: UserID;
	clientFeatures: ReadonlySet<string>;
	viewer?: ThreadViewer;
	isBot: () => Promise<boolean>;
}): Promise<boolean> {
	if (params.viewer) return userViewerActive(params.viewer);
	if (await params.isBot()) return !userExcluded(params.userId);
	return params.clientFeatures.has(CHANNEL_THREADS_CLIENT_FEATURE) && userActive(params.userId);
}

export function hasThreadPermissionBits(value: bigint | null | undefined): boolean {
	return value != null && (value & THREAD_PERMISSIONS) !== 0n;
}

export function stripThreadPermissionBits(value: bigint): bigint {
	return value & ~THREAD_PERMISSIONS;
}

export async function shouldMaskThreadPermissionBits(
	guildId: GuildID,
	viewer: ThreadViewer | undefined,
	values: Iterable<bigint | null | undefined>,
): Promise<boolean> {
	if (!viewer || viewer.kind === 'system') return false;
	let present = false;
	for (const value of values) {
		if (hasThreadPermissionBits(value)) {
			present = true;
			break;
		}
	}
	if (!present) return false;
	if (!guildActive(guildId) && !(await isTainted(guildId))) return false;
	return !viewerActive(viewer, guildId);
}

export async function maskChannelResponseThreadBits(
	guildId: GuildID | null | undefined,
	viewer: ThreadViewer | undefined,
	responses: Array<ChannelResponse>,
): Promise<Array<ChannelResponse>> {
	if (!guildId || !everEnabled()) return responses;
	const values = responses.flatMap((response) =>
		(response.permission_overwrites ?? []).flatMap((overwrite) => [BigInt(overwrite.allow), BigInt(overwrite.deny)]),
	);
	if (!(await shouldMaskThreadPermissionBits(guildId, viewer, values))) return responses;
	return responses.map((response) =>
		response.permission_overwrites
			? {
					...response,
					permission_overwrites: response.permission_overwrites.map((overwrite) => ({
						...overwrite,
						allow: stripThreadPermissionBits(BigInt(overwrite.allow)).toString(),
						deny: stripThreadPermissionBits(BigInt(overwrite.deny)).toString(),
					})),
				}
			: response,
	);
}

export async function maskGuildResponseThreadBits(
	guildId: GuildID,
	viewer: ThreadViewer,
	guild: GuildResponse,
): Promise<GuildResponse> {
	if (!everEnabled() || !guild.roles) return guild;
	const values = guild.roles.map((role) => BigInt(role.permissions));
	if (!(await shouldMaskThreadPermissionBits(guildId, viewer, values))) return guild;
	return {
		...guild,
		roles: guild.roles.map((role) => ({
			...role,
			permissions: stripThreadPermissionBits(BigInt(role.permissions)).toString(),
		})),
	};
}

export async function maskUserGuildsThreadBits(
	viewer: ThreadViewer,
	guilds: Array<GuildResponse>,
): Promise<Array<GuildResponse>> {
	if (!everEnabled()) return guilds;
	return Promise.all(
		guilds.map(async (guild) => {
			if (guild.permissions == null) return guild;
			const permissions = BigInt(guild.permissions);
			if (!(await shouldMaskThreadPermissionBits(createGuildID(BigInt(guild.id)), viewer, [permissions]))) return guild;
			return {...guild, permissions: stripThreadPermissionBits(permissions).toString()};
		}),
	);
}

export function flagThreadActiveUserGuilds(viewer: ThreadViewer, guilds: Array<GuildResponse>): Array<GuildResponse> {
	if (!userViewerActive(viewer)) return guilds;
	return guilds.map((guild) => (guildActive(guild.id) ? {...guild, threads_active: true} : guild));
}
