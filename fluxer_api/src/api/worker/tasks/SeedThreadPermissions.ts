// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, type GuildID, guildIdToRoleId} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {IThreadRepository} from '@app/api/channel/repositories/IThreadRepository';
import {withThreadParentFieldsMany} from '@app/api/channel/services/thread/ThreadParentSettings';
import {getCompiledChannelThreadsConfig, guildActive} from '@app/api/experiment/ChannelThreadsGate';
import {mapGuildRoleToResponse} from '@app/api/guild/GuildModel';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {Channel} from '@app/api/models/Channel';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DEFAULT_THREAD_PERMISSIONS, THREAD_PERMISSIONS} from '@fluxer/constants/src/ThreadPermissionUtils';
import {
	type ChannelThreadsConfig,
	channelThreadsGuildActive,
	compileChannelThreadsConfig,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	guildId: z.string(),
	configVersion: z.number().int().min(0).optional(),
});

const SEEDED_GUILD_CACHE_MAX_ENTRIES = 10_000;
const SEED_MARKER_READ_CHUNK = 100;
const seededGuildIds = new Set<string>();

export function seedThreadOverwriteBits(value: bigint): bigint {
	return value & ~THREAD_PERMISSIONS;
}

export function seedThreadOverwriteValue(value: bigint): bigint {
	const cleared = seedThreadOverwriteBits(value);
	return (value & Permissions.SEND_MESSAGES) !== 0n ? cleared | DEFAULT_THREAD_PERMISSIONS : cleared;
}

export function enabledThreadGuildIds(config: ChannelThreadsConfig): Array<string> {
	const compiled = compileChannelThreadsConfig(config);
	return config.enabled_guild_ids.filter((id) => channelThreadsGuildActive(compiled, id));
}

export function newlyEnabledThreadGuildIds(
	previous: ChannelThreadsConfig,
	landed: ChannelThreadsConfig,
): Array<string> {
	const before = new Set(enabledThreadGuildIds(previous));
	return enabledThreadGuildIds(landed).filter((id) => !before.has(id));
}

export async function enqueueThreadPermissionSeeds(
	threads: IThreadRepository,
	config: ChannelThreadsConfig,
): Promise<void> {
	const guildIds = enabledThreadGuildIds(config);
	for (let index = 0; index < guildIds.length; index += SEED_MARKER_READ_CHUNK) {
		const chunk = guildIds.slice(index, index + SEED_MARKER_READ_CHUNK);
		const markers = await Promise.all(chunk.map((id) => threads.getGuildMarker(createGuildID(BigInt(id)))));
		for (const [position, guildId] of chunk.entries()) {
			if (!markers[position]?.perms_seeded_at) await enqueueThreadPermissionSeed(guildId);
		}
	}
}

async function enqueueThreadPermissionSeed(guildId: string): Promise<void> {
	await getWorkerService().addJob(
		'seedThreadPermissions',
		{guildId, configVersion: getCompiledChannelThreadsConfig().config.config_version},
		{jobKey: `seed-thread-permissions-${guildId}`},
	);
}

export async function ensureGuildThreadPermissionsSeeded(threads: IThreadRepository, guildId: GuildID): Promise<void> {
	const key = guildId.toString();
	if (seededGuildIds.has(key)) return;
	const marker = await threads.getGuildMarker(guildId);
	if (marker?.perms_seeded_at) {
		if (seededGuildIds.size >= SEEDED_GUILD_CACHE_MAX_ENTRIES) seededGuildIds.clear();
		seededGuildIds.add(key);
		return;
	}
	if (!marker) await threads.ensureGuildMarker(guildId);
	await enqueueThreadPermissionSeed(key);
}

export async function ensureChannelThreadsConfigVersion(requiredVersion: number | undefined): Promise<void> {
	if (requiredVersion === undefined || getCompiledChannelThreadsConfig().config.config_version >= requiredVersion)
		return;
	const refreshed = await getWorkerDependencies().instanceConfigRepository.refreshChannelThreadsConfig();
	if (refreshed.config.config_version < requiredVersion) {
		throw new Error(`Channel threads config ${refreshed.config.config_version} is older than ${requiredVersion}`);
	}
}

const seedThreadPermissions: WorkerTaskHandler = async (payload) => {
	const validated = PayloadSchema.parse(payload);
	const guildId = createGuildID(BigInt(validated.guildId));
	const {channelRepository, guildRepository, gatewayService, userCacheService} = getWorkerDependencies();
	await ensureChannelThreadsConfigVersion(validated.configVersion);
	if (!guildActive(guildId)) return;
	const marker = await channelRepository.threads.getGuildMarker(guildId);
	if (marker?.perms_seeded_at) return;
	const guild = await guildRepository.findUnique(guildId);
	if (!guild) return;
	await channelRepository.threads.ensureGuildMarker(guildId);
	const channels = await channelRepository.listGuildChannels(guildId, 'complete');
	const updatedChannels: Array<Channel> = [];
	for (const channel of channels) {
		if (channel.permissionOverwrites.size === 0) continue;
		let changed = false;
		const overwrites = new Map(
			Array.from(channel.permissionOverwrites.entries()).map(([targetId, overwrite]) => {
				const allow = seedThreadOverwriteValue(overwrite.allow);
				const deny = seedThreadOverwriteValue(overwrite.deny);
				if (allow !== overwrite.allow || deny !== overwrite.deny) changed = true;
				return [targetId, {type: overwrite.type, allow_: allow, deny_: deny}];
			}),
		);
		if (!changed) continue;
		updatedChannels.push(
			await channelRepository.channelData.upsert(
				{...channel.toRow(), permission_overwrites: overwrites},
				channel.toRow(),
			),
		);
	}
	const everyone = await guildRepository.getRole(guildIdToRoleId(guildId), guildId);
	const everyoneSeeded =
		everyone && (everyone.permissions & Permissions.SEND_MESSAGES) !== 0n
			? everyone.permissions | DEFAULT_THREAD_PERMISSIONS
			: null;
	const updatedRole =
		everyone && everyoneSeeded !== null && everyoneSeeded !== everyone.permissions
			? await guildRepository.upsertRole({...everyone.toRow(), permissions: everyoneSeeded})
			: null;
	if (updatedRole) {
		await gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_ROLE_UPDATE_BULK',
			data: {roles: [mapGuildRoleToResponse(updatedRole)]},
		});
	}
	if (updatedChannels.length > 0) {
		const requestCache = createRequestCache();
		const channelResponses = await Promise.all(
			updatedChannels.map((channel) =>
				mapChannelToResponse({channel, currentUserId: null, userCacheService, requestCache}),
			),
		);
		await gatewayService.dispatchGuild({
			guildId,
			event: 'CHANNEL_UPDATE_BULK',
			data: {
				channels: await withThreadParentFieldsMany(
					channelRepository.threads,
					guildId,
					updatedChannels,
					channelResponses,
				),
			},
		});
	}
	await channelRepository.threads.markGuildPermsSeeded(guildId, new Date());
	Logger.info(
		{guildId: guildId.toString(), channels: updatedChannels.length, everyoneUpdated: updatedRole !== null},
		'Seeded thread permissions',
	);
};

export default seedThreadPermissions;
