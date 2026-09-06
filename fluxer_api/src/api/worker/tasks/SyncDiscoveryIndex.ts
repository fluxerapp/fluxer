// SPDX-License-Identifier: AGPL-3.0-or-later

import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {GuildID} from '../../BrandedTypes';
import {GuildDiscoveryRepository} from '../../guild/repositories/GuildDiscoveryRepository';
import {getGuildSearchService} from '../../SearchFactory';
import {mapWithConcurrency} from '../../utils/ConcurrencyUtils';
import {getWorkerDependencies} from '../WorkerContext';

const BATCH_SIZE = 200;
const UPDATE_CONCURRENCY = 25;
const syncDiscoveryIndex: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.info('Starting discovery index sync');
	const guildSearchService = getGuildSearchService();
	if (!guildSearchService) {
		helpers.logger.warn('Search service not available, skipping discovery index sync');
		return;
	}
	const {guildRepository, gatewayService} = getWorkerDependencies();
	const discoveryRepository = new GuildDiscoveryRepository();
	const approvedRows = await discoveryRepository.listByStatus(DiscoveryApplicationStatus.APPROVED);
	if (approvedRows.length === 0) {
		helpers.logger.info('No discoverable guilds to sync');
		return;
	}
	const guildIds = approvedRows.map((row) => row.guild_id);
	let freshCounts = new Map<
		GuildID,
		{
			memberCount: number;
			onlineCount: number;
		}
	>();
	try {
		freshCounts = await gatewayService.getDiscoveryGuildCounts(guildIds);
	} catch (error) {
		helpers.logger.warn(
			{error: error instanceof Error ? error.message : String(error)},
			'Failed to fetch fresh guild counts from gateway, using database values',
		);
	}
	let synced = 0;
	for (let i = 0; i < guildIds.length; i += BATCH_SIZE) {
		const batch = guildIds.slice(i, i + BATCH_SIZE);
		const [guilds, discoveryRows] = await Promise.all([
			guildRepository.listGuilds(batch),
			Promise.all(batch.map((guildId) => discoveryRepository.findByGuildId(guildId))),
		]);
		const guildMap = new Map(guilds.map((guild) => [guild.id.toString(), guild]));
		const updates = batch
			.map((guildId, index) => {
				const guild = guildMap.get(guildId.toString());
				if (!guild) return null;
				const discoveryRow = discoveryRows[index];
				if (!discoveryRow || discoveryRow.status !== DiscoveryApplicationStatus.APPROVED) return null;
				return {guild, discoveryRow, counts: freshCounts.get(guildId)};
			})
			.filter((update): update is NonNullable<typeof update> => update != null);
		await mapWithConcurrency(updates, UPDATE_CONCURRENCY, (update) =>
			guildSearchService.updateGuild(update.guild, {
				description: update.discoveryRow.description,
				categoryId: update.discoveryRow.category_type,
				primaryLanguage: update.discoveryRow.primary_language ?? null,
				tags: update.discoveryRow.custom_tags ?? [],
				memberCount: update.counts?.memberCount,
			}),
		);
		synced += updates.length;
	}
	helpers.logger.info({synced, total: guildIds.length}, 'Discovery index sync completed');
};

export default syncDiscoveryIndex;
