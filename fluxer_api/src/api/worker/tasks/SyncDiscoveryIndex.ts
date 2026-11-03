// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID} from '@app/api/BrandedTypes';
import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import {getGuildSearchService} from '@app/api/SearchFactory';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const SCAN_PAGE_SIZE = 200;
const UPDATE_CONCURRENCY = 25;

async function listApprovedGuildIds(discoveryRepository: GuildDiscoveryRepository): Promise<{
	approved: Array<GuildID>;
	restored: number;
}> {
	const indexed = new Set(
		(await discoveryRepository.listByStatus(DiscoveryApplicationStatus.APPROVED)).map((row) => row.guild_id.toString()),
	);
	const approved: Array<GuildID> = [];
	let restored = 0;
	let pageState: string | null = null;
	do {
		const page = await discoveryRepository.scanStatuses(pageState, SCAN_PAGE_SIZE);
		for (const row of page.rows) {
			if (row.status !== DiscoveryApplicationStatus.APPROVED) continue;
			approved.push(row.guild_id);
			if (indexed.has(row.guild_id.toString())) continue;
			const current = await discoveryRepository.findByGuildId(row.guild_id);
			if (current?.status !== DiscoveryApplicationStatus.APPROVED) continue;
			await discoveryRepository.addStatusEntry({
				status: current.status,
				applied_at: current.applied_at,
				guild_id: current.guild_id,
			});
			restored++;
		}
		pageState = page.pageState;
	} while (pageState);
	return {approved, restored};
}

const syncDiscoveryIndex: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.info('Starting discovery index sync');
	const guildSearchService = getGuildSearchService();
	if (!guildSearchService) {
		helpers.logger.warn('Search service not available, skipping discovery index sync');
		return;
	}
	const {guildRepository} = getWorkerDependencies();
	const discoveryRepository = new GuildDiscoveryRepository();
	const {approved: guildIds, restored} = await listApprovedGuildIds(discoveryRepository);
	if (restored > 0) {
		helpers.logger.warn({restored}, 'Restored approved guilds missing from the discovery status index');
	}
	if (guildIds.length === 0) {
		helpers.logger.info('No discoverable guilds to sync');
		return;
	}
	let synced = 0;
	for (let i = 0; i < guildIds.length; i += SCAN_PAGE_SIZE) {
		const batch = guildIds.slice(i, i + SCAN_PAGE_SIZE);
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
				return {guild, discoveryRow};
			})
			.filter((update): update is NonNullable<typeof update> => update != null);
		await mapWithConcurrency(updates, UPDATE_CONCURRENCY, (update) =>
			guildSearchService.updateGuild(update.guild, {
				description: update.discoveryRow.description,
				categoryId: update.discoveryRow.category_type,
				primaryLanguage: update.discoveryRow.primary_language ?? null,
				tags: update.discoveryRow.custom_tags ?? [],
			}),
		);
		synced += updates.length;
	}
	helpers.logger.info({synced, total: guildIds.length, restored}, 'Discovery index sync completed');
};

export default syncDiscoveryIndex;
