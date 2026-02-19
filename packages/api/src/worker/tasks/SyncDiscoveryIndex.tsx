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

import {GuildDiscoveryRepository} from '@fluxer/api/src/guild/repositories/GuildDiscoveryRepository';
import {getGuildSearchService} from '@fluxer/api/src/SearchFactory';
import {getWorkerDependencies} from '@fluxer/api/src/worker/WorkerContext';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import type {WorkerTaskHandler} from '@fluxer/worker/src/contracts/WorkerTask';

const BATCH_SIZE = 50;

const syncDiscoveryIndex: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.info('Starting discovery index sync');

	const guildSearchService = getGuildSearchService();
	if (!guildSearchService) {
		helpers.logger.warn('Search service not available, skipping discovery index sync');
		return;
	}

	const {guildRepository} = getWorkerDependencies();
	const discoveryRepository = new GuildDiscoveryRepository();

	const approvedRows = await discoveryRepository.listByStatus(DiscoveryApplicationStatus.APPROVED, 1000);
	if (approvedRows.length === 0) {
		helpers.logger.info('No discoverable guilds to sync');
		return;
	}

	const guildIds = approvedRows.map((row) => row.guild_id);

	let synced = 0;
	for (let i = 0; i < guildIds.length; i += BATCH_SIZE) {
		const batch = guildIds.slice(i, i + BATCH_SIZE);

		for (const guildId of batch) {
			const guild = await guildRepository.findUnique(guildId);
			if (!guild) continue;

			const discoveryRow = await discoveryRepository.findByGuildId(guildId);
			if (!discoveryRow) continue;

			await guildSearchService.updateGuild(guild, {
				description: discoveryRow.description,
				categoryId: discoveryRow.category_type,
			});

			synced++;
		}
	}

	helpers.logger.info({synced, total: guildIds.length}, 'Discovery index sync completed');
};

export default syncDiscoveryIndex;
