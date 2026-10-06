// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {mapGuildToGuildResponse} from '@app/api/guild/GuildModel';
import type {IGuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import type {Guild} from '@app/api/models/Guild';
import {getGuildSearchService} from '@app/api/SearchFactory';
import type {GuildDiscoveryContext} from '@app/api/search/guild/GuildSearchSerializer';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';

interface AdminGuildUpdatePropagatorDeps {
	gatewayService: IGatewayService;
	discoveryRepository: IGuildDiscoveryRepository;
}

interface AdminGuildUpdateDispatchOptions {
	adminUserId?: UserID;
	reconcileDiscoveryFeature?: boolean;
}

export class AdminGuildUpdatePropagator {
	constructor(private readonly deps: AdminGuildUpdatePropagatorDeps) {}

	async dispatchGuildUpdate(
		guildId: GuildID,
		updatedGuild: Guild,
		options: AdminGuildUpdateDispatchOptions = {},
	): Promise<void> {
		const {gatewayService, discoveryRepository} = this.deps;
		await gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_UPDATE',
			data: mapGuildToGuildResponse(updatedGuild),
		});
		let discoveryRow =
			options.reconcileDiscoveryFeature || updatedGuild.features.has(GuildFeatures.DISCOVERABLE)
				? await discoveryRepository.findByGuildId(guildId).catch(() => null)
				: null;
		if (options.reconcileDiscoveryFeature && discoveryRow) {
			const hasDiscoverable = updatedGuild.features.has(GuildFeatures.DISCOVERABLE);
			const shouldApprove = hasDiscoverable && discoveryRow.status !== DiscoveryApplicationStatus.APPROVED;
			const shouldRemove = !hasDiscoverable && discoveryRow.status === DiscoveryApplicationStatus.APPROVED;
			if (shouldApprove || shouldRemove) {
				const now = new Date();
				const updatedRow = shouldApprove
					? {
							...discoveryRow,
							status: DiscoveryApplicationStatus.APPROVED,
							reviewed_at: discoveryRow.reviewed_at ?? now,
							reviewed_by: discoveryRow.reviewed_by ?? options.adminUserId ?? null,
							review_reason: discoveryRow.review_reason ?? 'DISCOVERABLE feature enabled by admin',
							removed_at: null,
							removed_by: null,
							removal_reason: null,
						}
					: {
							...discoveryRow,
							status: DiscoveryApplicationStatus.REMOVED,
							removed_at: now,
							removed_by: options.adminUserId ?? null,
							removal_reason: 'Guild no longer has DISCOVERABLE feature',
						};
				await discoveryRepository.updateStatus(guildId, discoveryRow.status, discoveryRow.applied_at, updatedRow);
				discoveryRow = updatedRow;
			}
		}
		const guildSearchService = getGuildSearchService();
		if (guildSearchService) {
			let discoveryContext: GuildDiscoveryContext | undefined;
			if (updatedGuild.features.has(GuildFeatures.DISCOVERABLE)) {
				if (discoveryRow?.status === DiscoveryApplicationStatus.APPROVED) {
					discoveryContext = {
						description: discoveryRow.description,
						categoryId: discoveryRow.category_type,
						primaryLanguage: discoveryRow.primary_language ?? null,
						tags: discoveryRow.custom_tags ?? [],
					};
				}
			}
			await guildSearchService.updateGuild(updatedGuild, discoveryContext).catch((error) => {
				Logger.error({guildId, error}, 'Failed to update guild in search after admin update');
			});
		}
	}
}
