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

import {Config} from '~/Config';
import {Logger} from '~/Logger';
import type {VoiceRegionRecord} from './VoiceModel';
import {VoiceRepository} from './VoiceRepository';

export class VoiceDataInitializer {
	async initialize(): Promise<void> {
		if (!Config.voice.enabled || !Config.voice.autoCreateDummyData) {
			return;
		}

		try {
			const repository = new VoiceRepository();

			const existingRegions = await repository.listRegions();
			if (existingRegions.length > 0) {
				Logger.info(
					`[VoiceDataInitializer] Deleting ${existingRegions.length} existing voice regions to recreate with fresh data...`,
				);
				for (const region of existingRegions) {
					await repository.deleteRegion(region.id);
					Logger.info(`[VoiceDataInitializer] Deleted region: ${region.name} (${region.id})`);
				}
			}

			Logger.info('[VoiceDataInitializer] Creating dummy voice regions and servers...');

			const livekitApiKey = Config.voice.apiKey;
			const livekitApiSecret = Config.voice.apiSecret;

			if (!livekitApiKey || !livekitApiSecret) {
				Logger.warn('[VoiceDataInitializer] LiveKit API key/secret not configured, cannot create dummy servers');
				return;
			}

			await this.createDefaultRegions(repository, livekitApiKey, livekitApiSecret);

			Logger.info('[VoiceDataInitializer] Successfully created dummy voice regions and servers');
		} catch (error) {
			Logger.error({error}, '[VoiceDataInitializer] Failed to create dummy voice data');
		}
	}

	private async createDefaultRegions(
		repository: VoiceRepository,
		livekitApiKey: string,
		livekitApiSecret: string,
	): Promise<void> {
		const defaultRegions: Array<{
			region: VoiceRegionRecord;
		}> = [
			{
				region: {
					id: 'us-default',
					name: 'US Default',
					emoji: '🇺🇸',
					latitude: 39.8283,
					longitude: -98.5795,
					isDefault: true,
					restrictions: {
						vipOnly: false,
						requiredGuildFeatures: new Set(),
						allowedGuildIds: new Set(),
						allowedUserIds: new Set(),
					},
					createdAt: new Date(),
					updatedAt: new Date(),
				},
			},
			{
				region: {
					id: 'eu-default',
					name: 'EU Default',
					emoji: '🇪🇺',
					latitude: 50.0755,
					longitude: 14.4378,
					isDefault: false,
					restrictions: {
						vipOnly: false,
						requiredGuildFeatures: new Set(),
						allowedGuildIds: new Set(),
						allowedUserIds: new Set(),
					},
					createdAt: new Date(),
					updatedAt: new Date(),
				},
			},
			{
				region: {
					id: 'asia-default',
					name: 'Asia Default',
					emoji: '🌏',
					latitude: 35.6762,
					longitude: 139.6503,
					isDefault: false,
					restrictions: {
						vipOnly: false,
						requiredGuildFeatures: new Set(),
						allowedGuildIds: new Set(),
						allowedUserIds: new Set(),
					},
					createdAt: new Date(),
					updatedAt: new Date(),
				},
			},
		];

		const livekitEndpoint =
			Config.voice.url ||
			(() => {
				const protocol = new URL(Config.endpoints.apiPublic).protocol.slice(0, -1) === 'https' ? 'wss' : 'ws';
				return `${protocol}://${new URL(Config.endpoints.apiPublic).hostname}/livekit`;
			})();

		for (const {region} of defaultRegions) {
			await repository.createRegion(region);
			Logger.info(`[VoiceDataInitializer] Created region: ${region.name} (${region.id})`);

			const serverId = `${region.id}-server-1`;

			await repository.createServer({
				regionId: region.id,
				serverId,
				endpoint: livekitEndpoint,
				apiKey: livekitApiKey,
				apiSecret: livekitApiSecret,
				isActive: true,
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set(),
				},
			});

			Logger.info(`[VoiceDataInitializer] Created server: ${serverId} -> ${livekitEndpoint}`);
		}
	}
}
