// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '../Config';
import {Logger} from '../Logger';
import {VoiceRepository} from './VoiceRepository';

export function resolveLivekitEndpoint(configuredUrl: string | undefined, apiPublicUrl: string): string {
	if (configuredUrl) {
		return configuredUrl;
	}
	const apiPublic = new URL(apiPublicUrl);
	const protocol = apiPublic.protocol === 'https:' ? 'wss' : 'ws';
	return `${protocol}://${apiPublic.host}/livekit`;
}

export class VoiceDataInitializer {
	async initialize(): Promise<void> {
		if (!Config.voice.enabled || !Config.voice.defaultRegion) {
			return;
		}
		const defaultRegion = Config.voice.defaultRegion;
		const livekitApiKey = Config.voice.apiKey;
		const livekitApiSecret = Config.voice.apiSecret;
		if (!livekitApiKey || !livekitApiSecret) {
			Logger.warn('[VoiceDataInitializer] LiveKit API key/secret not configured, cannot create default region');
			return;
		}
		try {
			const repository = new VoiceRepository();
			const serverId = `${defaultRegion.id}-server-1`;
			const livekitEndpoint = resolveLivekitEndpoint(Config.voice.url, Config.endpoints.apiPublic);
			const existingRegions = await repository.listRegions();
			if (existingRegions.length === 0) {
				Logger.info('[VoiceDataInitializer] Creating default voice region from config...');
				await repository.createRegion({
					id: defaultRegion.id,
					name: defaultRegion.name,
					emoji: defaultRegion.emoji,
					latitude: defaultRegion.latitude,
					longitude: defaultRegion.longitude,
					isDefault: true,
					restrictions: {
						vipOnly: false,
						requiredGuildFeatures: new Set(),
						allowedGuildIds: new Set(),
						allowedUserIds: new Set(),
					},
				});
				Logger.info(`[VoiceDataInitializer] Created region: ${defaultRegion.name} (${defaultRegion.id})`);
				await repository.createServer({
					regionId: defaultRegion.id,
					serverId,
					endpoint: livekitEndpoint,
					apiKey: livekitApiKey,
					apiSecret: livekitApiSecret,
					latitude: null,
					longitude: null,
					isActive: true,
					restrictions: {
						vipOnly: false,
						requiredGuildFeatures: new Set(),
						allowedGuildIds: new Set(),
						allowedUserIds: new Set(),
					},
				});
				Logger.info(`[VoiceDataInitializer] Created server: ${serverId} -> ${livekitEndpoint}`);
				Logger.info('[VoiceDataInitializer] Successfully created default voice region');
				return;
			}
			const configuredRegion = await repository.getRegion(defaultRegion.id);
			if (!configuredRegion) {
				Logger.info(
					`[VoiceDataInitializer] ${existingRegions.length} voice ${existingRegions.length === 1 ? 'region' : 'regions'} already exist and configured default region (${defaultRegion.id}) is absent, skipping config sync`,
				);
				return;
			}
			const configuredServer = await repository.getServer(defaultRegion.id, serverId);
			if (!configuredServer) {
				Logger.info(
					`[VoiceDataInitializer] Region ${defaultRegion.id} exists but server ${serverId} is absent, skipping config sync`,
				);
				return;
			}
			const endpointChanged = configuredServer.endpoint !== livekitEndpoint;
			const apiKeyChanged = configuredServer.apiKey !== livekitApiKey;
			const apiSecretChanged = configuredServer.apiSecret !== livekitApiSecret;
			if (!endpointChanged && !apiKeyChanged && !apiSecretChanged) {
				Logger.info(`[VoiceDataInitializer] Default voice server ${serverId} already matches config credentials`);
				return;
			}
			await repository.upsertServer({
				...configuredServer,
				endpoint: livekitEndpoint,
				apiKey: livekitApiKey,
				apiSecret: livekitApiSecret,
				updatedAt: new Date(),
			});
			Logger.info(`[VoiceDataInitializer] Synced default voice server ${serverId} credentials from config`);
		} catch (error) {
			Logger.error({error}, '[VoiceDataInitializer] Failed to initialise config-managed voice topology');
		}
	}
}
