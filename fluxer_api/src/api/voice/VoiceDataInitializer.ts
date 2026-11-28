// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import type {VoiceRegionRecord} from '@app/api/voice/VoiceModel';
import {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {bundledVoiceServerId} from '@app/api/voice/VoiceServerEndpoints';

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
		const serverId = bundledVoiceServerId();
		if (serverId && (!livekitApiKey || !livekitApiSecret)) {
			Logger.warn('[VoiceDataInitializer] LiveKit API key/secret not configured, cannot create default region');
			return;
		}
		try {
			const repository = new VoiceRepository();
			const livekitEndpoint = resolveLivekitEndpoint(Config.voice.url, Config.endpoints.apiPublic);
			const existingRegions = await repository.listRegions();
			const configuredRegion = existingRegions.find((region) => region.id === defaultRegion.id);
			if (!serverId || !livekitApiKey || !livekitApiSecret) {
				if (existingRegions.length === 0) {
					await this.createDefaultRegion(repository, true);
					Logger.info(
						'[VoiceDataInitializer] No bundled voice server, add servers with rexctl voice join or in the admin panel',
					);
				} else if (configuredRegion) {
					await this.removeBundledServer(repository, defaultRegion.id, livekitApiKey);
				}
				return;
			}
			if (!configuredRegion) {
				await this.createDefaultRegion(repository, !existingRegions.some((region) => region.isDefault));
			}
			const configuredServer = await repository.getServer(defaultRegion.id, serverId);
			if (!configuredServer) {
				await repository.createServerIfAbsent({
					regionId: defaultRegion.id,
					serverId,
					endpoint: livekitEndpoint,
					apiKey: livekitApiKey,
					apiSecret: livekitApiSecret,
					latitude: null,
					longitude: null,
					isActive: true,
					softConnectionLimit: null,
					notes: null,
					internalEndpoint: null,
					draining: false,
					pinnedHostname: null,
					pinnedSoftLimit: false,
					publicIpv4: null,
					publicIpv6: null,
					restrictions: emptyRestrictions(),
				});
				Logger.info(`[VoiceDataInitializer] Registered bundled voice server ${serverId} -> ${livekitEndpoint}`);
				return;
			}
			const endpointChanged = configuredServer.endpoint !== livekitEndpoint;
			const apiKeyChanged = configuredServer.apiKey !== livekitApiKey;
			const apiSecretChanged = configuredServer.apiSecret !== livekitApiSecret;
			if (!endpointChanged && !apiKeyChanged && !apiSecretChanged) {
				Logger.info(`[VoiceDataInitializer] Default voice server ${serverId} already matches config credentials`);
				return;
			}
			if (endpointChanged && apiKeyChanged) {
				Logger.warn(
					`[VoiceDataInitializer] Server ${serverId} has another endpoint and API key than the bundled LiveKit, leaving it unchanged`,
				);
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

	private async createDefaultRegion(repository: VoiceRepository, isDefault: boolean): Promise<void> {
		const defaultRegion = Config.voice.defaultRegion!;
		await repository.createRegion({
			id: defaultRegion.id,
			name: defaultRegion.name,
			emoji: defaultRegion.emoji,
			latitude: defaultRegion.latitude,
			longitude: defaultRegion.longitude,
			isDefault,
			hidden: false,
			restrictions: emptyRestrictions(),
		});
		Logger.info(`[VoiceDataInitializer] Created region: ${defaultRegion.name} (${defaultRegion.id})`);
	}

	private async removeBundledServer(
		repository: VoiceRepository,
		regionId: string,
		bundledApiKey: string | undefined,
	): Promise<void> {
		const serverId = `${regionId}-server-1`;
		const server = await repository.getServer(regionId, serverId);
		if (!server || !bundledApiKey || server.apiKey !== bundledApiKey) {
			return;
		}
		await repository.deleteServer(regionId, serverId);
		Logger.info(`[VoiceDataInitializer] Removed ${serverId}, the bundled voice server is turned off`);
	}
}

function emptyRestrictions(): VoiceRegionRecord['restrictions'] {
	return {
		vipOnly: false,
		requiredGuildFeatures: new Set(),
		allowedGuildIds: new Set(),
		allowedUserIds: new Set(),
	};
}
