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

import type {Redis} from 'ioredis';
import {Logger} from '~/Logger';
import {VOICE_CONFIGURATION_CHANNEL} from '~/voice/VoiceConstants';
import type {VoiceRegionMetadata, VoiceRegionRecord, VoiceServerRecord} from '~/voice/VoiceModel';
import type {VoiceRepository} from '~/voice/VoiceRepository';

type Subscriber = () => void;

export class VoiceTopology {
	private initialized = false;
	private reloadPromise: Promise<void> | null = null;
	private regions: Map<string, VoiceRegionRecord> = new Map();
	private serversByRegion: Map<string, Array<VoiceServerRecord>> = new Map();
	private defaultRegionId: string | null = null;
	private subscribers: Set<Subscriber> = new Set();
	private serverRotationIndex: Map<string, number> = new Map();

	constructor(
		private voiceRepository: VoiceRepository,
		private redisSubscriber: Redis | null,
	) {}

	async initialize(): Promise<void> {
		if (this.initialized) {
			return;
		}

		await this.reload();

		if (this.redisSubscriber) {
			try {
				await this.redisSubscriber.subscribe(VOICE_CONFIGURATION_CHANNEL);
				this.redisSubscriber.on('message', (channel, _message) => {
					if (channel === VOICE_CONFIGURATION_CHANNEL) {
						this.reload().catch((error) => {
							Logger.error({error}, 'Failed to reload voice topology from Redis notification');
						});
					}
				});
			} catch (error) {
				Logger.error({error}, 'Failed to subscribe to voice configuration channel');
			}
		}

		this.initialized = true;
	}

	getDefaultRegion(): VoiceRegionRecord | null {
		if (this.defaultRegionId === null) {
			return null;
		}
		return this.regions.get(this.defaultRegionId) ?? null;
	}

	getDefaultRegionId(): string | null {
		return this.defaultRegionId;
	}

	getRegion(regionId: string): VoiceRegionRecord | null {
		return this.regions.get(regionId) ?? null;
	}

	getAllRegions(): Array<VoiceRegionRecord> {
		return Array.from(this.regions.values());
	}

	getRegionMetadataList(): Array<VoiceRegionMetadata> {
		return this.getAllRegions().map((region) => ({
			id: region.id,
			name: region.name,
			emoji: region.emoji,
			latitude: region.latitude,
			longitude: region.longitude,
			isDefault: region.isDefault,
			vipOnly: region.restrictions.vipOnly,
			requiredGuildFeatures: Array.from(region.restrictions.requiredGuildFeatures),
		}));
	}

	getServersForRegion(regionId: string): Array<VoiceServerRecord> {
		return (this.serversByRegion.get(regionId) ?? []).slice();
	}

	getServer(regionId: string, serverId: string): VoiceServerRecord | null {
		const servers = this.serversByRegion.get(regionId);
		if (!servers) {
			return null;
		}
		return servers.find((server) => server.serverId === serverId) ?? null;
	}

	registerSubscriber(subscriber: Subscriber): void {
		this.subscribers.add(subscriber);
	}

	unregisterSubscriber(subscriber: Subscriber): void {
		this.subscribers.delete(subscriber);
	}

	getNextServer(regionId: string): VoiceServerRecord | null {
		const servers = this.serversByRegion.get(regionId);
		if (!servers || servers.length === 0) {
			return null;
		}

		const currentIndex = this.serverRotationIndex.get(regionId) ?? 0;
		const server = servers[currentIndex % servers.length];
		this.serverRotationIndex.set(regionId, (currentIndex + 1) % servers.length);
		return server;
	}

	private async reload(): Promise<void> {
		if (this.reloadPromise) {
			return this.reloadPromise;
		}

		this.reloadPromise = (async () => {
			const regionsWithServers = await this.voiceRepository.listRegionsWithServers();

			const newRegions: Map<string, VoiceRegionRecord> = new Map();
			const newServers: Map<string, Array<VoiceServerRecord>> = new Map();

			for (const region of regionsWithServers) {
				const sortedServers = region.servers.slice().sort((a, b) => a.serverId.localeCompare(b.serverId));

				const regionRecord: VoiceRegionRecord = {
					id: region.id,
					name: region.name,
					emoji: region.emoji,
					latitude: region.latitude,
					longitude: region.longitude,
					isDefault: region.isDefault,
					restrictions: {
						vipOnly: region.restrictions.vipOnly,
						requiredGuildFeatures: new Set(region.restrictions.requiredGuildFeatures),
						allowedGuildIds: new Set(region.restrictions.allowedGuildIds),
						allowedUserIds: new Set(region.restrictions.allowedUserIds),
					},
					createdAt: region.createdAt,
					updatedAt: region.updatedAt,
				};

				newRegions.set(region.id, regionRecord);

				newServers.set(
					region.id,
					sortedServers.map((server) => ({
						...server,
						restrictions: {
							vipOnly: server.restrictions.vipOnly,
							requiredGuildFeatures: new Set(server.restrictions.requiredGuildFeatures),
							allowedGuildIds: new Set(server.restrictions.allowedGuildIds),
							allowedUserIds: new Set(server.restrictions.allowedUserIds),
						},
					})),
				);
			}

			this.regions = newRegions;
			this.serversByRegion = newServers;

			this.recalculateServerRotation();
			this.recalculateDefaultRegion();
			this.notifySubscribers();
		})()
			.catch((error) => {
				Logger.error({error}, 'Failed to reload voice topology');
				throw error;
			})
			.finally(() => {
				this.reloadPromise = null;
			});

		return this.reloadPromise;
	}

	private recalculateServerRotation(): void {
		const newIndex = new Map<string, number>();
		for (const [regionId, servers] of this.serversByRegion.entries()) {
			if (servers.length === 0) {
				continue;
			}
			const previousIndex = this.serverRotationIndex.get(regionId) ?? 0;
			newIndex.set(regionId, previousIndex % servers.length);
		}
		this.serverRotationIndex = newIndex;
	}

	private recalculateDefaultRegion(): void {
		const regions = Array.from(this.regions.values());

		let defaultRegion: VoiceRegionRecord | null = null;

		for (const region of regions) {
			if (region.isDefault) {
				defaultRegion = region;
				break;
			}
		}

		if (!defaultRegion && regions.length > 0) {
			defaultRegion = regions[0];
		}

		this.defaultRegionId = defaultRegion ? defaultRegion.id : null;
	}

	private notifySubscribers(): void {
		for (const subscriber of this.subscribers) {
			try {
				subscriber();
			} catch (error) {
				Logger.error({error}, 'VoiceTopology subscriber threw an error');
			}
		}
	}
}
