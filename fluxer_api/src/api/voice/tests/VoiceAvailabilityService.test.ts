// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {GatewayRpcClient} from '@app/api/infrastructure/GatewayRpcClient';
import {GatewayService} from '@app/api/infrastructure/GatewayService';
import type {GatewayVoiceStateCounts} from '@app/api/infrastructure/IGatewayService';
import {NatsGatewayRpcTransport} from '@app/api/infrastructure/NatsGatewayRpcTransport';
import {FakeNatsConnectionManager} from '@app/api/test/mocks/FakeNatsConnectionManager';
import type {VoiceAccessContext} from '@app/api/voice/VoiceAvailabilityService';
import {VoiceAvailabilityService} from '@app/api/voice/VoiceAvailabilityService';
import type {VoiceRegionRecord, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {VoiceServerLoadTracker} from '@app/api/voice/VoiceServerLoad';
import {VoiceTopology} from '@app/api/voice/VoiceTopology';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {afterEach, describe, expect, it} from 'vitest';

function createRegion(overrides: Partial<VoiceRegionRecord> = {}): VoiceRegionRecord {
	return {
		id: 'us-default',
		name: 'US Default',
		emoji: 'flag',
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
		...overrides,
	};
}

function createServer(overrides: Partial<VoiceServerRecord> = {}): VoiceServerRecord {
	return {
		regionId: 'us-default',
		serverId: 'server-1',
		endpoint: 'wss://voice.example.com',
		apiKey: 'test-key',
		apiSecret: 'test-secret',
		latitude: null,
		longitude: null,
		isActive: true,
		softConnectionLimit: null,
		notes: null,
		restrictions: {
			vipOnly: false,
			requiredGuildFeatures: new Set(),
			allowedGuildIds: new Set(),
			allowedUserIds: new Set(),
		},
		createdAt: new Date(),
		updatedAt: new Date(),
		...overrides,
	};
}

const ownedTopologies: Array<VoiceTopology> = [];
const ownedManagers: Array<FakeNatsConnectionManager> = [];

afterEach(async () => {
	const topologies = ownedTopologies.splice(0);
	const managers = ownedManagers.splice(0);
	const shutdowns = topologies.map(async (topology) => topology.shutdown());
	if (managers.length > 0) {
		shutdowns.push(GatewayRpcClient.resetForTests());
	}
	const outcomes = await Promise.allSettled(shutdowns);
	for (const outcome of outcomes) {
		if (outcome.status === 'rejected') {
			throw outcome.reason;
		}
	}
	for (const manager of managers) {
		expect(manager.drained).toBe(true);
	}
});

async function createTopology(
	regions: Array<VoiceRegionRecord>,
	serversByRegion: Map<string, Array<VoiceServerRecord>>,
): Promise<VoiceTopology> {
	const repository = new VoiceRepository();
	for (const region of regions) {
		await repository.upsertRegion(region);
	}
	for (const servers of serversByRegion.values()) {
		for (const server of servers) {
			await repository.upsertServer(server);
		}
	}
	const topology = new VoiceTopology(repository, null);
	ownedTopologies.push(topology);
	await topology.initialize();
	return topology;
}

async function createLoadSource(response: GatewayVoiceStateCounts): Promise<VoiceServerLoadTracker> {
	const manager = new FakeNatsConnectionManager(() => ({ok: true, result: response}));
	ownedManagers.push(manager);
	GatewayRpcClient.createForTests(new NatsGatewayRpcTransport(manager));
	const tracker = new VoiceServerLoadTracker({gatewayService: new GatewayService(), now: () => 1000});
	await tracker.refresh();
	expect(manager.requests).toStrictEqual([
		{subject: 'rpc.gateway.process.voice_state_counts', body: {}, timeout: 5000},
	]);
	return tracker;
}

describe('VoiceAvailabilityService', () => {
	let service: VoiceAvailabilityService;
	describe('region access through getAvailableRegions', () => {
		it('returns true for unrestricted region', async () => {
			const region = createRegion();
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: true, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns false when user is not in allowedUserIds', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set([createUserID(456n)]),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: false, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns true when user is in allowedUserIds', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set([createUserID(123n)]),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: true, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns false for vipOnly region without VIP_VOICE feature', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: true,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set(),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
				guildId: createGuildID(456n),
				guildFeatures: new Set(),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: false, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns true for vipOnly region with VIP_VOICE feature', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: true,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set(),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
				guildId: createGuildID(456n),
				guildFeatures: new Set([GuildFeatures.VIP_VOICE]),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: true, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns false for vipOnly region without guildId', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: true,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set(),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: false, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns true for guild in allowedGuildIds', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set([createGuildID(456n)]),
					allowedUserIds: new Set(),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
				guildId: createGuildID(456n),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: true, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns false for guild not in allowedGuildIds', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set([createGuildID(789n)]),
					allowedUserIds: new Set(),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
				guildId: createGuildID(456n),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: false, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns true for guild with required feature', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(['PREMIUM']),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set(),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
				guildId: createGuildID(456n),
				guildFeatures: new Set(['PREMIUM']),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: true, serverCount: 1, activeServerCount: 1}]);
		});
		it('returns false for guild without required feature', async () => {
			const region = createRegion({
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(['PREMIUM']),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set(),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [createServer()]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
				guildId: createGuildID(456n),
				guildFeatures: new Set(),
			};
			expect(
				service.getAvailableRegions(context).map(({id, isAccessible, serverCount, activeServerCount}) => ({
					id,
					isAccessible,
					serverCount,
					activeServerCount,
				})),
			).toStrictEqual([{id: 'us-default', isAccessible: false, serverCount: 1, activeServerCount: 1}]);
		});
	});
	describe('server access through getAccessibleServersForRegion', () => {
		it('returns false for inactive server', async () => {
			const region = createRegion();
			const server = createServer({isActive: false});
			const topology = await createTopology([region], new Map([['us-default', [server]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(service.getAccessibleServersForRegion('us-default', context).map(({serverId}) => serverId)).toStrictEqual(
				[],
			);
		});
		it('returns true for active unrestricted server', async () => {
			const region = createRegion();
			const server = createServer();
			const topology = await createTopology([region], new Map([['us-default', [server]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(service.getAccessibleServersForRegion('us-default', context).map(({serverId}) => serverId)).toStrictEqual([
				'server-1',
			]);
		});
		it('returns false when user is not in server allowedUserIds', async () => {
			const region = createRegion();
			const server = createServer({
				restrictions: {
					vipOnly: false,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set([createUserID(456n)]),
				},
			});
			const topology = await createTopology([region], new Map([['us-default', [server]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(service.getAccessibleServersForRegion('us-default', context).map(({serverId}) => serverId)).toStrictEqual(
				[],
			);
		});
	});
	describe('getAvailableRegions', () => {
		it('returns regions with accessibility status', async () => {
			const region1 = createRegion({id: 'us-default', isDefault: true});
			const region2 = createRegion({
				id: 'eu-vip',
				isDefault: false,
				restrictions: {
					vipOnly: true,
					requiredGuildFeatures: new Set(),
					allowedGuildIds: new Set(),
					allowedUserIds: new Set(),
				},
			});
			const server1 = createServer({regionId: 'us-default'});
			const topology = await createTopology(
				[region1, region2],
				new Map([
					['us-default', [server1]],
					['eu-vip', []],
				]),
			);
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			const regions = service.getAvailableRegions(context);
			expect(regions).toHaveLength(2);
			expect(regions.find(({id}) => id === 'us-default')?.isAccessible).toBe(true);
			expect(regions.find(({id}) => id === 'eu-vip')?.isAccessible).toBe(false);
		});
		it('marks region as not accessible if no servers are available', async () => {
			const region = createRegion();
			const topology = await createTopology([region], new Map([['us-default', []]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			const regions = service.getAvailableRegions(context);
			expect(regions).toHaveLength(1);
			expect(regions[0].isAccessible).toBe(false);
			expect(regions[0].serverCount).toBe(0);
		});
	});
	describe('selectServer', () => {
		it('returns server from specified region', async () => {
			const region = createRegion();
			const server = createServer();
			const topology = await createTopology([region], new Map([['us-default', [server]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			const selected = service.selectServer('us-default', context);
			expect(selected).not.toBeNull();
			expect(selected?.serverId).toBe('server-1');
		});
		it('returns null for region with no servers', async () => {
			const region = createRegion();
			const topology = await createTopology([region], new Map([['us-default', []]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			const selected = service.selectServer('us-default', context);
			expect(selected).toBeNull();
		});
		it('returns null for non-existent region', async () => {
			const topology = await createTopology([], new Map());
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			const selected = service.selectServer('non-existent', context);
			expect(selected).toBeNull();
		});
		it('rotates between accessible servers', async () => {
			const region = createRegion();
			const server1 = createServer({serverId: 'server-1'});
			const server2 = createServer({serverId: 'server-2'});
			const topology = await createTopology([region], new Map([['us-default', [server1, server2]]]));
			service = new VoiceAvailabilityService(topology);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			const first = service.selectServer('us-default', context);
			const second = service.selectServer('us-default', context);
			const third = service.selectServer('us-default', context);
			expect(first?.serverId).toBe('server-1');
			expect(second?.serverId).toBe('server-2');
			expect(third?.serverId).toBe('server-1');
		});
		it('rotates only between servers below their soft connection limit', async () => {
			const region = createRegion();
			const server1 = createServer({serverId: 'server-1', softConnectionLimit: 50});
			const server2 = createServer({serverId: 'server-2'});
			const topology = await createTopology([region], new Map([['us-default', [server1, server2]]]));
			service = new VoiceAvailabilityService(
				topology,
				await createLoadSource({
					total_voice_states: 50,
					regions: [],
					servers: [{server_id: 'server-1', voice_state_count: 50}],
				}),
			);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(service.selectServer('us-default', context)?.serverId).toBe('server-2');
			expect(service.selectServer('us-default', context)?.serverId).toBe('server-2');
		});
		it('rotates across every server when all of them are at their soft connection limit', async () => {
			const region = createRegion();
			const server1 = createServer({serverId: 'server-1', softConnectionLimit: 50});
			const server2 = createServer({serverId: 'server-2', softConnectionLimit: 50});
			const topology = await createTopology([region], new Map([['us-default', [server1, server2]]]));
			service = new VoiceAvailabilityService(
				topology,
				await createLoadSource({
					total_voice_states: 180,
					regions: [],
					servers: [
						{server_id: 'server-1', voice_state_count: 90},
						{server_id: 'server-2', voice_state_count: 90},
					],
				}),
			);
			const context: VoiceAccessContext = {
				requestingUserId: createUserID(123n),
			};
			expect(service.selectServer('us-default', context)?.serverId).toBe('server-1');
			expect(service.selectServer('us-default', context)?.serverId).toBe('server-2');
		});
	});
});
