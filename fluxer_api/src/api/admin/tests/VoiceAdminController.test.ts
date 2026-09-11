// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	CreateVoiceRegionResponse,
	CreateVoiceServerResponse,
	DeleteVoiceResponse,
	GetVoiceRegionResponse,
	GetVoiceServerResponse,
	ListVoiceRegionsResponse,
	ListVoiceServersResponse,
	UpdateVoiceRegionResponse,
	UpdateVoiceServerResponse,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {VoiceRepository} from '../../voice/VoiceRepository';

interface VoiceFixture {
	regionId: string;
	serverId: string;
	initialApiKey: string;
	initialApiSecret: string;
}

async function createAdminWithAcls(harness: ApiTestHarness, acls: Array<string>): Promise<TestAccount> {
	const account = await createTestAccount(harness);
	return await setUserACLs(harness, account, [AdminACLs.AUTHENTICATE, ...acls]);
}

async function createVoiceFixture(
	harness: ApiTestHarness,
	admin: TestAccount,
	params: {
		regionId: string;
		serverId: string;
		endpoint: string;
		apiKey: string;
		apiSecret: string;
	},
): Promise<VoiceFixture> {
	await createBuilder<CreateVoiceRegionResponse>(harness, `${admin.token}`)
		.post('/admin/voice/regions')
		.body({
			id: params.regionId,
			name: `Region ${params.regionId}`,
			emoji: ':earth_americas:',
			latitude: 1,
			longitude: 2,
		})
		.expect(HTTP_STATUS.OK)
		.execute();
	await createBuilder<CreateVoiceServerResponse>(harness, `${admin.token}`)
		.post(`/admin/voice/regions/${params.regionId}/servers`)
		.body({
			server_id: params.serverId,
			endpoint: params.endpoint,
			api_key: params.apiKey,
			api_secret: params.apiSecret,
		})
		.expect(HTTP_STATUS.OK)
		.execute();
	return {
		regionId: params.regionId,
		serverId: params.serverId,
		initialApiKey: params.apiKey,
		initialApiSecret: params.apiSecret,
	};
}

describe('VoiceAdminController', () => {
	let harness: ApiTestHarness;
	let voiceRepository: VoiceRepository;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		voiceRepository = new VoiceRepository();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('returns servers when listing voice regions with include_servers enabled', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_REGION_LIST,
			AdminACLs.VOICE_SERVER_CREATE,
		]);
		const fixture = await createVoiceFixture(harness, admin, {
			regionId: 'voice-region-list-with-servers',
			serverId: 'voice-server-list-with-servers',
			endpoint: 'https://voice-list.example.com/socket',
			apiKey: 'list-api-key',
			apiSecret: 'list-api-secret',
		});
		const result = await createBuilder<ListVoiceRegionsResponse>(harness, `${admin.token}`)
			.get('/admin/voice/regions?include_servers=true')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.regions).toHaveLength(1);
		expect(result.regions[0]?.id).toBe(fixture.regionId);
		expect(result.regions[0]?.servers).toBeDefined();
		expect(result.regions[0]?.servers).toHaveLength(1);
		expect(result.regions[0]?.servers?.[0]?.server_id).toBe(fixture.serverId);
	});
	test('resolves and removes regions and servers through the nested routes', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_REGION_DELETE,
			AdminACLs.VOICE_REGION_LIST,
			AdminACLs.VOICE_REGION_UPDATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_DELETE,
			AdminACLs.VOICE_SERVER_LIST,
		]);
		const fixture = await createVoiceFixture(harness, admin, {
			regionId: 'voice-region-nested-routes',
			serverId: 'voice-server-nested-routes',
			endpoint: 'https://voice-nested.example.com/socket',
			apiKey: 'nested-api-key',
			apiSecret: 'nested-api-secret',
		});
		const region = await createBuilder<GetVoiceRegionResponse>(harness, `${admin.token}`)
			.get(`/admin/voice/regions/${fixture.regionId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(region.region?.id).toBe(fixture.regionId);
		expect(region.region?.servers).toHaveLength(1);
		const servers = await createBuilder<ListVoiceServersResponse>(harness, `${admin.token}`)
			.get(`/admin/voice/regions/${fixture.regionId}/servers`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(servers.servers).toHaveLength(1);
		expect(servers.servers[0]?.server_id).toBe(fixture.serverId);
		const server = await createBuilder<GetVoiceServerResponse>(harness, `${admin.token}`)
			.get(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(server.server?.endpoint).toBe('https://voice-nested.example.com/socket');
		const renamed = await createBuilder<UpdateVoiceRegionResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}`)
			.body({name: 'Renamed region'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(renamed.region.id).toBe(fixture.regionId);
		expect(renamed.region.name).toBe('Renamed region');
		const deletedServer = await createBuilder<DeleteVoiceResponse>(harness, `${admin.token}`)
			.delete(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(deletedServer.success).toBe(true);
		expect(await voiceRepository.getServer(fixture.regionId, fixture.serverId)).toBeNull();
		const deletedRegion = await createBuilder<DeleteVoiceResponse>(harness, `${admin.token}`)
			.delete(`/admin/voice/regions/${fixture.regionId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(deletedRegion.success).toBe(true);
		expect(await voiceRepository.getRegion(fixture.regionId)).toBeNull();
	});
	test('rejects voice server creation when no region carries the identifier', async () => {
		const admin = await createAdminWithAcls(harness, [AdminACLs.VOICE_SERVER_CREATE]);
		const regionId = 'voice-region-missing-for-server-create';
		const serverId = 'voice-server-missing-region';
		await createBuilder(harness, `${admin.token}`)
			.post(`/admin/voice/regions/${regionId}/servers`)
			.body({
				server_id: serverId,
				endpoint: 'https://voice-orphan.example.com/socket',
				api_key: 'orphan-api-key',
				api_secret: 'orphan-api-secret',
			})
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_VOICE_REGION)
			.execute();
		expect(await voiceRepository.getServer(regionId, serverId)).toBeNull();
		expect(await voiceRepository.listServers(regionId)).toHaveLength(0);
	});
	test('updates voice server credentials when api key and secret are provided', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_UPDATE,
		]);
		const fixture = await createVoiceFixture(harness, admin, {
			regionId: 'voice-region-credentials-update',
			serverId: 'voice-server-credentials-update',
			endpoint: 'https://voice-original.example.com/socket',
			apiKey: 'original-api-key',
			apiSecret: 'original-api-secret',
		});
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.body({
				endpoint: 'https://voice-updated.example.com/socket',
				api_key: 'updated-api-key',
				api_secret: 'updated-api-secret',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const persisted = await voiceRepository.getServer(fixture.regionId, fixture.serverId);
		expect(persisted).not.toBeNull();
		expect(persisted?.endpoint).toBe('https://voice-updated.example.com/socket');
		expect(persisted?.apiKey).toBe('updated-api-key');
		expect(persisted?.apiSecret).toBe('updated-api-secret');
	});
	test('updates api key and api secret independently when only one field is provided', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_UPDATE,
		]);
		const fixture = await createVoiceFixture(harness, admin, {
			regionId: 'voice-region-credentials-partial-update',
			serverId: 'voice-server-credentials-partial-update',
			endpoint: 'https://voice-partial-before.example.com/socket',
			apiKey: 'partial-before-api-key',
			apiSecret: 'partial-before-api-secret',
		});
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.body({
				api_key: 'partial-after-api-key',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const afterApiKeyUpdate = await voiceRepository.getServer(fixture.regionId, fixture.serverId);
		expect(afterApiKeyUpdate).not.toBeNull();
		expect(afterApiKeyUpdate?.apiKey).toBe('partial-after-api-key');
		expect(afterApiKeyUpdate?.apiSecret).toBe(fixture.initialApiSecret);
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.body({
				api_secret: 'partial-after-api-secret',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const afterApiSecretUpdate = await voiceRepository.getServer(fixture.regionId, fixture.serverId);
		expect(afterApiSecretUpdate).not.toBeNull();
		expect(afterApiSecretUpdate?.apiKey).toBe('partial-after-api-key');
		expect(afterApiSecretUpdate?.apiSecret).toBe('partial-after-api-secret');
	});
	test('keeps voice server credentials unchanged when api key and secret are omitted', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_UPDATE,
		]);
		const fixture = await createVoiceFixture(harness, admin, {
			regionId: 'voice-region-credentials-unchanged',
			serverId: 'voice-server-credentials-unchanged',
			endpoint: 'https://voice-before.example.com/socket',
			apiKey: 'before-api-key',
			apiSecret: 'before-api-secret',
		});
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.body({
				endpoint: 'https://voice-after.example.com/socket',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const persisted = await voiceRepository.getServer(fixture.regionId, fixture.serverId);
		expect(persisted).not.toBeNull();
		expect(persisted?.endpoint).toBe('https://voice-after.example.com/socket');
		expect(persisted?.apiKey).toBe(fixture.initialApiKey);
		expect(persisted?.apiSecret).toBe(fixture.initialApiSecret);
	});
	test('stores, keeps, and clears a voice server soft connection limit', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_LIST,
			AdminACLs.VOICE_SERVER_UPDATE,
		]);
		const regionId = 'voice-region-soft-limit';
		const serverId = 'voice-server-soft-limit';
		await createBuilder<CreateVoiceRegionResponse>(harness, `${admin.token}`)
			.post('/admin/voice/regions')
			.body({
				id: regionId,
				name: `Region ${regionId}`,
				emoji: ':earth_americas:',
				latitude: 1,
				longitude: 2,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const created = await createBuilder<CreateVoiceServerResponse>(harness, `${admin.token}`)
			.post(`/admin/voice/regions/${regionId}/servers`)
			.body({
				server_id: serverId,
				endpoint: 'https://voice-soft-limit.example.com/socket',
				api_key: 'soft-limit-api-key',
				api_secret: 'soft-limit-api-secret',
				soft_connection_limit: 250,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(created.server.soft_connection_limit).toBe(250);
		expect((await voiceRepository.getServer(regionId, serverId))?.softConnectionLimit).toBe(250);
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${regionId}/servers/${serverId}`)
			.body({endpoint: 'https://voice-soft-limit-2.example.com/socket'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect((await voiceRepository.getServer(regionId, serverId))?.softConnectionLimit).toBe(250);
		const cleared = await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${regionId}/servers/${serverId}`)
			.body({soft_connection_limit: null})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(cleared.server.soft_connection_limit).toBeNull();
		expect((await voiceRepository.getServer(regionId, serverId))?.softConnectionLimit).toBeNull();
	});
	test('clears voice server restriction lists when empty arrays are supplied', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_UPDATE,
		]);
		const regionId = 'voice-region-clear-restrictions';
		const serverId = 'voice-server-clear-restrictions';
		await createBuilder<CreateVoiceRegionResponse>(harness, `${admin.token}`)
			.post('/admin/voice/regions')
			.body({
				id: regionId,
				name: `Region ${regionId}`,
				emoji: ':earth_americas:',
				latitude: 1,
				longitude: 2,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder<CreateVoiceServerResponse>(harness, `${admin.token}`)
			.post(`/admin/voice/regions/${regionId}/servers`)
			.body({
				server_id: serverId,
				endpoint: 'https://voice-clear.example.com/socket',
				api_key: 'clear-api-key',
				api_secret: 'clear-api-secret',
				required_guild_features: ['VIP_VOICE'],
				allowed_guild_ids: [1234567890123456789n.toString()],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const stored = await voiceRepository.getServer(regionId, serverId);
		expect(Array.from(stored?.restrictions.requiredGuildFeatures ?? [])).toEqual(['VIP_VOICE']);
		expect(stored?.restrictions.allowedGuildIds.size).toBe(1);
		const cleared = await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${regionId}/servers/${serverId}`)
			.body({required_guild_features: [], allowed_guild_ids: []})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(cleared.server.required_guild_features).toEqual([]);
		expect(cleared.server.allowed_guild_ids).toEqual([]);
		const persisted = await voiceRepository.getServer(regionId, serverId);
		expect(persisted?.restrictions.requiredGuildFeatures.size).toBe(0);
		expect(persisted?.restrictions.allowedGuildIds.size).toBe(0);
	});
	test('leaves voice server restriction lists unchanged when they are omitted', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_UPDATE,
		]);
		const regionId = 'voice-region-keep-restrictions';
		const serverId = 'voice-server-keep-restrictions';
		await createBuilder<CreateVoiceRegionResponse>(harness, `${admin.token}`)
			.post('/admin/voice/regions')
			.body({
				id: regionId,
				name: `Region ${regionId}`,
				emoji: ':earth_americas:',
				latitude: 1,
				longitude: 2,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder<CreateVoiceServerResponse>(harness, `${admin.token}`)
			.post(`/admin/voice/regions/${regionId}/servers`)
			.body({
				server_id: serverId,
				endpoint: 'https://voice-keep.example.com/socket',
				api_key: 'keep-api-key',
				api_secret: 'keep-api-secret',
				required_guild_features: ['VIP_VOICE'],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${regionId}/servers/${serverId}`)
			.body({is_active: false})
			.expect(HTTP_STATUS.OK)
			.execute();
		const persisted = await voiceRepository.getServer(regionId, serverId);
		expect(Array.from(persisted?.restrictions.requiredGuildFeatures ?? [])).toEqual(['VIP_VOICE']);
		expect(persisted?.isActive).toBe(false);
	});
	test('rejects a voice server soft connection limit below one', async () => {
		const admin = await createAdminWithAcls(harness, [AdminACLs.VOICE_REGION_CREATE, AdminACLs.VOICE_SERVER_CREATE]);
		const regionId = 'voice-region-soft-limit-invalid';
		await createBuilder<CreateVoiceRegionResponse>(harness, `${admin.token}`)
			.post('/admin/voice/regions')
			.body({
				id: regionId,
				name: `Region ${regionId}`,
				emoji: ':earth_americas:',
				latitude: 1,
				longitude: 2,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, `${admin.token}`)
			.post(`/admin/voice/regions/${regionId}/servers`)
			.body({
				server_id: 'voice-server-soft-limit-invalid',
				endpoint: 'https://voice-soft-limit-invalid.example.com/socket',
				api_key: 'soft-limit-invalid-api-key',
				api_secret: 'soft-limit-invalid-api-secret',
				soft_connection_limit: 0,
			})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
	});
});
