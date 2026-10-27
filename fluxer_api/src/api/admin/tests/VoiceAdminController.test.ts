// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminWithAcls} from '@app/api/admin/tests/AdminTestUtils';
import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {
	CreateVoiceRegionResponse,
	CreateVoiceServerResponse,
	UpdateVoiceServerResponse,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface VoiceFixture {
	regionId: string;
	serverId: string;
	initialApiKey: string;
	initialApiSecret: string;
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
	test('stores, keeps and clears free-form server notes', async () => {
		const admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_UPDATE,
		]);
		const fixture = await createVoiceFixture(harness, admin, {
			regionId: 'voice-region-notes',
			serverId: 'voice-server-notes',
			endpoint: 'https://voice-notes.example.com/socket',
			apiKey: 'notes-api-key',
			apiSecret: 'notes-api-secret',
		});
		expect((await voiceRepository.getServer(fixture.regionId, fixture.serverId))?.notes).toBeNull();
		const updated = await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.body({notes: '  Hetzner box, renew in March  '})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.server.notes).toBe('Hetzner box, renew in March');
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.body({is_active: false})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect((await voiceRepository.getServer(fixture.regionId, fixture.serverId))?.notes).toBe(
			'Hetzner box, renew in March',
		);
		await createBuilder<UpdateVoiceServerResponse>(harness, `${admin.token}`)
			.patch(`/admin/voice/regions/${fixture.regionId}/servers/${fixture.serverId}`)
			.body({notes: '   '})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect((await voiceRepository.getServer(fixture.regionId, fixture.serverId))?.notes).toBeNull();
	});
});
