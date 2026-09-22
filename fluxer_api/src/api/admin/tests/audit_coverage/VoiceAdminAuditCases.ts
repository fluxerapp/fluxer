// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';

const REGION_ID = 'coverage-region';
const REGION_NAME = 'Coverage Region';
const SERVER_ID = 'coverage-server';
const SERVER_ENDPOINT = 'wss://voice-coverage.example.com/livekit';

async function createRegion(harness: ApiTestHarness, admin: TestAccount): Promise<void> {
	await createBuilder(harness, admin.token)
		.post('/admin/voice/regions')
		.body({id: REGION_ID, name: REGION_NAME, emoji: ':earth_africa:', latitude: 1, longitude: 2})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function createRegionWithServer(harness: ApiTestHarness, admin: TestAccount): Promise<void> {
	await createRegion(harness, admin);
	await createBuilder(harness, admin.token)
		.post(`/admin/voice/regions/${REGION_ID}/servers`)
		.body({
			server_id: SERVER_ID,
			endpoint: SERVER_ENDPOINT,
			api_key: 'coverage-api-key',
			api_secret: 'coverage-api-secret',
		})
		.expect(HTTP_STATUS.OK)
		.execute();
}

export const VoiceAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/voice/regions',
		async prepare({harness, admin}) {
			await createRegionWithServer(harness, admin);
			return {
				request: {path: '/admin/voice/regions?include_servers=true'},
				expected: {
					action: 'list_voice_regions',
					targetType: 'voice_region',
					targetId: '0',
					metadata: {include_servers: 'true', result_count: '1'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/voice/regions',
		async prepare() {
			return {
				request: {
					path: '/admin/voice/regions',
					body: {id: REGION_ID, name: REGION_NAME, emoji: ':earth_africa:', latitude: 1, longitude: 2},
				},
				expected: {
					action: 'create_voice_region',
					targetType: 'voice_region',
					targetId: '0',
					metadata: {region_id: REGION_ID, name: REGION_NAME},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/voice/regions/:region_id',
		name: 'found',
		async prepare({harness, admin}) {
			await createRegionWithServer(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}`},
				expected: {
					action: 'get_voice_region',
					targetType: 'voice_region',
					targetId: '0',
					metadata: {region_id: REGION_ID, include_servers: 'true', found: 'true'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/voice/regions/:region_id',
		name: 'missing',
		async prepare() {
			return {
				request: {path: '/admin/voice/regions/missing-region?include_servers=false'},
				expected: {
					action: 'get_voice_region',
					targetType: 'voice_region',
					targetId: '0',
					metadata: {region_id: 'missing-region', include_servers: 'false', found: 'false'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/voice/regions/:region_id',
		async prepare({harness, admin}) {
			await createRegion(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}`, body: {name: 'Renamed Region'}},
				expected: {
					action: 'update_voice_region',
					targetType: 'voice_region',
					targetId: '0',
					metadata: {region_id: REGION_ID},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/voice/regions/:region_id',
		async prepare({harness, admin}) {
			await createRegion(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}`},
				expected: {
					action: 'delete_voice_region',
					targetType: 'voice_region',
					targetId: '0',
					metadata: {region_id: REGION_ID, name: REGION_NAME},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/voice/regions/:region_id/servers',
		async prepare({harness, admin}) {
			await createRegionWithServer(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}/servers`},
				expected: {
					action: 'list_voice_servers',
					targetType: 'voice_server',
					targetId: '0',
					metadata: {region_id: REGION_ID, result_count: '1'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/voice/regions/:region_id/servers',
		async prepare({harness, admin}) {
			await createRegion(harness, admin);
			return {
				request: {
					path: `/admin/voice/regions/${REGION_ID}/servers`,
					body: {
						server_id: SERVER_ID,
						endpoint: SERVER_ENDPOINT,
						api_key: 'coverage-api-key',
						api_secret: 'coverage-api-secret',
					},
				},
				expected: {
					action: 'create_voice_server',
					targetType: 'voice_server',
					targetId: '0',
					metadata: {region_id: REGION_ID, server_id: SERVER_ID, endpoint: SERVER_ENDPOINT},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/voice/regions/:region_id/servers/:server_id',
		name: 'found',
		async prepare({harness, admin}) {
			await createRegionWithServer(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}/servers/${SERVER_ID}`},
				expected: {
					action: 'get_voice_server',
					targetType: 'voice_server',
					targetId: '0',
					metadata: {region_id: REGION_ID, server_id: SERVER_ID, found: 'true'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/voice/regions/:region_id/servers/:server_id',
		name: 'missing',
		async prepare({harness, admin}) {
			await createRegion(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}/servers/missing-server`},
				expected: {
					action: 'get_voice_server',
					targetType: 'voice_server',
					targetId: '0',
					metadata: {region_id: REGION_ID, server_id: 'missing-server', found: 'false'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/voice/regions/:region_id/servers/:server_id',
		async prepare({harness, admin}) {
			await createRegionWithServer(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}/servers/${SERVER_ID}`, body: {is_active: false}},
				expected: {
					action: 'update_voice_server',
					targetType: 'voice_server',
					targetId: '0',
					metadata: {region_id: REGION_ID, server_id: SERVER_ID},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/voice/regions/:region_id/servers/:server_id',
		async prepare({harness, admin}) {
			await createRegionWithServer(harness, admin);
			return {
				request: {path: `/admin/voice/regions/${REGION_ID}/servers/${SERVER_ID}`},
				expected: {
					action: 'delete_voice_server',
					targetType: 'voice_server',
					targetId: '0',
					metadata: {region_id: REGION_ID, server_id: SERVER_ID, endpoint: SERVER_ENDPOINT},
				},
			};
		},
	},
];
