// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUniqueEmail, createUniqueUsername} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {
	VOICE_P2P_CONNECTION_REPORT_COLUMNS,
	type VoiceP2pConnectionReportRow,
} from '@app/api/database/types/VoiceTypes';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {VoiceP2pConnectionReports} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS, TEST_USER_DATA} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {DEFAULT_VOICE_P2P_CONFIG} from '@fluxer/schema/src/domains/admin/VoiceP2pSchemas';
import type {GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const {lookupGeoipMock} = vi.hoisted(() => ({
	lookupGeoipMock: vi.fn(),
}));

vi.mock('@app/api/utils/IpUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/api/utils/IpUtils')>()),
	lookupGeoip: lookupGeoipMock,
}));

const ENDPOINT = '/voice/p2p/connection-reports';
const CLIENT_IP = '203.0.113.10';
const ANDROID_USER_AGENT = 'Fluxer Android/1.4.0';

const LIST_REPORTS = VoiceP2pConnectionReports.select({where: VoiceP2pConnectionReports.where.eq('user_id')});

function geoipCountry(countryCode: string | null): GeoipResult {
	return {countryCode, normalizedIp: CLIENT_IP, city: 'São Paulo', region: 'SP', countryName: 'Brazil'};
}

function connectedReport(overrides: Record<string, unknown> = {}): Record<string, unknown> {
	return {
		channel_id: '1189375284394692610',
		guild_id: null,
		participant_count: 2,
		outcome: 'connected',
		local_candidate_type: 'srflx',
		remote_candidate_type: 'host',
		ip_family: 'ipv6',
		protocol: 'udp',
		setup_ms: 840,
		ice_restarted: false,
		...overrides,
	};
}

async function registerAndroidAccount(harness: ApiTestHarness): Promise<{userId: string; token: string}> {
	const registration = await createBuilder<{user_id: string; token: string}>(harness, '')
		.post('/auth/register')
		.header('user-agent', ANDROID_USER_AGENT)
		.header('x-forwarded-for', CLIENT_IP)
		.body({
			email: createUniqueEmail('p2p-report'),
			username: createUniqueUsername('p2preport'),
			global_name: TEST_USER_DATA.DEFAULT_GLOBAL_NAME,
			password: TEST_CREDENTIALS.STRONG_PASSWORD,
			date_of_birth: TEST_USER_DATA.DEFAULT_DATE_OF_BIRTH,
			consent: true,
		})
		.execute();
	return {userId: registration.user_id, token: registration.token};
}

async function listReports(userId: string): Promise<Array<VoiceP2pConnectionReportRow>> {
	return fetchMany<VoiceP2pConnectionReportRow>(LIST_REPORTS.bind({user_id: createUserID(BigInt(userId))}));
}

describe('POST /voice/p2p/connection-reports', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		lookupGeoipMock.mockReset();
		lookupGeoipMock.mockResolvedValue(geoipCountry('BR'));
		await getInstanceConfigRepository().setVoiceP2pConfig({...DEFAULT_VOICE_P2P_CONFIG, enabled: true});
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('rejects an unauthenticated caller', async () => {
		await createBuilderWithoutAuth(harness)
			.post(ENDPOINT)
			.body({reports: [connectedReport()]})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});

	it('rejects a bot', async () => {
		const bot = await createTestBotAccount(harness);
		await createBuilder(harness, `Bot ${bot.botToken}`)
			.post(ENDPOINT)
			.body({reports: [connectedReport()]})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('stores each report with the server-derived fields and the request address', async () => {
		const account = await registerAndroidAccount(harness);

		await createBuilder(harness, account.token)
			.post(ENDPOINT)
			.header('x-forwarded-for', CLIENT_IP)
			.body({
				reports: [
					connectedReport(),
					connectedReport({
						guild_id: '1189375284394692608',
						participant_count: 3,
						outcome: 'failed',
						local_candidate_type: null,
						remote_candidate_type: null,
						ip_family: null,
						protocol: null,
						setup_ms: null,
						ice_restarted: true,
					}),
				],
			})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const rows = await listReports(account.userId);
		expect(rows).toHaveLength(2);
		const shared = {
			user_id: BigInt(account.userId),
			channel_id: 1189375284394692610n,
			country: 'BR',
			client_platform: `${getInstanceProductName()} Android`,
			client_os: 'Android',
		};
		const connected = rows.find((row) => row.outcome === 'connected');
		const failed = rows.find((row) => row.outcome === 'failed');
		expect(connected).toMatchObject({
			...shared,
			guild_id: null,
			participant_count: 2,
			local_candidate_type: 'srflx',
			remote_candidate_type: 'host',
			ip_family: 'ipv6',
			protocol: 'udp',
			setup_ms: 840,
			ice_restarted: false,
		});
		expect(failed).toMatchObject({
			...shared,
			guild_id: 1189375284394692608n,
			participant_count: 3,
			local_candidate_type: null,
			remote_candidate_type: null,
			ip_family: null,
			protocol: null,
			setup_ms: null,
			ice_restarted: true,
		});
		for (const row of rows) {
			expect(Object.keys(row).sort()).toEqual([...VOICE_P2P_CONNECTION_REPORT_COLUMNS].sort());
			expect(row.reported_at).toBeInstanceOf(Date);
			expect(row.ip).toBe(CLIENT_IP);
		}
		expect(connected?.report_id).not.toEqual(failed?.report_id);
	});

	it('accepts and drops reports while the experiment is disabled', async () => {
		await getInstanceConfigRepository().setVoiceP2pConfig(DEFAULT_VOICE_P2P_CONFIG);
		const account = await registerAndroidAccount(harness);

		await createBuilder(harness, account.token)
			.post(ENDPOINT)
			.body({reports: [connectedReport()]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		expect(await listReports(account.userId)).toEqual([]);
	});

	it('stores a null country when the lookup finds none', async () => {
		lookupGeoipMock.mockResolvedValue(geoipCountry(null));
		const account = await registerAndroidAccount(harness);

		await createBuilder(harness, account.token)
			.post(ENDPOINT)
			.body({reports: [connectedReport()]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const rows = await listReports(account.userId);
		expect(rows).toHaveLength(1);
		expect(rows[0]?.country).toBeNull();
	});

	it.each([
		{name: 'an empty batch', body: {reports: []}},
		{name: 'more than 8 reports', body: {reports: Array.from({length: 9}, () => connectedReport())}},
		{name: 'a single participant', body: {reports: [connectedReport({participant_count: 1})]}},
		{name: 'more than 4 participants', body: {reports: [connectedReport({participant_count: 5})]}},
		{name: 'an unknown outcome', body: {reports: [connectedReport({outcome: 'checking'})]}},
		{name: 'an unknown candidate type', body: {reports: [connectedReport({local_candidate_type: 'turn'})]}},
		{name: 'an unknown address family', body: {reports: [connectedReport({ip_family: 'ipx'})]}},
		{name: 'an unknown protocol', body: {reports: [connectedReport({protocol: 'sctp'})]}},
		{name: 'a negative setup time', body: {reports: [connectedReport({setup_ms: -1})]}},
		{name: 'a malformed channel id', body: {reports: [connectedReport({channel_id: 'abc'})]}},
		{name: 'a missing ice_restarted', body: {reports: [connectedReport({ice_restarted: undefined})]}},
	])('rejects $name and stores nothing', async ({body}) => {
		const account = await registerAndroidAccount(harness);

		await createBuilder(harness, account.token)
			.post(ENDPOINT)
			.body(body)
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();

		expect(await listReports(account.userId)).toEqual([]);
	});
});
