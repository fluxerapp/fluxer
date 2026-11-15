// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminWithAcls} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import type {LiveKitService} from '@app/api/infrastructure/LiveKitService';
import type {VoiceRoomStore} from '@app/api/infrastructure/VoiceRoomStore';
import {
	getLiveKitServiceInstance,
	getVoiceAvailabilityService,
	getVoiceRoomStoreInstance,
	getVoiceTopology,
} from '@app/api/middleware/ServiceRegistry';
import {
	getChannelRepository,
	getGuildRepository,
	getInstanceConfigRepository,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {VoiceService} from '@app/api/voice/VoiceService';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {
	GetVoiceRegionResponse,
	VoiceRegionAdminResponse,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import type {ChannelResponse, RtcRegionResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const LISTED_REGION_ID = 'example-listed';
const HIDDEN_REGION_ID = 'example-hidden';

interface RegionFixture {
	id: string;
	latitude: number;
	longitude: number;
	hidden?: boolean;
}

interface ValidationErrorBody {
	errors?: Array<{code?: string}>;
}

describe('hidden voice regions', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let owner: TestAccount;
	let guildId: string;
	let channelId: string;
	const previousVoiceConfig = {...Config.voice};

	beforeEach(async () => {
		Config.voice.enabled = true;
		Config.voice.defaultRegion = undefined;
		Config.voice.healthIntervalMs = 0;
		harness = await createApiTestHarness();
		admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_REGION_LIST,
			AdminACLs.VOICE_REGION_UPDATE,
			AdminACLs.VOICE_SERVER_CREATE,
		]);
		owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Hidden region guild');
		guildId = guild.id;
		const channel = await createChannel(harness, owner.token, guild.id, 'voice', ChannelTypes.GUILD_VOICE);
		channelId = channel.id;
	});

	afterEach(async () => {
		await harness?.shutdown();
		Object.assign(Config.voice, previousVoiceConfig);
	});

	async function createRegion(fixture: RegionFixture): Promise<void> {
		await createBuilder(harness, admin.token)
			.post('/admin/voice/regions')
			.body({
				id: fixture.id,
				name: `Region ${fixture.id}`,
				emoji: ':earth_americas:',
				latitude: fixture.latitude,
				longitude: fixture.longitude,
				...(fixture.hidden !== undefined && {hidden: fixture.hidden}),
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, admin.token)
			.post(`/admin/voice/regions/${fixture.id}/servers`)
			.body({
				server_id: `${fixture.id}-server-1`,
				endpoint: `wss://${fixture.id}.example.test`,
				api_key: `${fixture.id}-key`,
				api_secret: `${fixture.id}-secret`,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function setHidden(regionId: string, hidden: boolean): Promise<VoiceRegionAdminResponse> {
		const response = await createBuilder<{region: VoiceRegionAdminResponse}>(harness, admin.token)
			.patch(`/admin/voice/regions/${regionId}`)
			.body({hidden})
			.expect(HTTP_STATUS.OK)
			.execute();
		return response.region;
	}

	async function waitForTopology(regionId: string, hidden: boolean): Promise<void> {
		await vi.waitFor(() => {
			const topology = getVoiceTopology();
			const region = topology?.getAllRegions().find((candidate) => candidate.id === regionId);
			expect(region?.hidden).toBe(hidden);
			expect(topology?.getServersForRegion(regionId)).toHaveLength(1);
		});
	}

	async function createRegions(): Promise<void> {
		await createRegion({id: LISTED_REGION_ID, latitude: 51.5, longitude: -0.1});
		await createRegion({id: HIDDEN_REGION_ID, latitude: 45.5, longitude: -73.6, hidden: true});
		await waitForTopology(LISTED_REGION_ID, false);
		await waitForTopology(HIDDEN_REGION_ID, true);
	}

	async function listRtcRegionIds(): Promise<Array<string>> {
		const regions = await createBuilder<Array<RtcRegionResponse>>(harness, owner.token)
			.get(`/channels/${channelId}/rtc-regions`)
			.expect(HTTP_STATUS.OK)
			.execute();
		return regions.map((region) => region.id);
	}

	function patchChannelRegion(body: Record<string, unknown>, status: number) {
		return createBuilder<ChannelResponse & ValidationErrorBody>(harness, owner.token)
			.patch(`/channels/${channelId}`)
			.body(body)
			.expect(status)
			.execute();
	}

	function createVoiceService(): VoiceService {
		const liveKitService = getLiveKitServiceInstance();
		const roomStore = getVoiceRoomStoreInstance();
		const availability = getVoiceAvailabilityService();
		if (!liveKitService || !roomStore || !availability) {
			throw new Error('Voice resources are not initialised');
		}
		return new VoiceService(
			liveKitService as LiveKitService,
			getGuildRepository(),
			getUserRepository(),
			getChannelRepository(),
			roomStore as VoiceRoomStore,
			availability,
			getInstanceConfigRepository(),
		);
	}

	test('leaves a hidden region out of the channel region list', async () => {
		await createRegions();
		expect(await listRtcRegionIds()).toEqual([LISTED_REGION_ID]);
		await setHidden(HIDDEN_REGION_ID, false);
		await waitForTopology(HIDDEN_REGION_ID, false);
		expect((await listRtcRegionIds()).sort()).toEqual([HIDDEN_REGION_ID, LISTED_REGION_ID]);
	});

	test('automatic selection still places a call in a hidden region', async () => {
		await createRegions();
		const grant = await createVoiceService().getVoiceToken({
			guildId: createGuildID(BigInt(guildId)),
			channelId: createChannelID(BigInt(channelId)),
			userId: createUserID(BigInt(owner.userId)),
			latitude: '45.4',
			longitude: '-73.5',
		});
		expect(grant).toMatchObject({regionId: HIDDEN_REGION_ID, serverId: `${HIDDEN_REGION_ID}-server-1`});
	});

	test('a channel already set to a region keeps using it after the region is hidden', async () => {
		await createRegion({id: LISTED_REGION_ID, latitude: 51.5, longitude: -0.1});
		await createRegion({id: HIDDEN_REGION_ID, latitude: 45.5, longitude: -73.6});
		await waitForTopology(HIDDEN_REGION_ID, false);
		await patchChannelRegion({rtc_region: HIDDEN_REGION_ID}, HTTP_STATUS.OK);
		await setHidden(HIDDEN_REGION_ID, true);
		await waitForTopology(HIDDEN_REGION_ID, true);
		const grant = await createVoiceService().getVoiceToken({
			guildId: createGuildID(BigInt(guildId)),
			channelId: createChannelID(BigInt(channelId)),
			userId: createUserID(BigInt(owner.userId)),
			latitude: '51.5',
			longitude: '-0.1',
		});
		expect(grant).toMatchObject({regionId: HIDDEN_REGION_ID});
	});

	test('refuses a new pick of a hidden region but keeps a channel that already has it', async () => {
		await createRegion({id: LISTED_REGION_ID, latitude: 51.5, longitude: -0.1});
		await createRegion({id: HIDDEN_REGION_ID, latitude: 45.5, longitude: -73.6});
		await waitForTopology(HIDDEN_REGION_ID, false);
		await patchChannelRegion({rtc_region: HIDDEN_REGION_ID}, HTTP_STATUS.OK);
		await setHidden(HIDDEN_REGION_ID, true);
		await waitForTopology(HIDDEN_REGION_ID, true);
		const resaved = await patchChannelRegion({name: 'renamed', rtc_region: HIDDEN_REGION_ID}, HTTP_STATUS.OK);
		expect(resaved.rtc_region).toBe(HIDDEN_REGION_ID);
		expect(resaved.name).toBe('renamed');
		const moved = await patchChannelRegion({rtc_region: LISTED_REGION_ID}, HTTP_STATUS.OK);
		expect(moved.rtc_region).toBe(LISTED_REGION_ID);
		const refused = await patchChannelRegion({rtc_region: HIDDEN_REGION_ID}, HTTP_STATUS.BAD_REQUEST);
		expect(refused.errors?.map((error) => error.code)).toContain(ValidationErrorCodes.INVALID_OR_RESTRICTED_RTC_REGION);
		const cleared = await patchChannelRegion({rtc_region: null}, HTTP_STATUS.OK);
		expect(cleared.rtc_region).toBeNull();
	});

	test('the admin flag round-trips and survives unrelated edits', async () => {
		await createRegion({id: HIDDEN_REGION_ID, latitude: 45.5, longitude: -73.6, hidden: true});
		const getRegion = async () =>
			(
				await createBuilder<GetVoiceRegionResponse>(harness, admin.token)
					.get(`/admin/voice/regions/${HIDDEN_REGION_ID}`)
					.expect(HTTP_STATUS.OK)
					.execute()
			).region;
		expect((await getRegion())?.hidden).toBe(true);
		await createBuilder(harness, admin.token)
			.patch(`/admin/voice/regions/${HIDDEN_REGION_ID}`)
			.body({name: 'Renamed region'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await getRegion()).toMatchObject({name: 'Renamed region', hidden: true});
		expect((await setHidden(HIDDEN_REGION_ID, false)).hidden).toBe(false);
		expect((await getRegion())?.hidden).toBe(false);
		expect((await setHidden(HIDDEN_REGION_ID, true)).hidden).toBe(true);
		expect((await getRegion())?.hidden).toBe(true);
	});

	test('a new region is listed unless the flag is set', async () => {
		await createRegion({id: LISTED_REGION_ID, latitude: 51.5, longitude: -0.1});
		const region = (
			await createBuilder<GetVoiceRegionResponse>(harness, admin.token)
				.get(`/admin/voice/regions/${LISTED_REGION_ID}`)
				.expect(HTTP_STATUS.OK)
				.execute()
		).region;
		expect(region?.hidden).toBe(false);
	});
});
