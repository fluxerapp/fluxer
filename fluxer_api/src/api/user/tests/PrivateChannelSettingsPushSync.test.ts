// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {GatewayRpcClient} from '@app/api/infrastructure/GatewayRpcClient';
import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from '@app/api/infrastructure/GatewayRpcError';
import {GatewayService} from '@app/api/infrastructure/GatewayService';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {updateGuildSettings} from '@app/api/user/tests/UserTestUtils';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const DM_CHANNEL_ID = '1500000000000000005';
const MUTE_DM = {
	channel_overrides: {
		[DM_CHANNEL_ID]: {collapsed: false, message_notifications: 3, muted: true, mute_config: null},
	},
};

function gatewayRejectingSyncWith(code: string): GatewayService {
	GatewayRpcClient.createForTests({
		async call(): Promise<unknown> {
			throw new GatewayRpcMethodError(code);
		},
		async destroy(): Promise<void> {},
	});
	return new GatewayService();
}

function routeSyncThrough(gateway: GatewayService): void {
	vi.spyOn(NoopGatewayService.prototype, 'syncPushUserGuildSettings').mockImplementation((params) =>
		gateway.syncPushUserGuildSettings(params),
	);
}

describe('Private channel settings push sync', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(async () => {
		vi.restoreAllMocks();
		await GatewayRpcClient.resetForTests();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('syncs a muted DM to the push cache under the private scope', async () => {
		const account = await createTestAccount(harness);
		const syncPush = vi.spyOn(NoopGatewayService.prototype, 'syncPushUserGuildSettings');

		await updateGuildSettings(harness, account.token, MUTE_DM);

		expect(syncPush).toHaveBeenCalledTimes(1);
		const [params] = syncPush.mock.calls[0]!;
		expect(params.userId.toString()).toBe(account.userId);
		expect(params.guildId.toString()).toBe('0');
		expect(params.settings).toMatchObject({
			guild_id: null,
			channel_overrides: {[DM_CHANNEL_ID]: {muted: true}},
		});
	});

	it('still mutes a DM when the gateway predates private scope sync', async () => {
		const account = await createTestAccount(harness);
		routeSyncThrough(gatewayRejectingSyncWith(GatewayRpcMethodErrorCodes.INVALID_PARAMS));

		const {json} = await updateGuildSettings(harness, account.token, MUTE_DM);

		expect(json).toMatchObject({channel_overrides: {[DM_CHANNEL_ID]: {muted: true}}});
	});

	it('fails the DM settings update on any other gateway sync error', async () => {
		const account = await createTestAccount(harness);
		routeSyncThrough(gatewayRejectingSyncWith(GatewayRpcMethodErrorCodes.INTERNAL_ERROR));

		await createBuilder(harness, account.token)
			.patch('/users/@me/guilds/@me/settings')
			.body(MUTE_DM)
			.expect(502)
			.execute();
	});

	it('keeps rejecting invalid_params for a real guild', async () => {
		const gateway = gatewayRejectingSyncWith(GatewayRpcMethodErrorCodes.INVALID_PARAMS);

		await expect(
			gateway.syncPushUserGuildSettings({userId: createUserID(2n), guildId: createGuildID(1n), settings: {}}),
		).rejects.toBeInstanceOf(BadRequestError);
	});
});
