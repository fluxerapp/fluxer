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

import {createChannelID, createGuildID, createUserID, type GuildID} from '@fluxer/api/src/BrandedTypes';
import {GatewayRpcClient} from '@fluxer/api/src/infrastructure/GatewayRpcClient';
import {GatewayRpcMethodErrorCodes} from '@fluxer/api/src/infrastructure/GatewayRpcError';
import {GatewayService} from '@fluxer/api/src/infrastructure/GatewayService';
import {MockGatewayRpcTransport} from '@fluxer/api/src/test/mocks/MockGatewayRpcTransport';
import {CallAlreadyExistsError} from '@fluxer/errors/src/domains/channel/CallAlreadyExistsError';
import {InvalidChannelTypeForCallError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeForCallError';
import {NoActiveCallError} from '@fluxer/errors/src/domains/channel/NoActiveCallError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {GatewayTimeoutError} from '@fluxer/errors/src/domains/core/GatewayTimeoutError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UserNotInVoiceError} from '@fluxer/errors/src/domains/user/UserNotInVoiceError';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('GatewayRpcService Error Handling', () => {
	const TEST_GUILD_ID = createGuildID(123456789n);
	const TEST_USER_ID = createUserID(987654321n);
	const TEST_CHANNEL_ID = createChannelID(111222333n);

	let mockTransport: MockGatewayRpcTransport;
	let gatewayService: GatewayService;

	beforeEach(() => {
		mockTransport = new MockGatewayRpcTransport();
		GatewayRpcClient.createForTests(mockTransport);
		gatewayService = new GatewayService();
	});

	afterEach(async () => {
		gatewayService.destroy();
		await GatewayRpcClient.resetForTests();
		mockTransport.reset();
	});

	it('transforms guild_not_found RPC error to UnknownGuildError', async () => {
		mockTransport.setMethodError('guild.get_data', GatewayRpcMethodErrorCodes.GUILD_NOT_FOUND);

		await expect(
			gatewayService.getGuildData({
				guildId: TEST_GUILD_ID,
				userId: TEST_USER_ID,
			}),
		).rejects.toThrow(UnknownGuildError);
	});

	it('transforms forbidden RPC error to MissingPermissionsError', async () => {
		mockTransport.setMethodError('guild.get_data', GatewayRpcMethodErrorCodes.FORBIDDEN);

		await expect(
			gatewayService.getGuildData({
				guildId: TEST_GUILD_ID,
				userId: TEST_USER_ID,
			}),
		).rejects.toThrow(MissingPermissionsError);
	});

	it('transforms guild_not_found RPC error to UnknownGuildError for non-batched calls', async () => {
		mockTransport.setMethodError('guild.get_counts', GatewayRpcMethodErrorCodes.GUILD_NOT_FOUND);

		await expect(gatewayService.getGuildCounts(TEST_GUILD_ID)).rejects.toThrow(UnknownGuildError);
	});

	it('transforms call_already_exists RPC error to CallAlreadyExistsError', async () => {
		mockTransport.setMethodError('call.create', GatewayRpcMethodErrorCodes.CALL_ALREADY_EXISTS);

		await expect(gatewayService.createCall(TEST_CHANNEL_ID, '123', 'us-east', [], [])).rejects.toThrow(
			CallAlreadyExistsError,
		);
	});

	it('transforms call_not_found RPC error to NoActiveCallError', async () => {
		mockTransport.setMethodError('call.delete', GatewayRpcMethodErrorCodes.CALL_NOT_FOUND);

		await expect(gatewayService.deleteCall(TEST_CHANNEL_ID)).rejects.toThrow(NoActiveCallError);
	});

	it('transforms channel_not_found RPC error to UnknownChannelError', async () => {
		mockTransport.setMethodError('call.get', GatewayRpcMethodErrorCodes.CHANNEL_NOT_FOUND);

		await expect(gatewayService.getCall(TEST_CHANNEL_ID)).rejects.toThrow(UnknownChannelError);
	});

	it('transforms channel_not_voice RPC error to InvalidChannelTypeForCallError', async () => {
		mockTransport.setMethodError('call.get', GatewayRpcMethodErrorCodes.CHANNEL_NOT_VOICE);

		await expect(gatewayService.getCall(TEST_CHANNEL_ID)).rejects.toThrow(InvalidChannelTypeForCallError);
	});

	it('transforms user_not_in_voice RPC error to UserNotInVoiceError', async () => {
		mockTransport.setMethodError('guild.update_member_voice', GatewayRpcMethodErrorCodes.USER_NOT_IN_VOICE);

		await expect(
			gatewayService.updateMemberVoice({
				guildId: TEST_GUILD_ID,
				userId: TEST_USER_ID,
				mute: false,
				deaf: false,
			}),
		).rejects.toThrow(UserNotInVoiceError);
	});

	it('transforms timeout RPC error to GatewayTimeoutError', async () => {
		mockTransport.setMethodError('guild.get_data', GatewayRpcMethodErrorCodes.TIMEOUT);

		await expect(
			gatewayService.getGuildData({
				guildId: TEST_GUILD_ID,
				userId: TEST_USER_ID,
			}),
		).rejects.toThrow(GatewayTimeoutError);
	});

	it('does not open circuit breaker for mapped gateway business errors', async () => {
		mockTransport.setMethodError('guild.get_data', GatewayRpcMethodErrorCodes.GUILD_NOT_FOUND);

		for (let attempt = 0; attempt < 6; attempt += 1) {
			await expect(
				gatewayService.getGuildData({
					guildId: TEST_GUILD_ID,
					userId: TEST_USER_ID,
				}),
			).rejects.toThrow(UnknownGuildError);
		}
	});

	it('opens circuit breaker for repeated gateway internal errors', async () => {
		mockTransport.setMethodError('guild.get_data', GatewayRpcMethodErrorCodes.INTERNAL_ERROR);

		for (let attempt = 0; attempt < 5; attempt += 1) {
			await expect(
				gatewayService.getGuildData({
					guildId: TEST_GUILD_ID,
					userId: TEST_USER_ID,
				}),
			).rejects.toThrow(BadGatewayError);
		}

		await expect(
			gatewayService.getGuildData({
				guildId: TEST_GUILD_ID,
				userId: TEST_USER_ID,
			}),
		).rejects.toThrow(ServiceUnavailableError);
	});

	it('parses both member_count and online_count from getDiscoveryGuildCounts', async () => {
		const guildIdA = createGuildID(100n);
		const guildIdB = createGuildID(200n);

		mockTransport.setMethodResult('guild.get_online_counts_batch', {
			online_counts: [
				{guild_id: '100', member_count: 500, online_count: 42},
				{guild_id: '200', member_count: 1200, online_count: 300},
			],
		});

		const counts = await gatewayService.getDiscoveryGuildCounts([guildIdA, guildIdB]);

		expect(counts.size).toBe(2);
		expect(counts.get(guildIdA)).toEqual({memberCount: 500, onlineCount: 42});
		expect(counts.get(guildIdB)).toEqual({memberCount: 1200, onlineCount: 300});

		expect(mockTransport.call).toHaveBeenCalledWith('guild.get_online_counts_batch', {
			guild_ids: ['100', '200'],
		});
	});

	it('returns empty map from getDiscoveryGuildCounts when response has no entries', async () => {
		mockTransport.setMethodResult('guild.get_online_counts_batch', {
			online_counts: [],
		});

		const counts = await gatewayService.getDiscoveryGuildCounts([createGuildID(999n)]);

		expect(counts.size).toBe(0);
	});

	it('getDiscoveryOnlineCounts still works with member_count present in response', async () => {
		mockTransport.setMethodResult('guild.get_online_counts_batch', {
			online_counts: [{guild_id: '100', member_count: 500, online_count: 42}],
		});

		const counts = await gatewayService.getDiscoveryOnlineCounts([createGuildID(100n)]);

		expect(counts.size).toBe(1);
		expect(counts.get(100n as GuildID)).toBe(42);
	});
});
