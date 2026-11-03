// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import type {GatewayRpcTransport} from '@app/api/infrastructure/GatewayRpcClient';
import {GatewayRpcClient} from '@app/api/infrastructure/GatewayRpcClient';
import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from '@app/api/infrastructure/GatewayRpcError';
import {GatewayService} from '@app/api/infrastructure/GatewayService';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {afterEach, describe, expect, it} from 'vitest';

function failingTransport(code: string): GatewayRpcTransport {
	return {
		async call(): Promise<unknown> {
			throw new GatewayRpcMethodError(code);
		},
		async destroy(): Promise<void> {},
	};
}

function serviceRaising(code: string): GatewayService {
	GatewayRpcClient.createForTests(failingTransport(code));
	return new GatewayService();
}

describe('GatewayService gateway error mapping', () => {
	afterEach(async () => {
		await GatewayRpcClient.resetForTests();
	});

	it('returns 503 with Retry-After for an overloaded guild', async () => {
		const service = serviceRaising(GatewayRpcMethodErrorCodes.GUILD_OVERLOADED);
		const error = await service
			.getUserPermissions({guildId: createGuildID(1n), userId: createUserID(2n)})
			.catch((raised: unknown) => raised);
		expect(error).toBeInstanceOf(ServiceUnavailableError);
		const response = (error as ServiceUnavailableError).getResponse();
		expect(response.status).toBe(503);
		expect(response.headers.get('Retry-After')).toBe('1');
	});

	it('does not retry a guild overload response', async () => {
		let calls = 0;
		const client = GatewayRpcClient.createForTests({
			async call(): Promise<unknown> {
				calls += 1;
				throw new GatewayRpcMethodError(GatewayRpcMethodErrorCodes.GUILD_OVERLOADED);
			},
			async destroy(): Promise<void> {},
		});
		await expect(client.call('guild.dispatch', {guild_id: '1'})).rejects.toMatchObject({
			code: GatewayRpcMethodErrorCodes.GUILD_OVERLOADED,
		});
		expect(calls).toBe(1);
	});
});
