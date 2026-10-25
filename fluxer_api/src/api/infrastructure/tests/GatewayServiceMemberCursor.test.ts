// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {GatewayRpcClient} from '@app/api/infrastructure/GatewayRpcClient';
import {GatewayService} from '@app/api/infrastructure/GatewayService';
import {afterEach, describe, expect, it} from 'vitest';

function serviceRecordingParams(calls: Array<Record<string, unknown>>): GatewayService {
	GatewayRpcClient.createForTests({
		async call(_method: string, params: Record<string, unknown>): Promise<unknown> {
			calls.push(params);
			return {members: [], total: 0};
		},
		async destroy(): Promise<void> {},
	});
	return new GatewayService();
}

describe('GatewayService member list cursor', () => {
	afterEach(async () => {
		await GatewayRpcClient.resetForTests();
	});

	it('starts from the first member when after is 0', async () => {
		const calls: Array<Record<string, unknown>> = [];
		const service = serviceRecordingParams(calls);
		await service.listGuildMembersCursor({guildId: createGuildID(1n), limit: 1000, after: createUserID(0n)});
		await service.listGuildMembersCursor({guildId: createGuildID(1n), limit: 1000, after: createUserID(5n)});
		expect(calls).toEqual([
			{guild_id: '1', limit: 1000},
			{guild_id: '1', limit: 1000, after: '5'},
		]);
	});
});
