// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import type {User} from '../../models/User';
import type {InstancePolicyConfig} from '../InstanceConfigRepository';
import {SingleCommunityService} from '../SingleCommunityService';

const EXISTING_GUILD_ID = '1234567890123456789';
const OWNER = {id: 42n} as unknown as User;

interface Harness {
	service: SingleCommunityService;
	written: Array<Partial<InstancePolicyConfig>>;
	createdNames: Array<string>;
}

function createHarness(params: {designatedGuildId: string | null; designatedGuildExists: boolean}): Harness {
	const written: Array<Partial<InstancePolicyConfig>> = [];
	const createdNames: Array<string> = [];
	let nextCreatedGuildId = 999n;
	const instanceConfigRepository = {
		getInstancePolicyConfig: async () => ({
			single_community_enabled: false,
			single_community_guild_id: params.designatedGuildId,
		}),
		setInstancePolicyConfig: async (patch: Partial<InstancePolicyConfig>) => {
			written.push(patch);
		},
	};
	const guildDataService = {
		getGuildSystem: async () => {
			if (!params.designatedGuildExists) {
				throw new Error('unknown guild');
			}
			return {} as never;
		},
		createGuild: async ({data}: {data: {name: string}}) => {
			createdNames.push(data.name);
			nextCreatedGuildId += 1n;
			return {id: nextCreatedGuildId.toString()} as never;
		},
	};
	const service = new SingleCommunityService(
		instanceConfigRepository as never,
		guildDataService as never,
		null as never,
	);
	return {service, written, createdNames};
}

describe('SingleCommunityService.ensureStockCommunity', () => {
	it('reuses the designated community when it still exists', async () => {
		const harness = createHarness({designatedGuildId: EXISTING_GUILD_ID, designatedGuildExists: true});
		const guildId = await harness.service.ensureStockCommunity({owner: OWNER, name: 'Fluxer'});
		expect(guildId.toString()).toBe(EXISTING_GUILD_ID);
		expect(harness.createdNames).toEqual([]);
		expect(harness.written).toEqual([{single_community_enabled: true, single_community_guild_id: EXISTING_GUILD_ID}]);
	});

	it('creates a fresh community when the designated one was deleted', async () => {
		const harness = createHarness({designatedGuildId: EXISTING_GUILD_ID, designatedGuildExists: false});
		const guildId = await harness.service.ensureStockCommunity({owner: OWNER, name: 'Fluxer'});
		expect(guildId.toString()).not.toBe(EXISTING_GUILD_ID);
		expect(harness.createdNames).toEqual(['Fluxer']);
	});

	it('creates a fresh community when the instance never designated one', async () => {
		const harness = createHarness({designatedGuildId: null, designatedGuildExists: false});
		await harness.service.ensureStockCommunity({owner: OWNER, name: 'Fluxer'});
		expect(harness.createdNames).toEqual(['Fluxer']);
	});

	it('creates a fresh community when the stored guild id is not a snowflake', async () => {
		const harness = createHarness({designatedGuildId: 'not-a-snowflake', designatedGuildExists: true});
		await harness.service.ensureStockCommunity({owner: OWNER, name: 'Fluxer'});
		expect(harness.createdNames).toEqual(['Fluxer']);
	});
});
