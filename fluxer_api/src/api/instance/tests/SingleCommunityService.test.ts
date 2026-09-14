// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstancePolicyConfig} from '@app/api/instance/InstanceConfigRepository';
import {SingleCommunityService} from '@app/api/instance/SingleCommunityService';
import type {User} from '@app/api/models/User';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {describe, expect, it} from 'vitest';

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
				throw new UnknownGuildError();
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

	it('rejects an invalid stored guild id without creating a community', async () => {
		const harness = createHarness({designatedGuildId: 'not-a-snowflake', designatedGuildExists: true});
		await expect(harness.service.ensureStockCommunity({owner: OWNER, name: 'Fluxer'})).rejects.toThrow(SyntaxError);
		expect(harness.createdNames).toEqual([]);
	});
});
