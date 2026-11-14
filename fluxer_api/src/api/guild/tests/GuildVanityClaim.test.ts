// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Guild vanity URL claim', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('gives a vanity code to only one of two communities claiming it at once', async () => {
		const owners = await Promise.all([createTestAccount(harness), createTestAccount(harness)]);
		const guilds = await Promise.all(owners.map((owner, i) => createGuild(harness, owner.token, `Vanity ${i}`)));
		for (const guild of guilds) {
			await createBuilder(harness, '')
				.post(`/test/guilds/${guild.id}/features`)
				.body({add_features: [GuildFeatures.VANITY_URL]})
				.execute();
		}
		const results = await Promise.all(
			guilds.map((guild, i) =>
				createBuilder(harness, owners[i]!.token)
					.patch(`/guilds/${guild.id}/vanity-url`)
					.body({code: 'samecode'})
					.executeRaw(),
			),
		);
		const statuses = results.map((result) => result.response.status).sort();
		expect(statuses).toEqual([HTTP_STATUS.OK, HTTP_STATUS.BAD_REQUEST].sort());
		const codes = await Promise.all(
			guilds.map(async (guild, i) => {
				const response = await createBuilder<{code: string | null}>(harness, owners[i]!.token)
					.get(`/guilds/${guild.id}/vanity-url`)
					.execute();
				return response.code;
			}),
		);
		const winner = results.findIndex((result) => result.response.status === HTTP_STATUS.OK);
		expect(codes[winner]).toBe('samecode');
		expect(codes[1 - winner]).toBeNull();
	});
});
