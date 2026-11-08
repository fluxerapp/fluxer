// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {setInjectedGatewayService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

type GuildDispatch = Parameters<NoopGatewayService['dispatchGuild']>[0];

class FlakyGateway extends NoopGatewayService {
	failGuildDelete = false;
	guildDeleteDispatches = 0;

	override async dispatchGuild(params: GuildDispatch): Promise<void> {
		if (params.event !== 'GUILD_DELETE') return;
		if (this.failGuildDelete) {
			throw new Error('gateway unavailable');
		}
		this.guildDeleteDispatches++;
	}
}

describe('Guild deletion scheduling', () => {
	let harness: ApiTestHarness;
	let gateway: FlakyGateway;
	let worker: RecordingWorkerService;
	let owner: TestAccount;
	let guild: GuildResponse;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		gateway = new FlakyGateway();
		worker = new RecordingWorkerService();
		setInjectedGatewayService(gateway);
		setInjectedWorkerService(worker);
		await harness.resetData();
		owner = await createTestAccount(harness);
		guild = await createGuild(harness, owner.token, 'Deletion Guild');
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	function deleteGuild(expected: number) {
		return createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/delete`)
			.body({password: owner.password})
			.expect(expected)
			.execute();
	}

	async function pendingFlags(): Promise<Array<string>> {
		const row = await new GuildRepository().findUnique(createGuildID(BigInt(guild.id)));
		return [GuildFeatures.UNAVAILABLE_FOR_EVERYONE, GuildFeatures.UNAVAILABLE_HIDDEN].filter((feature) =>
			row?.features.has(feature),
		);
	}

	function deletionJobs() {
		return worker.byTask('guildProcessDeletion').filter((job) => job.payload.guildId === guild.id);
	}

	test('a failed GUILD_DELETE dispatch still queues the purge', async () => {
		gateway.failGuildDelete = true;
		await deleteGuild(HTTP_STATUS.NO_CONTENT);
		expect(deletionJobs()).toHaveLength(1);
		expect(deletionJobs()[0]?.payload.copyMode).toBe('source_deleted');
	});

	test('a failed enqueue leaves the guild available so the owner can delete it again', async () => {
		worker.failure = (taskType) => (taskType === 'guildProcessDeletion' ? new Error('jobs stream full') : null);
		await deleteGuild(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		expect(await pendingFlags()).toEqual([]);
		expect(deletionJobs()).toHaveLength(0);
		expect(gateway.guildDeleteDispatches).toBe(0);
		worker.failure = null;
		await deleteGuild(HTTP_STATUS.NO_CONTENT);
		expect(await pendingFlags()).toHaveLength(2);
		expect(deletionJobs()).toHaveLength(1);
		expect(gateway.guildDeleteDispatches).toBe(1);
	});
});
