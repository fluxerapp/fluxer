// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID, type GuildID} from '@app/api/BrandedTypes';
import {createTestGuildStack} from '@app/api/channel/tests/CrosspostTestUtils';
import {processRecordedGuildDeletions, recordGuildDeletions} from '@app/api/guild/tests/GuildDeletionTestUtils';
import {acceptInvite, createChannelInvite, createGuild, getChannel} from '@app/api/guild/tests/GuildTestUtils';
import {getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import type {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

describe('Guild deletion job', () => {
	let harness: ApiTestHarness;
	let worker: RecordingWorkerService;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		worker = recordGuildDeletions();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function guildWithMember(): Promise<{owner: TestAccount; member: TestAccount; guild: GuildResponse}> {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Deletion Guild');
		const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		return {owner, member, guild};
	}

	async function deleteAsOwner(owner: TestAccount, guildId: string): Promise<void> {
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guildId}/delete`)
			.body({password: owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	}

	function storedGuild(guildId: string) {
		return getGuildRepository().findUnique(createGuildID(BigInt(guildId)));
	}

	test('the request hides the guild and leaves the purge to the worker', async () => {
		const {owner, member, guild} = await guildWithMember();
		const dispatched = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const leaves = vi.spyOn(NoopGatewayService.prototype, 'leaveGuild');
		await deleteAsOwner(owner, guild.id);
		const jobs = worker.byTask('guildProcessDeletion');
		expect(jobs).toHaveLength(1);
		expect(jobs[0]?.payload).toEqual({guildId: guild.id, copyMode: 'source_deleted'});
		expect(jobs[0]?.options?.jobKey).toBe(`guild-deletion-${guild.id}`);
		expect(dispatched).toHaveBeenCalledWith(expect.objectContaining({event: 'GUILD_DELETE'}));
		expect(leaves).not.toHaveBeenCalled();
		const pending = await storedGuild(guild.id);
		expect(pending?.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE)).toBe(true);
		expect(pending?.features.has(GuildFeatures.UNAVAILABLE_HIDDEN)).toBe(true);
		await createBuilder(harness, member.token).get(`/guilds/${guild.id}`).expect(HTTP_STATUS.FORBIDDEN).execute();
		await processRecordedGuildDeletions(worker);
		expect(await storedGuild(guild.id)).toBeNull();
		await createBuilder(harness, owner.token).get(`/guilds/${guild.id}`).expect(HTTP_STATUS.NOT_FOUND).execute();
	});

	test('an admin deletion enqueues a purge job', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup', 'guild:delete']);
		const guild = await createGuild(harness, admin.token, 'Admin Deletion Guild');
		await createBuilder(harness, admin.token)
			.delete(`/admin/guilds/${guild.id}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(worker.byTask('guildProcessDeletion').map((job) => job.payload)).toEqual([
			{guildId: guild.id, copyMode: 'purge'},
		]);
		await processRecordedGuildDeletions(worker);
		expect(await storedGuild(guild.id)).toBeNull();
	});

	test('a purge interrupted by a gateway failure completes when retried', async () => {
		const {owner, guild} = await guildWithMember();
		await deleteAsOwner(owner, guild.id);
		const leaveGuild = NoopGatewayService.prototype.leaveGuild;
		const failing = vi
			.spyOn(NoopGatewayService.prototype, 'leaveGuild')
			.mockRejectedValueOnce(new Error('gateway timeout'))
			.mockImplementation(function (this: NoopGatewayService, params) {
				return leaveGuild.call(this, params);
			});
		await expect(processRecordedGuildDeletions(worker)).rejects.toThrow('gateway timeout');
		expect(await storedGuild(guild.id)).not.toBeNull();
		await processRecordedGuildDeletions(worker);
		expect(await storedGuild(guild.id)).toBeNull();
		await processRecordedGuildDeletions(worker);
		expect(failing).toHaveBeenCalled();
	});

	test('member teardown runs with bounded concurrency', async () => {
		const {owner, guild} = await guildWithMember();
		await deleteAsOwner(owner, guild.id);
		const guildId: GuildID = createGuildID(BigInt(guild.id));
		const repository = getGuildRepository();
		const realMembers = await repository.listMembers(guildId);
		const extraMembers = Array.from({length: 120}, (_, index) => ({
			...realMembers[0]!,
			userId: createUserID(BigInt(realMembers[0]!.userId) + BigInt(index + 1)),
		}));
		vi.spyOn(Object.getPrototypeOf(repository), 'listMembers').mockResolvedValueOnce([...realMembers, ...extraMembers]);
		let inFlight = 0;
		let peak = 0;
		vi.spyOn(NoopGatewayService.prototype, 'leaveGuild').mockImplementation(async () => {
			inFlight += 1;
			peak = Math.max(peak, inFlight);
			await new Promise((resolve) => setTimeout(resolve, 1));
			inFlight -= 1;
		});
		await createTestGuildStack().guildService.data.operations.purgeGuild(guildId, 'source_deleted');
		expect(peak).toBeGreaterThan(1);
		expect(peak).toBeLessThanOrEqual(25);
		expect(await storedGuild(guild.id)).toBeNull();
	});
});
