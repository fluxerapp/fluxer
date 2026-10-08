// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {addMemberRole, createGuild, createRole, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {Event} from '@app/api/infrastructure/activity/Contract.generated';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

class CapturingPublisher implements ActivityPublisher {
	readonly events: Array<Event> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.events.push(JSON.parse(payload) as Event);
	}

	of<K extends Event['kind']>(kind: K): Array<Extract<Event, {kind: K}>> {
		return this.events.filter((event): event is Extract<Event, {kind: K}> => event.kind === kind);
	}
}

describe('moderation activity events', () => {
	let harness: ApiTestHarness;
	let publisher: CapturingPublisher;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
	});

	afterEach(async () => {
		resetActivityEventsForTests();
		await harness?.shutdown();
	});

	async function ban(token: string, path: string): Promise<void> {
		await createBuilder(harness, token).put(path).body({}).expect(HTTP_STATUS.NO_CONTENT).execute();
	}

	test('a moderator ban and unban publish facts about the target without addresses', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		await ban(owner.token, `/guilds/${guild.id}/bans/${target.userId}`);
		const [banned] = publisher.of('guild_member_banned');
		expect(banned?.key).toBe(target.userId);
		expect(banned?.data).toEqual({
			guild_id: guild.id,
			user_id: target.userId,
			moderator_id: owner.userId,
			by: 'moderator',
			guild_member_count: 2,
			target_moderator: false,
			expires_at_ms: null,
		});
		expect(banned?.meta).toMatchObject({ip: null, country: null, ua: null, locale: null});
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/bans/${target.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const [unbanned] = publisher.of('guild_member_unbanned');
		expect(unbanned?.data).toEqual({
			guild_id: guild.id,
			user_id: target.userId,
			moderator_id: owner.userId,
			by: 'moderator',
		});
		expect(unbanned?.meta.ip).toBeNull();
	});

	test('banning a member who can moderate the guild marks the target as a moderator', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Mods',
			permissions: Permissions.BAN_MEMBERS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, target.userId, role.id);
		await ban(owner.token, `/guilds/${guild.id}/bans/${target.userId}`);
		expect(publisher.of('guild_member_banned')[0]?.data.target_moderator).toBe(true);
	});

	test('a staff ban says it came from staff', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'guild:ban_member',
		]);
		const target = await createTestAccount(harness);
		const guild = await createGuild(harness, admin.token, 'Staff ban guild');
		await ban(admin.token, `/admin/guilds/${guild.id}/bans/${target.userId}`);
		expect(publisher.of('guild_member_banned')[0]?.data).toMatchObject({
			user_id: target.userId,
			by: 'staff',
			target_moderator: false,
		});
	});

	describe('report resolution', () => {
		let admin: TestAccount;

		beforeEach(async () => {
			admin = await setUserACLs(harness, await createTestAccount(harness), [
				'admin:authenticate',
				'report:resolve',
				'user:temp_ban',
			]);
		});

		async function fileReport(): Promise<{reporter: TestAccount; reported: TestAccount; reportId: string}> {
			const reporter = await createTestAccount(harness);
			const reported = await createTestAccount(harness);
			const report = await createBuilder<{report_id: string}>(harness, reporter.token)
				.post('/reports/user')
				.body({user_id: reported.userId, category: 'harassment'})
				.execute();
			return {reporter, reported, reportId: report.report_id};
		}

		async function resolve(reportId: string, body: Record<string, unknown>): Promise<void> {
			await createBuilder(harness, admin.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', notify_reporter: false, ...body})
				.expect(HTTP_STATUS.OK)
				.execute();
		}

		test('a staff dismissal publishes the outcome staff chose', async () => {
			const {reporter, reported, reportId} = await fileReport();
			await resolve(reportId, {resolution: 'no_violation'});
			const [resolved] = publisher.of('report_resolved');
			expect(resolved?.key).toBe(reported.userId);
			expect(resolved?.data).toEqual({
				report_id: reportId,
				reporter_id: reporter.userId,
				category: 'harassment',
				target_type: 'user',
				reported_user_id: reported.userId,
				outcome: 'no_violation',
				resolved_by: 'staff',
			});
			expect(resolved?.meta.ip).toBeNull();
		});

		test.each(['actioned', 'no_violation', 'duplicate'] as const)(
			'a staff resolve with %s publishes that outcome even when the account is under enforcement',
			async (resolution) => {
				const {reported, reportId} = await fileReport();
				await createBuilder(harness, admin.token)
					.put(`/admin/users/${reported.userId}/ban`)
					.body({duration_hours: 24, notify_user: false})
					.expect(HTTP_STATUS.OK)
					.execute();
				await resolve(reportId, {resolution});
				expect(publisher.of('report_resolved').map((event) => event.data)).toEqual([
					expect.objectContaining({report_id: reportId, outcome: resolution, resolved_by: 'staff'}),
				]);
			},
		);

		test('without a chosen outcome the reported account state decides between actioned and unspecified', async () => {
			const first = await fileReport();
			await resolve(first.reportId, {});
			const second = await fileReport();
			await createBuilder(harness, admin.token)
				.put(`/admin/users/${second.reported.userId}/ban`)
				.body({duration_hours: 24, notify_user: false})
				.expect(HTTP_STATUS.OK)
				.execute();
			await resolve(second.reportId, {});
			expect(publisher.of('report_resolved').map((event) => event.data.outcome)).toEqual(['unspecified', 'actioned']);
		});

		test('an unknown resolution is rejected', async () => {
			const {reportId} = await fileReport();
			await createBuilder(harness, admin.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', resolution: 'maybe'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(publisher.of('report_resolved')).toEqual([]);
		});
	});
});
