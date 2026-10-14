// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	setUserACLs,
} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {createChannelInvite, createGuild, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {PendingRegistrationListResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {AdminUsersMeResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface PendingRegistrationResponse {
	user_id: string;
}

function invitedRegistrationBody(prefix: string, inviteCode?: string): Record<string, unknown> {
	return {
		email: createUniqueEmail(prefix),
		username: createUniqueUsername(prefix),
		global_name: 'Invited person',
		password: 'invited-since-1999',
		date_of_birth: '2000-01-01',
		consent: true,
		invite_code: inviteCode,
	};
}

describe('pending registration approval and the stock community', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('keeps a banned user out of the stock community on approval', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		await getInstanceConfigRepository().setInstancePolicyConfig({
			single_community_enabled: true,
			single_community_guild_id: guild.id,
		});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('bannedapproval'),
				username: createUniqueUsername('bannedapproval'),
				global_name: 'The banned man',
				password: 'approving-since-1999',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();

		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${pending.user_id}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('does not add a user who was never pending to the stock community', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const outsider = await createTestAccount(harness);
		await getInstanceConfigRepository().setInstancePolicyConfig({
			single_community_enabled: true,
			single_community_guild_id: guild.id,
		});

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${outsider.userId}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${outsider.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('joins the invited community on approval', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(invitedRegistrationBody('invitedapproval', invite.code))
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	});

	it('lists the invite code on a pending registration', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(invitedRegistrationBody('invitedlisted', invite.code))
			.execute();

		const listed = await createBuilder<PendingRegistrationListResponse>(harness, admin.token)
			.get('/admin/instance/pending-registrations')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(listed.pending_registrations).toEqual([
			expect.objectContaining({user_id: pending.user_id, invite_code: invite.code}),
		]);
	});

	it('does not join the invited community on rejection', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(invitedRegistrationBody('invitedrejected', invite.code))
			.execute();

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
			.body({status: 'rejected'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('joins the auto-join community on approval of a registration without an invite', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const originalAutoJoinInviteCode = Config.instance.autoJoinInviteCode;
		Config.instance.autoJoinInviteCode = invite.code;
		try {
			const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
				.post('/auth/register')
				.body(invitedRegistrationBody('autojoinapproval'))
				.execute();

			const listed = await createBuilder<PendingRegistrationListResponse>(harness, admin.token)
				.get('/admin/instance/pending-registrations')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(listed.pending_registrations).toEqual([
				expect.objectContaining({user_id: pending.user_id, invite_code: invite.code}),
			]);

			await createBuilder(harness, admin.token)
				.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();

			await createBuilder(harness, owner.token)
				.get(`/guilds/${guild.id}/members/${pending.user_id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
		} finally {
			Config.instance.autoJoinInviteCode = originalAutoJoinInviteCode;
		}
	});

	it('does not join the auto-join community when the pending entry has no invite', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(invitedRegistrationBody('noinviteapproval'))
			.execute();

		const originalAutoJoinInviteCode = Config.instance.autoJoinInviteCode;
		Config.instance.autoJoinInviteCode = invite.code;
		try {
			await createBuilder(harness, admin.token)
				.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
		} finally {
			Config.instance.autoJoinInviteCode = originalAutoJoinInviteCode;
		}

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('approves and joins the single community when the invite is gone', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const {owner, guild: stockGuild} = await setupTestGuildWithMembers(harness, 0);
		const invitedGuild = await createGuild(harness, owner.token, 'Invited Guild');
		const invite = await createChannelInvite(harness, owner.token, invitedGuild.system_channel_id!);
		await getInstanceConfigRepository().setInstancePolicyConfig({
			single_community_enabled: true,
			single_community_guild_id: stockGuild.id,
		});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(invitedRegistrationBody('invitegone', invite.code))
			.execute();

		await createBuilder(harness, owner.token)
			.delete(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${stockGuild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, owner.token)
			.get(`/guilds/${invitedGuild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('lets the review ACL alone list and decide registrations', async () => {
		const reviewer = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const configAdmin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('reviewonly'),
				username: createUniqueUsername('reviewonly'),
				global_name: 'Review only',
				password: 'reviewing-since-1999',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();

		const listed = await createBuilder<PendingRegistrationListResponse>(harness, reviewer.token)
			.get('/admin/instance/pending-registrations')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(listed.pending_registrations.map((entry) => entry.user_id)).toEqual([pending.user_id]);
		const reviewerMe = await createBuilder<AdminUsersMeResponse>(harness, reviewer.token)
			.get('/admin/users/@me')
			.execute();
		expect(reviewerMe.pending_registration_count).toBe(1);
		const configAdminMe = await createBuilder<AdminUsersMeResponse>(harness, configAdmin.token)
			.get('/admin/users/@me')
			.execute();
		expect(configAdminMe.pending_registration_count).toBeNull();

		await createBuilder(harness, configAdmin.token)
			.get('/admin/instance/pending-registrations')
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
		await createBuilder(harness, configAdmin.token)
			.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();

		const decided = await createBuilder<PendingRegistrationListResponse>(harness, reviewer.token)
			.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
			.body({status: 'rejected'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(decided.pending_registrations).toEqual([]);
	});
});
