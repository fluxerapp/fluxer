// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	listTestEmails,
	setUserACLs,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {createChannelInvite, createGuild, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {getAdminRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {setPendingDeletionAt, triggerDeletionWorker} from '@app/api/user/tests/UserTestUtils';
import {isDeletionComplete} from '@app/api/user/UserHelpers';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
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
			.expect(HTTP_STATUS.NOT_FOUND)
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

	it('emails the applicant when the registration is approved, and not when it is rejected', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const approvedBody = invitedRegistrationBody('approvedmail');
		const rejectedBody = invitedRegistrationBody('rejectedmail');
		const approved = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(approvedBody)
			.execute();
		const rejected = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(rejectedBody)
			.execute();
		await clearTestEmails(harness);

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${approved.user_id}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${rejected.user_id}`)
			.body({status: 'rejected'})
			.expect(HTTP_STATUS.OK)
			.execute();

		const approvedEmails = await listTestEmails(harness, {recipient: approvedBody.email as string});
		expect(approvedEmails.map((email) => email.type)).toEqual(['registration_approved']);
		expect(await listTestEmails(harness, {recipient: rejectedBody.email as string})).toEqual([]);
	});

	it('deletes the account on rejection, records it, and lets the email register again', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const body = invitedRegistrationBody('rejecteddeleted');
		const rejected = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(body)
			.execute();
		await clearTestEmails(harness);

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${rejected.user_id}`)
			.body({status: 'rejected'})
			.expect(HTTP_STATUS.OK)
			.execute();

		const users = createApiContext().services.users;
		const scheduled = await users.findUniqueAssert(createUserID(BigInt(rejected.user_id)));
		expect(scheduled.pendingDeletionAt!.getTime()).toBeLessThanOrEqual(Date.now());
		const audit = (await getAdminRepository().listAllAuditLogsPaginated(1000)).filter(
			(log) => log.action === 'reject_registration' && log.targetId.toString() === rejected.user_id,
		);
		expect(audit.map((log) => Object.fromEntries(log.metadata))).toEqual([
			{pending_deletion_at: scheduled.pendingDeletionAt!.toISOString()},
		]);
		const {json} = await createBuilderWithoutAuth<{errors: Array<{path: string; code: string}>}>(harness)
			.post('/auth/register')
			.body({...body, username: createUniqueUsername('rejectedagain')})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors).toEqual([
			expect.objectContaining({path: 'email', code: ValidationErrorCodes.EMAIL_ALREADY_IN_USE}),
		]);

		await triggerDeletionWorker(harness);

		expect(isDeletionComplete(await users.findUniqueAssert(createUserID(BigInt(rejected.user_id))))).toBe(true);
		expect(await listTestEmails(harness, {recipient: body.email as string})).toEqual([]);
		const again = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body({...body, username: createUniqueUsername('rejectedagain')})
			.execute();
		expect(again.user_id).not.toBe(rejected.user_id);
		const listed = await createBuilder<PendingRegistrationListResponse>(harness, admin.token)
			.get('/admin/instance/pending-registrations')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(listed.pending_registrations.map((entry) => entry.user_id)).toEqual([again.user_id]);
	});

	it('leaves an account that was not waiting untouched', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		const outsider = await createTestAccount(harness);
		await clearTestEmails(harness);

		for (const status of ['approved', 'rejected'] as const) {
			await createBuilder(harness, admin.token)
				.patch(`/admin/instance/pending-registrations/${outsider.userId}`)
				.body({status})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		}

		expect(await listTestEmails(harness, {recipient: outsider.email})).toEqual([]);
		await createBuilder(harness, outsider.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
	});

	it('drops the pending registration when the account is deleted', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body(invitedRegistrationBody('deletedpending'))
			.execute();

		await setPendingDeletionAt(harness, pending.user_id, new Date(Date.now() - 1000));
		await triggerDeletionWorker(harness);

		const listed = await createBuilder<PendingRegistrationListResponse>(harness, admin.token)
			.get('/admin/instance/pending-registrations')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(listed.pending_registrations.map((entry) => entry.user_id)).not.toContain(pending.user_id);
	});
});
