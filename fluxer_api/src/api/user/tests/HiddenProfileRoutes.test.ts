// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {decisionBody} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {getMember, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {profilePseudonym} from '@app/api/user/ProfileVisibility';
import {fetchUser, fetchUserMe, fetchUserProfile} from '@app/api/user/tests/UserTestUtils';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const PROFILE_HIDDEN_BIT = Number(UserFlags.PROFILE_HIDDEN);

interface AdminUser {
	username: string;
	global_name: string | null;
	bio: string | null;
	pronouns: string | null;
	pending_deletion_at: string | null;
	enforcement_decision_id: string | null;
}

describe('hidden profiles', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let viewer: TestAccount;
	let target: TestAccount;
	let guildId: string;
	let targetName: string;
	let pseudonym: {username: string; discriminator: string};

	beforeEach(async () => {
		harness = await createApiTestHarness();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'user:lookup',
			'user:temp_ban',
			'user:delete',
			'user:update:flags',
			'enforcement:view',
		]);
		const setup = await setupTestGuildWithMembers(harness, 1);
		viewer = setup.owner;
		target = setup.members[0]!;
		guildId = setup.guild.id;
		await createBuilder(harness, target.token)
			.patch('/users/@me')
			.body({global_name: 'Shown Name', bio: 'shown bio', pronouns: 'they/them'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, target.token)
			.patch(`/guilds/${guildId}/members/@me`)
			.body({nick: 'Shown Nick'})
			.expect(HTTP_STATUS.OK)
			.execute();
		targetName = (await fetchUser(harness, target.userId, viewer.token)).json.username;
		pseudonym = profilePseudonym(target.userId);
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function expectShown(): Promise<void> {
		const {json} = await fetchUser(harness, target.userId, viewer.token);
		expect(json).toMatchObject({username: targetName, global_name: 'Shown Name'});
		expect(json.flags & PROFILE_HIDDEN_BIT).toBe(0);
		const profile = await fetchUserProfile(harness, target.userId, viewer.token);
		expect(profile.json.user_profile).toMatchObject({bio: 'shown bio', pronouns: 'they/them'});
		const member = await getMember(harness, viewer.token, guildId, target.userId);
		expect(member.nick).toBe('Shown Nick');
	}

	async function expectHidden(): Promise<void> {
		const {json} = await fetchUser(harness, target.userId, viewer.token);
		expect(json).toMatchObject({...pseudonym, global_name: null, avatar: null});
		expect(json.username).not.toBe(targetName);
		expect(json.flags & PROFILE_HIDDEN_BIT).toBe(0);
		const profile = await fetchUserProfile(harness, target.userId, viewer.token);
		expect(profile.json.user_profile).toMatchObject({bio: null, pronouns: null, banner: null, accent_color: null});
		const member = await getMember(harness, viewer.token, guildId, target.userId);
		expect(member).toMatchObject({nick: null, avatar: null, banner: null});
		expect(member.user).toMatchObject(pseudonym);
		expect(member.user.flags & PROFILE_HIDDEN_BIT).toBe(0);
	}

	async function expectStaffSeeStoredProfile(): Promise<void> {
		const {users} = await createBuilder<{users: Array<AdminUser>}>(harness, admin.token)
			.get(`/admin/users/${target.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(users[0]).toMatchObject({username: targetName, global_name: 'Shown Name', bio: 'shown bio'});
	}

	test('a staff ban hides the profile until the unban restores it', async () => {
		const {user: suspended} = await createBuilder<{user: {enforcement_decision_id: string}}>(harness, admin.token)
			.put(`/admin/users/${target.userId}/ban`)
			.header('X-Audit-Log-Reason', 'Test suspension')
			.body({
				duration_hours: 24,
				decision: decisionBody(),
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectHidden();
		await expectStaffSeeStoredProfile();
		await createBuilder(harness, admin.token)
			.post(`/admin/enforcement-decisions/${suspended.enforcement_decision_id}/end`)
			.body({outcome: 'lifted'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectShown();
	});

	async function endDecision(decisionId: string) {
		await createBuilder(harness, admin.token)
			.post(`/admin/enforcement-decisions/${decisionId}/end`)
			.header('X-Audit-Log-Reason', 'Checked again')
			.body({outcome: 'reversed'})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	test('a direct message limit hides the profile and ending it restores it', async () => {
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/feature-limits`)
			.header('X-Audit-Log-Reason', 'Spam review')
			.body({limit: 'direct_messages', decision: decisionBody()})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectHidden();
		await expectStaffSeeStoredProfile();
		const {decisions} = await createBuilder<{decisions: Array<{decision_id: string}>}>(harness, admin.token)
			.get(`/admin/enforcement-decisions?subject_type=user&subject_id=${target.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		await endDecision(decisions[0]!.decision_id);
		await expectShown();
	});

	test('a closure for abuse hides the profile and ending it restores it', async () => {
		const {user} = await createBuilder<{user: AdminUser}>(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.header('X-Audit-Log-Reason', 'Hate review')
			.body({
				days_until_deletion: 74,
				basis: {
					type: 'enforcement',
					decision: decisionBody({reason_code: DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT}),
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectHidden();
		await expectStaffSeeStoredProfile();
		await endDecision(user.enforcement_decision_id!);
		await expectShown();
	});

	test('the hidden profile flag hides the profile from others while the owner keeps seeing it', async () => {
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/flags`)
			.body({add_flags: [UserFlags.PROFILE_HIDDEN.toString()]})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectHidden();
		const own = await fetchUserMe(harness, target.token);
		expect(own.json).toMatchObject({username: targetName, global_name: 'Shown Name', bio: 'shown bio'});
		expect(own.json.flags & PROFILE_HIDDEN_BIT).toBe(0);
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/flags`)
			.body({remove_flags: [UserFlags.PROFILE_HIDDEN.toString()]})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectShown();
	});
});
