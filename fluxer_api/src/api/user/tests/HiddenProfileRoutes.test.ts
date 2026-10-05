// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getMember, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {profilePseudonym} from '@app/api/user/ProfileVisibility';
import {fetchUser, fetchUserMe, fetchUserProfile} from '@app/api/user/tests/UserTestUtils';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const PROFILE_HIDDEN_BIT = Number(UserFlags.PROFILE_HIDDEN);

interface AdminUser {
	username: string;
	global_name: string | null;
	bio: string | null;
	pronouns: string | null;
	pending_deletion_at: string | null;
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
		vi.restoreAllMocks();
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

	function memberUpdates(): Array<GuildMemberResponse> {
		const dispatch = vi.mocked(NoopGatewayService.prototype.dispatchGuild);
		return dispatch.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'GUILD_MEMBER_UPDATE' && params.guildId.toString() === guildId)
			.map((params) => params.data as GuildMemberResponse)
			.filter((member) => member.user.id === target.userId);
	}

	test('a normal account shows its stored profile', async () => {
		await expectShown();
	});

	test('a staff ban hides the profile until the unban restores it, and clients are told both ways', async () => {
		vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const presence = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/ban`)
			.body({duration_hours: 24, notify_user: false})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectHidden();
		await expectStaffSeeStoredProfile();
		expect(memberUpdates().map((member) => [member.user.username, member.user.discriminator, member.nick])).toEqual([
			[pseudonym.username, pseudonym.discriminator, null],
		]);
		const ownUpdates = presence.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'USER_UPDATE' && params.userId.toString() === target.userId)
			.map((params) => params.data as {username: string});
		expect(ownUpdates.length).toBeGreaterThan(0);
		expect(ownUpdates.every((update) => update.username === targetName)).toBe(true);
		await createBuilder(harness, admin.token)
			.delete(`/admin/users/${target.userId}/ban`)
			.body({notify_user: false})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectShown();
		expect(memberUpdates().map((member) => [member.user.username, member.nick])).toEqual([
			[pseudonym.username, null],
			[targetName, 'Shown Nick'],
		]);
	});

	test('the spammer flag hides the profile and clearing it restores it', async () => {
		const flags = (body: Record<string, Array<string>>) =>
			createBuilder(harness, admin.token)
				.patch(`/admin/users/${target.userId}/flags`)
				.body(body)
				.expect(HTTP_STATUS.OK)
				.execute();
		await flags({add_flags: [UserFlags.SPAMMER.toString()]});
		await expectHidden();
		await expectStaffSeeStoredProfile();
		await flags({remove_flags: [UserFlags.SPAMMER.toString()]});
		await expectShown();
	});

	test('a pending deletion for abuse hides the profile and cancelling it restores it', async () => {
		const {user} = await createBuilder<{user: AdminUser}>(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.body({
				reason_code: DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT,
				days_until_deletion: 30,
				notify_user: false,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await expectHidden();
		await expectStaffSeeStoredProfile();
		await createBuilder(harness, admin.token)
			.delete(`/admin/users/${target.userId}/deletion`)
			.body({expected_pending_deletion_at: user.pending_deletion_at})
			.expect(HTTP_STATUS.OK)
			.execute();
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
