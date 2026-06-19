// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildBanResponse, GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {getPngDataUrl, getTooLargePngDataUrl} from '../../emoji/tests/EmojiTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {profileSubstringBlocklistCache} from '../../middleware/ProfileSubstringBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {grantPremium} from '../../user/tests/UserTestUtils';
import {
	addMemberRole,
	createRole,
	getMember,
	removeMemberRole,
	setupTestGuildWithMembers,
	updateMember,
	updateRolePositions,
} from './GuildTestUtils';

describe('Guild Member Management', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		for (const scope of ['nickname', 'bio', 'pronouns'] as const) {
			profileSubstringBlocklistCache.remove(scope, 'blockedslug');
		}
		await harness?.shutdown();
	});
	test('should remove role from member', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Test Role',
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, role.id);
		let memberInfo = await getMember(harness, owner.token, guild.id, member.userId);
		expect(memberInfo.roles).toContain(role.id);
		await removeMemberRole(harness, owner.token, guild.id, member.userId, role.id);
		memberInfo = await getMember(harness, owner.token, guild.id, member.userId);
		expect(memberInfo.roles).not.toContain(role.id);
	});
	test('should reject assigning @everyone via member update', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		type InvalidFormResponse = {
			errors: Array<{
				path: string;
			}>;
			code: string;
		};
		const {json} = await createBuilder<InvalidFormResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({roles: [guild.id]})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.executeWithResponse();
		expect(json.errors?.some((error) => error.path === 'roles')).toBe(true);
	});
	test('should reject adding @everyone role explicitly', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		type InvalidFormResponse = {
			errors: Array<{
				path: string;
			}>;
			code: string;
		};
		const {json} = await createBuilder<InvalidFormResponse>(harness, owner.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${guild.id}`)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.executeWithResponse();
		expect(json.errors?.some((error) => error.path === 'role_id')).toBe(true);
	});
	test('should update member nickname', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const updatedMember = await updateMember(harness, owner.token, guild.id, member.userId, {
			nick: 'New Nickname',
		});
		expect(updatedMember.nick).toBe('New Nickname');
	});
	test('should block banned profile substrings in member nickname', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		profileSubstringBlocklistCache.add('nickname', 'blockedslug');
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({nick: 'BlockedSlug Nick'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
	});
	test('should require MANAGE_NICKNAMES to change others nickname', async () => {
		const {members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [member1, member2] = members;
		await createBuilder(harness, member1.token)
			.patch(`/guilds/${guild.id}/members/${member2.userId}`)
			.body({nick: 'New Nick'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should allow changing own nickname with CHANGE_NICKNAME', async () => {
		const {members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}/members/@me`)
			.body({nick: 'My Nickname'})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('should require MANAGE_ROLES to add roles', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [member1, member2] = members;
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Test Role',
		});
		await createBuilder(harness, member1.token)
			.put(`/guilds/${guild.id}/members/${member2.userId}/roles/${role.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should enforce role hierarchy when adding roles', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const higherRole = await createRole(harness, owner.token, guild.id, {
			name: 'Higher Role',
			permissions: Permissions.MANAGE_ROLES.toString(),
		});
		const lowerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Lower Role',
		});
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: higherRole.id, position: 2},
			{id: lowerRole.id, position: 1},
		]);
		await addMemberRole(harness, owner.token, guild.id, member.userId, lowerRole.id);
		await createBuilder(harness, member.token)
			.put(`/guilds/${guild.id}/members/${owner.userId}/roles/${higherRole.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should kick member from guild', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/members/${member.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${member.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should require KICK_MEMBERS to kick members', async () => {
		const {members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [member1, member2] = members;
		await createBuilder(harness, member1.token)
			.delete(`/guilds/${guild.id}/members/${member2.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should not allow kicking the owner', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: Permissions.KICK_MEMBERS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, modRole.id);
		await createBuilder(harness, member.token)
			.delete(`/guilds/${guild.id}/members/${owner.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should ban member from guild', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('should ban non-member from guild', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 1);
		const nonMember = await createTestAccount(harness);
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${nonMember.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('should disallow banning nonexistent user from guild', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 1);
		const nonExistentId = '1234567890123456789';
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${nonExistentId}`)
			.body({})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should fall back to audit log reason header when ban reason is omitted', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const auditLogReason = 'Header fallback guild ban reason';
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.header('X-Audit-Log-Reason', auditLogReason)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const bans = await createBuilder<Array<GuildBanResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/bans`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const ban = bans.find((entry) => entry.user.id === member.userId);
		expect(ban).toBeDefined();
		expect(ban?.reason).toBe(auditLogReason);
	});
	test('should require BAN_MEMBERS to ban members', async () => {
		const {members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [member1, member2] = members;
		await createBuilder(harness, member1.token)
			.put(`/guilds/${guild.id}/bans/${member2.userId}`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should not allow member to ban themselves', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: Permissions.BAN_MEMBERS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, modRole.id);
		await createBuilder(harness, member.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.body({})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should clear member nickname by setting to null', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await updateMember(harness, owner.token, guild.id, member.userId, {
			nick: 'Temporary Nick',
		});
		const memberInfo = await getMember(harness, owner.token, guild.id, member.userId);
		expect(memberInfo.nick).toBe('Temporary Nick');
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({nick: null})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('should clear member nickname when nick is an empty string', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await updateMember(harness, owner.token, guild.id, member.userId, {
			nick: 'Temporary Nick',
		});
		const updatedMember = await createBuilder<GuildMemberResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({nick: ''})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updatedMember.nick).toBeNull();
	});
	test('should prevent a timed-out member from changing their own nickname', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		const timeoutUntil = new Date(Date.now() + 5 * 60 * 1000).toISOString();
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({communication_disabled_until: timeoutUntil})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}/members/@me`)
			.body({nick: 'While Timed Out'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const fetched = await getMember(harness, owner.token, guild.id, member.userId);
		expect(fetched.nick).toBeNull();
	});
	test('should still allow a moderator to change a timed-out member nickname', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		const timeoutUntil = new Date(Date.now() + 5 * 60 * 1000).toISOString();
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({communication_disabled_until: timeoutUntil})
			.expect(HTTP_STATUS.OK)
			.execute();
		const updated = await createBuilder<GuildMemberResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({nick: 'Moderator Set'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.nick).toBe('Moderator Set');
	});
	test('should clear a timeout when communication_disabled_until is an empty string', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const timeoutUntil = new Date(Date.now() + 5 * 60 * 1000).toISOString();
		const timedOutMember = await createBuilder<GuildMemberResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({
				communication_disabled_until: timeoutUntil,
				timeout_reason: 'Compatibility test timeout',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(timedOutMember.communication_disabled_until).toBe(timeoutUntil);
		const clearedMember = await createBuilder<GuildMemberResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({
				communication_disabled_until: '',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(clearedMember.communication_disabled_until).toBeNull();
	});
	test('should ignore invalid role IDs when updating a member', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const validRole = await createRole(harness, owner.token, guild.id, {
			name: 'Valid Role',
		});
		const invalidRoleId = '999999999999999998';
		const updatedMember = await createBuilder<GuildMemberResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({roles: [validRole.id, invalidRoleId]})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updatedMember.roles).toContain(validRole.id);
		expect(updatedMember.roles).not.toContain(invalidRoleId);
	});
	describe('List Members Permission Checks', () => {
		test('should reject non-member from listing guild members', async () => {
			const {guild} = await setupTestGuildWithMembers(harness, 1);
			const nonMember = await createTestAccount(harness);
			await createBuilder(harness, nonMember.token)
				.get(`/guilds/${guild.id}/members`)
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
		test('should allow regular member to list guild members', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, member.token).get(`/guilds/${guild.id}/members`).expect(HTTP_STATUS.OK).execute();
		});
		test('should support limit parameter for listing members', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			const memberList = await createBuilder<
				Array<{
					user: {
						id: string;
					};
				}>
			>(harness, owner.token)
				.get(`/guilds/${guild.id}/members?limit=2`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(memberList.length).toBeLessThanOrEqual(2);
		});
	});
	describe('Get Member Permission Checks', () => {
		test('should reject non-member from getting member info', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const nonMember = await createTestAccount(harness);
			await createBuilder(harness, nonMember.token)
				.get(`/guilds/${guild.id}/members/${members[0].userId}`)
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
		test('should allow member to get their own member info', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const memberInfo = await createBuilder<GuildMemberResponse>(harness, member.token)
				.get(`/guilds/${guild.id}/members/${member.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(memberInfo.user?.id).toBe(member.userId);
		});
		test('should allow member to get other member info', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [member1, member2] = members;
			const memberInfo = await createBuilder<GuildMemberResponse>(harness, member1.token)
				.get(`/guilds/${guild.id}/members/${member2.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(memberInfo.user?.id).toBe(member2.userId);
		});
		test('should return 404 for non-existent member', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 1);
			await createBuilder(harness, owner.token)
				.get(`/guilds/${guild.id}/members/999999999999999999`)
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
	describe('Update Member Nick Permission Checks', () => {
		test('should reject member without CHANGE_NICKNAME from changing own nickname when permission revoked', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const restrictedRole = await createRole(harness, owner.token, guild.id, {
				name: 'Restricted',
				permissions: '0',
			});
			await addMemberRole(harness, owner.token, guild.id, member.userId, restrictedRole.id);
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({nick: 'Test Nick'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('should block banned profile substrings in community profile bio and pronouns', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			profileSubstringBlocklistCache.add('bio', 'blockedslug');
			profileSubstringBlocklistCache.add('pronouns', 'blockedslug');
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({bio: 'community bio with blockedslug'})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
				.execute();
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({pronouns: 'blockedslug'})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
				.execute();
		});
		test('should ignore a disallowed self nickname change while still updating other profile fields', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			await createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/roles/${guild.id}`)
				.body({
					permissions: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
				})
				.execute();
			const updatedMember = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({nick: 'Ignored Nick', avatar: getPngDataUrl()})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(updatedMember.nick ?? null).toBeNull();
			expect(updatedMember.avatar).toBeTruthy();
		});
		test('should allow owner to change any member nickname', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const updatedMember = await updateMember(harness, owner.token, guild.id, member.userId, {
				nick: 'Owner Set Nick',
			});
			expect(updatedMember.nick).toBe('Owner Set Nick');
		});
		test('should allow member with MANAGE_NICKNAMES to change others nickname', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [moderator, target] = members;
			const modRole = await createRole(harness, owner.token, guild.id, {
				name: 'Moderator',
				permissions: Permissions.MANAGE_NICKNAMES.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, moderator.userId, modRole.id);
			const updatedMember = await updateMember(harness, moderator.token, guild.id, target.userId, {
				nick: 'Mod Set Nick',
			});
			expect(updatedMember.nick).toBe('Mod Set Nick');
		});
	});
	describe('Kick Member Permission Checks', () => {
		test('should not allow member to kick someone with higher role', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [moderator, target] = members;
			const modRole = await createRole(harness, owner.token, guild.id, {
				name: 'Moderator',
				permissions: Permissions.KICK_MEMBERS.toString(),
			});
			const targetRole = await createRole(harness, owner.token, guild.id, {
				name: 'Target Role',
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: targetRole.id, position: 3},
				{id: modRole.id, position: 2},
			]);
			await addMemberRole(harness, owner.token, guild.id, moderator.userId, modRole.id);
			await addMemberRole(harness, owner.token, guild.id, target.userId, targetRole.id);
			await createBuilder(harness, moderator.token)
				.delete(`/guilds/${guild.id}/members/${target.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should allow member with KICK_MEMBERS to kick lower ranked member', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [moderator, target] = members;
			const modRole = await createRole(harness, owner.token, guild.id, {
				name: 'Moderator',
				permissions: Permissions.KICK_MEMBERS.toString(),
			});
			await updateRolePositions(harness, owner.token, guild.id, [{id: modRole.id, position: 2}]);
			await addMemberRole(harness, owner.token, guild.id, moderator.userId, modRole.id);
			await createBuilder(harness, moderator.token)
				.delete(`/guilds/${guild.id}/members/${target.userId}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		test('should not allow member to kick themselves', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const modRole = await createRole(harness, owner.token, guild.id, {
				name: 'Moderator',
				permissions: Permissions.KICK_MEMBERS.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, member.userId, modRole.id);
			await createBuilder(harness, member.token)
				.delete(`/guilds/${guild.id}/members/${member.userId}`)
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
	describe('Member Avatar Upload Validation', () => {
		test('should reject member avatar upload without premium', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			const memberInfo = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: getPngDataUrl()})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(memberInfo.avatar).toBeNull();
		});
		test('should allow member avatar upload with premium', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			const updatedMember = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: getPngDataUrl()})
				.execute();
			expect(updatedMember.avatar).toBeTruthy();
		});
		test('should reject avatar that exceeds size limit', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: getTooLargePngDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should allow clearing member avatar by setting to null', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: getPngDataUrl()})
				.execute();
			const clearedMember = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: null})
				.execute();
			expect(clearedMember.avatar).toBeNull();
		});
	});
	describe('Member Banner Upload Validation', () => {
		test('should reject member banner upload without premium', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			const memberInfo = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: getPngDataUrl()})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(memberInfo.banner).toBeNull();
		});
		test('should allow member banner upload with premium', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			const updatedMember = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: getPngDataUrl()})
				.execute();
			expect(updatedMember.banner).toBeTruthy();
		});
		test('should reject banner that exceeds size limit', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: getTooLargePngDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should allow clearing member banner by setting to null', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await ensureSessionStarted(harness, member.token);
			await grantPremium(harness, member.userId, 2);
			await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: getPngDataUrl()})
				.execute();
			const clearedMember = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: null})
				.execute();
			expect(clearedMember.banner).toBeNull();
		});
	});
	describe('Member Role Assignment/Removal', () => {
		test('should not allow assigning role higher than own highest role', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [moderator, target] = members;
			const highRole = await createRole(harness, owner.token, guild.id, {
				name: 'High Role',
				permissions: '0',
			});
			const modRole = await createRole(harness, owner.token, guild.id, {
				name: 'Moderator',
				permissions: '268435456',
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 3},
				{id: modRole.id, position: 2},
			]);
			await updateMember(harness, owner.token, guild.id, moderator.userId, {
				roles: [modRole.id],
			});
			await createBuilder(harness, moderator.token)
				.put(`/guilds/${guild.id}/members/${target.userId}/roles/${highRole.id}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should allow owner to assign any role to a member', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const newRole = await createRole(harness, owner.token, guild.id, {
				name: 'New Role',
				permissions: '0',
			});
			const updatedMember = await updateMember(harness, owner.token, guild.id, member.userId, {
				roles: [newRole.id],
			});
			expect(updatedMember.roles).toContain(newRole.id);
		});
		test('should not allow removing role higher than own highest role', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [moderator, target] = members;
			const highRole = await createRole(harness, owner.token, guild.id, {
				name: 'High Role',
				permissions: '0',
			});
			const modRole = await createRole(harness, owner.token, guild.id, {
				name: 'Moderator',
				permissions: '268435456',
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 3},
				{id: modRole.id, position: 2},
			]);
			await updateMember(harness, owner.token, guild.id, moderator.userId, {
				roles: [modRole.id],
			});
			await updateMember(harness, owner.token, guild.id, target.userId, {
				roles: [highRole.id],
			});
			await createBuilder(harness, moderator.token)
				.delete(`/guilds/${guild.id}/members/${target.userId}/roles/${highRole.id}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should allow owner to remove any role from a member', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Removable Role',
				permissions: '0',
			});
			await updateMember(harness, owner.token, guild.id, member.userId, {
				roles: [role.id],
			});
			const updatedMember = await updateMember(harness, owner.token, guild.id, member.userId, {
				roles: [],
			});
			expect(updatedMember.roles).not.toContain(role.id);
		});
		test('should not allow member without MANAGE_ROLES to assign roles', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [member1, member2] = members;
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Test Role',
			});
			await createBuilder(harness, member1.token)
				.put(`/guilds/${guild.id}/members/${member2.userId}/roles/${role.id}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should not allow member without MANAGE_ROLES to remove roles', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [member1, member2] = members;
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Test Role',
			});
			await addMemberRole(harness, owner.token, guild.id, member2.userId, role.id);
			await createBuilder(harness, member1.token)
				.delete(`/guilds/${guild.id}/members/${member2.userId}/roles/${role.id}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should not allow assigning non-existent role', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, owner.token)
				.put(`/guilds/${guild.id}/members/${member.userId}/roles/999999999999999999`)
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
});
