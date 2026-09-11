// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {
	addMemberRole,
	createGuild,
	createRole,
	setupTestGuildWithMembers,
	updateGuild,
	updateMember,
	updateRolePositions,
} from './GuildTestUtils';

interface AuditLogEntry {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: Record<string, unknown>;
	changes?: Array<{
		key: string;
		old_value?: unknown;
		new_value?: unknown;
	}>;
}

interface AuditLogResponse {
	audit_log_entries: Array<AuditLogEntry>;
	users: Array<{
		id: string;
	}>;
	webhooks: Array<unknown>;
}

describe('Guild Features', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('ADMINISTRATOR Permission Effects', () => {
		test('should allow owner to assign ADMINISTRATOR role to member', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const adminRole = await createRole(harness, owner.token, guild.id, {
				name: 'Admin Role',
				permissions: Permissions.ADMINISTRATOR.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, member.userId, adminRole.id);
			const memberData = await createBuilder<{
				roles: Array<string>;
			}>(harness, owner.token)
				.get(`/guilds/${guild.id}/members/${member.userId}`)
				.execute();
			expect(memberData.roles).toContain(adminRole.id);
		});
	});
	describe('Guild Audit Log Does Not Leak IP Addresses in Ban Entries', () => {
		test('should not include IP address in ban audit log changes', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetMember = members[0];
			await createBuilder(harness, owner.token)
				.put(`/guilds/${guild.id}/bans/${targetMember.userId}`)
				.body({reason: 'Test ban for audit log check'})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const auditLogResponse = await createBuilder<AuditLogResponse>(harness, owner.token)
				.get(`/guilds/${guild.id}/audit-logs?action_type=22`)
				.execute();
			expect(auditLogResponse.audit_log_entries.length).toBeGreaterThan(0);
			for (const entry of auditLogResponse.audit_log_entries) {
				if (entry.changes) {
					for (const change of entry.changes) {
						expect(change.key).not.toBe('ip');
						expect(change.key).not.toBe('ip_address');
					}
				}
				if (entry.options) {
					expect(entry.options).not.toHaveProperty('ip');
					expect(entry.options).not.toHaveProperty('ip_address');
				}
			}
		});
		test('should preserve other ban details in audit log while scrubbing IP', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetMember = members[0];
			const banReason = 'Ban reason for audit test';
			await createBuilder(harness, owner.token)
				.put(`/guilds/${guild.id}/bans/${targetMember.userId}`)
				.body({reason: banReason})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const auditLogResponse = await createBuilder<AuditLogResponse>(harness, owner.token)
				.get(`/guilds/${guild.id}/audit-logs?action_type=22`)
				.execute();
			const banEntry = auditLogResponse.audit_log_entries.find(
				(entry) => entry.target_id === targetMember.userId && entry.action_type === 22,
			);
			expect(banEntry).toBeDefined();
			expect(banEntry!.user_id).toBe(owner.userId);
			expect(banEntry!.target_id).toBe(targetMember.userId);
		});
	});
	describe('CHANGE_NICKNAME Permission Enforcement', () => {
		test('should allow member with CHANGE_NICKNAME to change their own nickname', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({nick: 'My New Nickname'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('should ignore self nickname changes when CHANGE_NICKNAME is denied by @everyone', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/roles/${guild.id}`)
				.body({
					permissions: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
				})
				.execute();
			const updatedMember = await createBuilder<GuildMemberResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({nick: 'Cannot Set This'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(updatedMember.nick ?? null).toBeNull();
		});
		test('should allow MANAGE_NICKNAMES holder to change other member nicknames', async () => {
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
		test('should not allow CHANGE_NICKNAME holder to change other member nicknames', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [member1, member2] = members;
			const changeNickRole = await createRole(harness, owner.token, guild.id, {
				name: 'Change Nick Only',
				permissions: Permissions.CHANGE_NICKNAME.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, member1.userId, changeNickRole.id);
			await createBuilder(harness, member1.token)
				.patch(`/guilds/${guild.id}/members/${member2.userId}`)
				.body({nick: 'Should Fail'})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('Guild Feature Flags Validation', () => {
		test('should allow toggling INVITES_DISABLED feature', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Feature Test Guild');
			const updatedGuild = await updateGuild(harness, account.token, guild.id, {
				features: [...guild.features, GuildFeatures.INVITES_DISABLED],
			});
			expect(updatedGuild.features).toContain(GuildFeatures.INVITES_DISABLED);
			const enabledGuild = await updateGuild(harness, account.token, guild.id, {
				features: updatedGuild.features.filter((f: string) => f !== GuildFeatures.INVITES_DISABLED),
			});
			expect(enabledGuild.features).not.toContain(GuildFeatures.INVITES_DISABLED);
		});
		test('should allow toggling TEXT_CHANNEL_FLEXIBLE_NAMES feature', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Flexible Names Test');
			const updatedGuild = await updateGuild(harness, account.token, guild.id, {
				features: [...guild.features, GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES],
			});
			expect(updatedGuild.features).toContain(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES);
		});
		test('should preserve base features when toggling user-controlled features', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Base Features Test');
			expect(guild.features).toContain(GuildFeatures.ANIMATED_ICON);
			expect(guild.features).toContain(GuildFeatures.BANNER);
			const updatedGuild = await updateGuild(harness, account.token, guild.id, {
				features: [...guild.features, GuildFeatures.INVITES_DISABLED],
			});
			expect(updatedGuild.features).toContain(GuildFeatures.ANIMATED_ICON);
			expect(updatedGuild.features).toContain(GuildFeatures.BANNER);
			expect(updatedGuild.features).toContain(GuildFeatures.INVITES_DISABLED);
		});
		test('should allow toggling CLONE_EMOJI_ENABLED and CLONE_STICKER_ENABLED features', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Clone Opt In Test');
			const updatedGuild = await updateGuild(harness, account.token, guild.id, {
				features: [...guild.features, GuildFeatures.CLONE_EMOJI_ENABLED, GuildFeatures.CLONE_STICKER_ENABLED],
			});
			expect(updatedGuild.features).toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
			expect(updatedGuild.features).toContain(GuildFeatures.CLONE_STICKER_ENABLED);
			const optedOut = await updateGuild(harness, account.token, guild.id, {
				features: updatedGuild.features.filter(
					(feature: string) =>
						feature !== GuildFeatures.CLONE_EMOJI_ENABLED && feature !== GuildFeatures.CLONE_STICKER_ENABLED,
				),
			});
			expect(optedOut.features).not.toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
			expect(optedOut.features).not.toContain(GuildFeatures.CLONE_STICKER_ENABLED);
		});
		test('should reject toggling the deprecated CLONE_EMOJI_DISABLED feature', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Deprecated Clone Emoji Test');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}`)
				.body({features: [...guild.features, GuildFeatures.CLONE_EMOJI_DISABLED]})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject toggling the deprecated CLONE_STICKER_DISABLED feature', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Deprecated Clone Sticker Test');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}`)
				.body({features: [...guild.features, GuildFeatures.CLONE_STICKER_DISABLED]})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject toggling non-toggleable features', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Non Toggleable Feature Test');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}`)
				.body({features: [...guild.features, GuildFeatures.DISCOVERABLE]})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should require MANAGE_GUILD permission to update features', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}`)
				.body({features: [...guild.features, GuildFeatures.INVITES_DISABLED]})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('Role Hierarchy With ADMINISTRATOR', () => {
		test('should not allow non-owner ADMINISTRATOR to modify roles above their highest role', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const adminMember = members[0];
			const highRole = await createRole(harness, owner.token, guild.id, {
				name: 'High Role',
				permissions: '0',
			});
			const adminRole = await createRole(harness, owner.token, guild.id, {
				name: 'Admin Role',
				permissions: Permissions.ADMINISTRATOR.toString(),
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 3},
				{id: adminRole.id, position: 2},
			]);
			await addMemberRole(harness, owner.token, guild.id, adminMember.userId, adminRole.id);
			await createBuilder(harness, adminMember.token)
				.patch(`/guilds/${guild.id}/roles/${highRole.id}`)
				.body({name: 'Trying to Modify'})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should allow owner to modify any role regardless of hierarchy', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Owner Hierarchy Test');
			const highRole = await createRole(harness, account.token, guild.id, {
				name: 'High Role',
				permissions: Permissions.ADMINISTRATOR.toString(),
			});
			await updateRolePositions(harness, account.token, guild.id, [{id: highRole.id, position: 10}]);
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/roles/${highRole.id}`)
				.body({name: 'Modified By Owner'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});
});
