// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGuild,
	createPermissionOverwrite,
	getChannel,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '../../channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {ReadStateRepository} from '../../read_state/ReadStateRepository';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {ReportRepository} from '../ReportRepository';

interface ReportResponse {
	report_id: string;
	status: string;
	reported_at: string;
}

interface PrivateChannelResponse {
	id: string;
	recipients?: Array<{
		id: string;
	}>;
}

interface DmMessageResponse {
	id: string;
	content: string | null;
}

async function listSystemDmChannels(harness: ApiTestHarness, token: string): Promise<Array<PrivateChannelResponse>> {
	const channels = await createBuilder<Array<PrivateChannelResponse>>(harness, token)
		.get('/users/@me/channels')
		.expect(HTTP_STATUS.OK)
		.execute();
	return channels.filter((channel) => channel.recipients?.some((recipient) => recipient.id === '0'));
}

async function listSystemDmMessages(harness: ApiTestHarness, token: string): Promise<Array<DmMessageResponse>> {
	const channels = await listSystemDmChannels(harness, token);
	const messagesByChannel = await Promise.all(
		channels.map((channel) =>
			createBuilder<Array<DmMessageResponse>>(harness, token)
				.get(`/channels/${channel.id}/messages?limit=50`)
				.expect(HTTP_STATUS.OK)
				.execute(),
		),
	);
	return messagesByChannel.flat();
}

async function countReports(): Promise<number> {
	return (await new ReportRepository().listAllReportsPaginated(100)).length;
}

describe('Content Reporting', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Report User', () => {
		test('should report a user with valid category', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
			expect(result.status).toBe('pending');
			expect(result.reported_at).toBeTruthy();
		});
		test('should report a user with spam category', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'spam_account',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
			expect(result.status).toBe('pending');
		});
		test('should report a user with guild context', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const result = await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
					guild_id: guild.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should reject a user report naming an unknown guild', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await createBuilder(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
					guild_id: TEST_IDS.NONEXISTENT_GUILD,
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
				.execute();
		});
		test('should reject report with invalid category', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await createBuilder(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId.toString(),
					category: 'invalid_category',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject report without authentication', async () => {
			const targetUser = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId.toString(),
					category: 'harassment',
				})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('should report user with impersonation category', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'impersonation',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('admin report detail includes mutual DM channel when present', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
			await createFriendship(harness, reporter, targetUser);
			const mutualDm = await createDmChannel(harness, reporter.token, targetUser.userId);
			const report = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const reportDetail = await createBuilder<{
				report_id: string;
				mutual_dm_channel_id?: string | null;
			}>(harness, `${admin.token}`)
				.get(`/admin/reports/${report.report_id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(reportDetail.report_id).toBe(report.report_id);
			expect(reportDetail.mutual_dm_channel_id).toBe(mutualDm.id);
		});
		test('sends a localized system DM and email marker when a report is reviewed with a public comment', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			let admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, ['admin:authenticate', 'report:resolve']);
			await createBuilder<void>(harness, reporter.token)
				.patch('/users/@me/settings')
				.body({locale: 'fr'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			await clearTestEmails(harness);
			const publicComment = 'Nous avons examiné votre signalement et pris des mesures.';
			await createBuilder<{
				report_id: string;
				status: string;
				resolved_at: string | null;
				public_comment: string | null;
			}>(harness, `${admin.token}`)
				.patch(`/admin/reports/${report.report_id}`)
				.body({
					status: 'resolved',
					public_comment: publicComment,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const sentEmails = await listTestEmails(harness, {recipient: reporter.email});
			const email = findLastTestEmail(sentEmails, 'report_resolved');
			expect(email).not.toBeNull();
			expect(email?.metadata['report_id']).toBe(report.report_id);
			expect(email?.metadata['public_comment']).toBe(publicComment);
			const systemMessages = await listSystemDmMessages(harness, reporter.token);
			expect(systemMessages).toHaveLength(1);
			const template = getEmailTemplate('report_resolved', 'fr', {
				username: reporter.username!,
				reportId: report.report_id,
				publicComment,
				hasComment: 'yes',
			});
			expect(template.ok).toBe(true);
			if (!template.ok) {
				throw new Error('Failed to resolve expected report_resolved email template');
			}
			expect(systemMessages[0]?.content).toBe(template.value.body);
			const systemDmChannel = (await listSystemDmChannels(harness, reporter.token))[0];
			expect(systemDmChannel?.id).toBeTruthy();
			const readStateRepository = new ReadStateRepository();
			await expect
				.poll(async () => {
					const readStates = await readStateRepository.listReadStates(createUserID(BigInt(reporter.userId)));
					const readState = readStates.find((state) => state.channelId.toString() === systemDmChannel?.id);
					return readState?.mentionCount ?? null;
				})
				.toBe(1);
		});
	});
	describe('Report Message', () => {
		test('should report a message with valid category', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Offensive content');
			const result = await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
			expect(result.status).toBe('pending');
		});
		test('should report message with spam category', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Buy now! Click link!');
			const result = await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'spam',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should report message with hate_speech category', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Test message');
			const result = await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'hate_speech',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should report message with illegal_activity category', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Test message');
			const result = await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'illegal_activity',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should reject message report without authentication', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Test message');
			await createBuilderWithoutAuth(harness)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('rejects a non-member reporting a guild message without creating a report', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const outsider = await createTestAccount(harness);
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Guild-only message');
			await createBuilder(harness, outsider.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
			expect(await countReports()).toBe(0);
		});
		test('rejects mismatched channel and message IDs without creating a report', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const sourceChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const otherChannel = await createChannel(harness, owner.token, guild.id, 'other-report-channel');
			const message = await sendChannelMessage(harness, targetUser.token, sourceChannel.id, 'Wrong channel target');
			await createBuilder(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: otherChannel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			expect(await countReports()).toBe(0);
		});
		test('rejects reports for channels the reporter cannot view without creating a report', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const reporter = members[0];
			const privateChannel = await createChannel(harness, owner.token, guild.id, 'private-report-channel');
			const message = await sendChannelMessage(harness, owner.token, privateChannel.id, 'Hidden channel message');
			await createPermissionOverwrite(harness, owner.token, privateChannel.id, reporter.userId, {
				type: 1,
				allow: '0',
				deny: Permissions.VIEW_CHANNEL.toString(),
			});
			await createBuilder(harness, reporter.token)
				.post('/reports/message')
				.body({
					channel_id: privateChannel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			expect(await countReports()).toBe(0);
		});
		test('rejects reports for deleted messages without creating a report', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Deleted report target');
			await createBuilder<void>(harness, owner.token)
				.delete(`/channels/${channel.id}/messages/${message.id}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			expect(await countReports()).toBe(0);
		});
		test('rate limits repeated message reports in the same channel', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const messages = await Promise.all(
				[0, 1, 2, 3].map((index) =>
					sendChannelMessage(harness, targetUser.token, channel.id, `Report rate limit target ${index}`),
				),
			);
			for (const message of messages.slice(0, 3)) {
				await createBuilder<ReportResponse>(harness, owner.token)
					.post('/reports/message')
					.body({
						channel_id: channel.id,
						message_id: message.id,
						category: 'harassment',
					})
					.expect(HTTP_STATUS.OK)
					.execute();
			}
			await createBuilder(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: messages[3].id,
					category: 'harassment',
				})
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
			expect(await countReports()).toBe(3);
		});
		test('a duplicate message report does not spend the reporter allowance', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const [firstMessage, secondMessage] = await Promise.all([
				sendChannelMessage(harness, targetUser.token, channel.id, 'Duplicate allowance target'),
				sendChannelMessage(harness, targetUser.token, channel.id, 'Duplicate allowance second target'),
			]);
			await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: firstMessage.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			for (let attempt = 0; attempt < 5; attempt += 1) {
				await createBuilder(harness, owner.token)
					.post('/reports/message')
					.body({
						channel_id: channel.id,
						message_id: firstMessage.id,
						category: 'spam',
					})
					.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
					.execute();
			}
			await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: secondMessage.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await countReports()).toBe(2);
		});
	});
	describe('Report Guild', () => {
		async function setupMemberAndGuild(): Promise<{
			reporter: TestAccount;
			owner: TestAccount;
			guild: GuildResponse;
		}> {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Problematic Guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, reporter.token, invite.code);
			return {reporter, owner, guild};
		}
		test('should report a guild with valid category', async () => {
			const {reporter, guild} = await setupMemberAndGuild();
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
			expect(result.status).toBe('pending');
		});
		test('should report guild with extremist_community category', async () => {
			const {reporter, guild} = await setupMemberAndGuild();
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'extremist_community',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should report guild with raid_coordination category', async () => {
			const {reporter, guild} = await setupMemberAndGuild();
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'raid_coordination',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should report guild with malware_distribution category', async () => {
			const {reporter, guild} = await setupMemberAndGuild();
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'malware_distribution',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should reject non-member reporting a non-discoverable guild without invite code', async () => {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			await createBuilder(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should allow non-member to report a non-discoverable guild with a valid invite code', async () => {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			const result = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'harassment',
					invite_code: invite.code,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should reject when invite_code resolves to a different guild', async () => {
			const reporter = await createTestAccount(harness);
			const ownerA = await createTestAccount(harness);
			const ownerB = await createTestAccount(harness);
			const targetGuild = await createGuild(harness, ownerA.token, 'Target Guild');
			const otherGuild = await createGuild(harness, ownerB.token, 'Other Guild');
			const wrongInvite = await createChannelInvite(harness, ownerB.token, otherGuild.system_channel_id!);
			await createBuilder(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: targetGuild.id,
					category: 'harassment',
					invite_code: wrongInvite.code,
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should reject guild report with invalid category', async () => {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			await createBuilder(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'invalid_category',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject guild report without authentication', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			await createBuilderWithoutAuth(harness)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('Report Requires Category', () => {
		test('should reject user report without category', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await createBuilder(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId.toString(),
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject message report without category', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			await ensureSessionStarted(harness, targetUser.token);
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Test message');
			await createBuilder(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject guild report without category', async () => {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			await createBuilder(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('Duplicate Reports', () => {
		test('should allow user to report same user multiple times', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			const firstReport = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(firstReport.report_id).toBeTruthy();
			const secondReport = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'spam_account',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(secondReport.report_id).toBeTruthy();
			expect(secondReport.report_id).not.toBe(firstReport.report_id);
		});
		test('should reject duplicate reports for the same message by the same user', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const targetUser = members[0];
			await ensureSessionStarted(harness, targetUser.token);
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Problematic message');
			const firstReport = await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(firstReport.report_id).toBeTruthy();
			await createBuilder(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'spam',
				})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			expect(await countReports()).toBe(1);
		});
		test('should allow different users to report the same message', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const targetUser = members[0];
			const secondReporter = members[1];
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, targetUser.token, channel.id, 'Problematic shared target');
			const firstReport = await createBuilder<ReportResponse>(harness, owner.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const secondReport = await createBuilder<ReportResponse>(harness, secondReporter.token)
				.post('/reports/message')
				.body({
					channel_id: channel.id,
					message_id: message.id,
					category: 'spam',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(secondReport.report_id).not.toBe(firstReport.report_id);
		});
		test('should allow user to report same guild multiple times', async () => {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Problematic Guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, reporter.token, invite.code);
			const firstReport = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(firstReport.report_id).toBeTruthy();
			const secondReport = await createBuilder<ReportResponse>(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'extremist_community',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(secondReport.report_id).toBeTruthy();
			expect(secondReport.report_id).not.toBe(firstReport.report_id);
		});
		test('should allow different users to report same content', async () => {
			const reporter1 = await createTestAccount(harness);
			const reporter2 = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			const report1 = await createBuilder<ReportResponse>(harness, reporter1.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const report2 = await createBuilder<ReportResponse>(harness, reporter2.token)
				.post('/reports/user')
				.body({
					user_id: targetUser.userId,
					category: 'harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(report1.report_id).toBeTruthy();
			expect(report2.report_id).toBeTruthy();
			expect(report1.report_id).not.toBe(report2.report_id);
		});
	});
	describe('DSA Report Flow', () => {
		test('should send DSA verification email', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa/email/send')
				.body({email})
				.expect(HTTP_STATUS.OK)
				.execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			expect(dsaEmail).toBeTruthy();
			expect(dsaEmail!.to).toBe(email.toLowerCase());
			expect(dsaEmail!.metadata.code).toBeTruthy();
		});
		test('should verify DSA email and return ticket', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			expect(dsaEmail).toBeTruthy();
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilder<{
				ticket: string;
			}>(harness, '')
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(verifyResponse.ticket).toBeTruthy();
			expect(verifyResponse.ticket.length).toBeGreaterThan(0);
		});
		test('should reject DSA email verification with invalid code', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa/email/verify')
				.body({email, code: 'XXXX-XXXX'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should create DSA user report with valid ticket', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			const targetUser = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilder<{
				ticket: string;
			}>(harness, '')
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<ReportResponse>(harness, '')
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'user',
					category: 'harassment',
					user_id: targetUser.userId,
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'DE',
					additional_info: 'DSA report for harassment',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
			expect(result.status).toBe('pending');
		});
		test('a DSA notice that fails target resolution keeps its ticket', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			const targetUser = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilder<{
				ticket: string;
			}>(harness, '')
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'user',
					category: 'harassment',
					user_id: TEST_IDS.NONEXISTENT_USER,
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'DE',
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_USER)
				.execute();
			const result = await createBuilder<ReportResponse>(harness, '')
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'user',
					category: 'harassment',
					user_id: targetUser.userId,
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'DE',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
		});
		test('should create DSA guild report with valid ticket', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'DSA Test Guild');
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilder<{
				ticket: string;
			}>(harness, '')
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<ReportResponse>(harness, '')
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'guild',
					category: 'illegal_activity',
					guild_id: guild.id,
					reporter_full_legal_name: 'Jane Doe',
					reporter_country_of_residence: 'FR',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.report_id).toBeTruthy();
			expect(result.status).toBe('pending');
		});
		test('should reject DSA message report with a non-numeric message link segment', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilder<{
				ticket: string;
			}>(harness, '')
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'message',
					category: 'harassment',
					message_link: 'https://fluxer.test/channels/1/abc/def',
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'DE',
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
		});
		test('should reject DSA message report with an out-of-range message link segment', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilder<{
				ticket: string;
			}>(harness, '')
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'message',
					category: 'harassment',
					message_link: 'https://fluxer.test/channels/1/99999999999999999999/1',
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'DE',
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
		});
		test('should reject DSA report with invalid ticket', async () => {
			const targetUser = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa')
				.body({
					ticket: 'invalid-ticket-value',
					report_type: 'user',
					category: 'harassment',
					user_id: targetUser.userId.toString(),
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'DE',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject DSA report with malformed ticket', async () => {
			const targetUser = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa')
				.body({
					ticket: '',
					report_type: 'user',
					category: 'harassment',
					user_id: targetUser.userId,
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'DE',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should require reporter_full_legal_name for DSA report', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			const targetUser = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilderWithoutAuth<{
				ticket: string;
			}>(harness)
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'user',
					category: 'harassment',
					user_id: targetUser.userId.toString(),
					reporter_country_of_residence: 'DE',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should require EU country for DSA report', async () => {
			await clearTestEmails(harness);
			const email = createUniqueEmail('dsa-reporter');
			const targetUser = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
			const emails = await listTestEmails(harness);
			const dsaEmail = findLastTestEmail(emails, 'dsa_report_verification');
			const code = dsaEmail!.metadata.code;
			const verifyResponse = await createBuilderWithoutAuth<{
				ticket: string;
			}>(harness)
				.post('/reports/dsa/email/verify')
				.body({email, code})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa')
				.body({
					ticket: verifyResponse.ticket,
					report_type: 'user',
					category: 'harassment',
					user_id: targetUser.userId.toString(),
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'US',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
});
