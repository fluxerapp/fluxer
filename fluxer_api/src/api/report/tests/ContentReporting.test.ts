// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createReportID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	createPermissionOverwrite,
	getChannel,
	leaveGuild,
	sendChannelMessage,
	setupTestGuildWithMembers,
	updateGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {deleteOneOrMany} from '@app/api/database/CassandraQueryExecution';
import {GuildMemberRepository} from '@app/api/guild/repositories/GuildMemberRepository';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {getRateLimitService} from '@app/api/middleware/ServiceSingletons';
import {ReadStateRepository} from '@app/api/read_state/ReadStateRepository';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {Users} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DELETED_USER_USERNAME} from '@fluxer/constants/src/UserConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

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

async function readReport(reportId: string) {
	const report = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
	if (!report) {
		throw new Error(`Report ${reportId} was not stored`);
	}
	return report;
}

function reportMessage(harness: ApiTestHarness, token: string, channelId: string, messageId: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/message')
		.body({channel_id: channelId, message_id: messageId, category: 'harassment'});
}

function reportUser(harness: ApiTestHarness, token: string, userId: string, guildId?: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/user')
		.body({user_id: userId, category: 'harassment', ...(guildId ? {guild_id: guildId} : {})});
}

function reportGuild(harness: ApiTestHarness, token: string, guildId: string, inviteCode?: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/guild')
		.body({guild_id: guildId, category: 'harassment', ...(inviteCode ? {invite_code: inviteCode} : {})});
}

function embedStorageKey(url: string | null | undefined): string {
	const unsigned = (url ?? '').split('?')[0]!;
	expect(unsigned.startsWith(`${Config.endpoints.media}/attachments/`)).toBe(true);
	return unsigned.slice(`${Config.endpoints.media}/`.length);
}

function refuseNextAllowanceSpend() {
	return vi.spyOn(getRateLimitService(), 'checkLimit').mockResolvedValueOnce({
		allowed: false,
		limit: 5,
		remaining: 0,
		resetTime: new Date(Date.now() + 1000),
		resetAfterDecimal: 1,
		retryAfter: 1,
		retryAfterDecimal: 1,
	});
}

function contextIds(report: Awaited<ReturnType<typeof readReport>>): Array<string> {
	return (report.messageContext ?? [])
		.map((entry) => entry.messageId)
		.sort((a, b) => (a < b ? -1 : a > b ? 1 : 0))
		.map((id) => id.toString());
}

async function setJoinedAt(guildId: string, userId: string, joinedAt: Date): Promise<void> {
	const repository = new GuildMemberRepository();
	const member = await repository.getMember(createGuildID(BigInt(guildId)), createUserID(BigInt(userId)));
	if (!member) {
		throw new Error(`Member ${userId} of guild ${guildId} was not stored`);
	}
	await repository.upsertMember({...member.toRow(), joined_at: joinedAt});
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
		test('should answer a guild the reporter has not joined like an unknown guild', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [targetUser, formerMember] = members;
			const outsider = await createTestAccount(harness);
			const unknown = await reportUser(harness, outsider.token, targetUser.userId, TEST_IDS.NONEXISTENT_GUILD)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
				.executeRaw();
			const notJoined = await reportUser(harness, outsider.token, targetUser.userId, guild.id)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
				.executeRaw();
			expect(notJoined.text).toBe(unknown.text);
			await leaveGuild(harness, formerMember.token, guild.id);
			const left = await reportUser(harness, formerMember.token, targetUser.userId, guild.id)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
				.executeRaw();
			expect(left.text).toBe(unknown.text);
			expect(await countReports()).toBe(0);
			await reportUser(harness, outsider.token, targetUser.userId).expect(HTTP_STATUS.OK).execute();
			const member = await reportUser(harness, owner.token, targetUser.userId, guild.id)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await readReport(member.report_id)).reportedGuildId?.toString()).toBe(guild.id);
			expect(await countReports()).toBe(2);
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
			const template = getEmailTemplate(
				'report_resolved',
				'fr',
				{
					username: reporter.username!,
					reportId: report.report_id,
					publicComment,
					hasComment: 'yes',
					safety_email: resolveContactEmails().safetyEmail,
				},
				getInstanceProductName(),
			);
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
		describe('without Read Message History', () => {
			async function setupLateJoiner() {
				const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
				const [author, reporter] = members;
				const channelId = (await createChannel(harness, owner.token, guild.id, 'history-floor')).id;
				await createPermissionOverwrite(harness, owner.token, channelId, guild.id, {
					type: 0,
					allow: '0',
					deny: Permissions.READ_MESSAGE_HISTORY.toString(),
				});
				const earlier = await sendChannelMessage(harness, author.token, channelId, 'Sent before the reporter joined');
				const later = await sendChannelMessage(harness, author.token, channelId, 'Sent after the reporter joined');
				const latest = await sendChannelMessage(harness, author.token, channelId, 'Sent last');
				await setJoinedAt(guild.id, reporter.userId, new Date(extractTimestamp(later.id)));
				return {owner, reporter, guild, channelId, earlier, later, latest};
			}
			test('a message sent after the reporter joined is reportable and the context starts at the join', async () => {
				const {reporter, channelId, later, latest} = await setupLateJoiner();
				const result = await reportMessage(harness, reporter.token, channelId, later.id)
					.expect(HTTP_STATUS.OK)
					.execute();
				expect(contextIds(await readReport(result.report_id))).toEqual([later.id, latest.id]);
			});
			test('a message sent before the reporter joined is an unknown message', async () => {
				const {reporter, channelId, earlier} = await setupLateJoiner();
				await reportMessage(harness, reporter.token, channelId, earlier.id)
					.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
					.execute();
				expect(await countReports()).toBe(0);
			});
			test('a history cutoff earlier than the join makes older messages reportable', async () => {
				const {owner, reporter, guild, channelId, earlier, later, latest} = await setupLateJoiner();
				const updated = await updateGuild(harness, owner.token, guild.id, {
					message_history_cutoff: new Date(extractTimestamp(guild.id)).toISOString(),
				});
				expect(updated.message_history_cutoff).toBeTruthy();
				const result = await reportMessage(harness, reporter.token, channelId, earlier.id)
					.expect(HTTP_STATUS.OK)
					.execute();
				expect(contextIds(await readReport(result.report_id))).toEqual([earlier.id, later.id, latest.id]);
			});
			test('a history cutoff later than the join keeps messages since the join reportable', async () => {
				const {owner, reporter, guild, channelId, earlier, later, latest} = await setupLateJoiner();
				const updated = await updateGuild(harness, owner.token, guild.id, {
					message_history_cutoff: new Date(extractTimestamp(latest.id)).toISOString(),
				});
				expect(updated.message_history_cutoff).toBeTruthy();
				const result = await reportMessage(harness, reporter.token, channelId, later.id)
					.expect(HTTP_STATUS.OK)
					.execute();
				expect(contextIds(await readReport(result.report_id))).toEqual([later.id, latest.id]);
				await reportMessage(harness, reporter.token, channelId, earlier.id)
					.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
					.execute();
			});
			test('a channel the reporter cannot view stays forbidden', async () => {
				const {owner, reporter, channelId, later} = await setupLateJoiner();
				await createPermissionOverwrite(harness, owner.token, channelId, reporter.userId, {
					type: 1,
					allow: '0',
					deny: Permissions.VIEW_CHANNEL.toString(),
				});
				await reportMessage(harness, reporter.token, channelId, later.id)
					.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
					.execute();
				expect(await countReports()).toBe(0);
			});
		});
		describe('Evidence gaps', () => {
			test('an attachment that cannot be copied is recorded and the report is filed', async () => {
				const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
				const {json: message} = await sendMessageWithAttachments(
					harness,
					members[0].token,
					systemChannel.id,
					{content: 'Evidence', attachments: [{id: 0, filename: 'evidence.png'}]},
					[{index: 0, filename: 'evidence.png', data: loadFixture('yeah.png')}],
				);
				const attachment = message.attachments![0]!;
				harness.storageService.configure({shouldFailCopy: true});
				const result = await reportMessage(harness, owner.token, systemChannel.id, message.id)
					.expect(HTTP_STATUS.OK)
					.execute();
				harness.storageService.configure({shouldFailCopy: false});
				const report = await readReport(result.report_id);
				const reported = report.messageContext!.find((entry) => entry.messageId.toString() === message.id)!;
				expect(reported.attachments).toEqual([]);
				expect(reported.missingAttachments).toHaveLength(1);
				expect(reported.missingAttachments[0]).toMatchObject({
					attachment_id: BigInt(attachment.id),
					filename: 'evidence.png',
					size: BigInt(attachment.size),
					content_type: 'image/png',
				});
				const key = `attachments/${systemChannel.id}/${attachment.id}/evidence.png`;
				expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(true);
				expect(harness.storageService.hasObject(Config.s3.buckets.reports, key)).toBe(false);
			});
			test('an attachment that is copied has no gap recorded', async () => {
				const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
				const {json: message} = await sendMessageWithAttachments(
					harness,
					members[0].token,
					systemChannel.id,
					{content: 'Evidence', attachments: [{id: 0, filename: 'evidence.png'}]},
					[{index: 0, filename: 'evidence.png', data: loadFixture('yeah.png')}],
				);
				const result = await reportMessage(harness, owner.token, systemChannel.id, message.id)
					.expect(HTTP_STATUS.OK)
					.execute();
				const report = await readReport(result.report_id);
				const reported = report.messageContext!.find((entry) => entry.messageId.toString() === message.id)!;
				expect(reported.attachments.map((entry) => entry.filename)).toEqual(['evidence.png']);
				expect(reported.missingAttachments).toEqual([]);
				const key = `attachments/${systemChannel.id}/${message.attachments![0]!.id}/evidence.png`;
				expect(harness.storageService.hasObject(Config.s3.buckets.reports, key)).toBe(true);
			});
			test('embed files that are gone or cannot be copied are recorded', async () => {
				const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
				const channelId = (await createChannel(harness, owner.token, guild.id, 'embed-evidence')).id;
				const fileData = loadFixture('yeah.png');
				const {json: message} = await sendMessageWithAttachments(
					harness,
					owner.token,
					channelId,
					{
						content: 'Embed evidence',
						attachments: [
							{id: 0, filename: 'image.png'},
							{id: 1, filename: 'thumb.png'},
						],
						embeds: [
							{title: 'Embed', image: {url: 'attachment://image.png'}, thumbnail: {url: 'attachment://thumb.png'}},
						],
					},
					[
						{index: 0, filename: 'image.png', data: fileData},
						{index: 1, filename: 'thumb.png', data: fileData},
					],
				);
				const embed = message.embeds![0]!;
				const imageKey = embedStorageKey(embed.image?.url);
				const thumbKey = embedStorageKey(embed.thumbnail?.url);
				await harness.storageService.deleteObject(Config.s3.buckets.cdn, thumbKey);
				const first = await reportMessage(harness, members[0].token, channelId, message.id)
					.expect(HTTP_STATUS.OK)
					.execute();
				const firstEntry = (await readReport(first.report_id)).messageContext!.find(
					(entry) => entry.messageId.toString() === message.id,
				)!;
				expect(firstEntry.attachments.map((entry) => entry.filename)).toEqual(['image.png']);
				expect(firstEntry.attachments[0]?.size).toBe(BigInt(fileData.length));
				expect(firstEntry.missingAttachments.map((entry) => entry.filename)).toEqual(['thumb.png']);
				expect(firstEntry.missingAttachments[0]?.size).toBe(0n);
				expect(harness.storageService.hasObject(Config.s3.buckets.reports, imageKey)).toBe(true);
				expect(harness.storageService.hasObject(Config.s3.buckets.reports, thumbKey)).toBe(false);
				await harness.storageService.deleteObject(Config.s3.buckets.reports, imageKey);
				harness.storageService.configure({shouldFailCopy: true});
				const second = await reportMessage(harness, members[1].token, channelId, message.id)
					.expect(HTTP_STATUS.OK)
					.execute();
				harness.storageService.configure({shouldFailCopy: false});
				const secondEntry = (await readReport(second.report_id)).messageContext!.find(
					(entry) => entry.messageId.toString() === message.id,
				)!;
				expect(secondEntry.attachments).toEqual([]);
				expect(secondEntry.missingAttachments.map((entry) => entry.filename).sort()).toEqual([
					'image.png',
					'thumb.png',
				]);
				expect(harness.storageService.hasObject(Config.s3.buckets.reports, imageKey)).toBe(false);
			});
			test('a context message whose author has no account row is kept as a deleted user', async () => {
				const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
				const [targetUser, goneAuthor] = members;
				const channelId = (await createChannel(harness, owner.token, guild.id, 'evidence')).id;
				const before = await sendChannelMessage(harness, goneAuthor.token, channelId, 'Said before');
				const target = await sendChannelMessage(harness, targetUser.token, channelId, 'Reported');
				const after = await sendChannelMessage(harness, goneAuthor.token, channelId, 'Said after');
				await deleteOneOrMany(Users.deleteByPk({user_id: createUserID(BigInt(goneAuthor.userId))}));
				const result = await reportMessage(harness, owner.token, channelId, target.id).expect(HTTP_STATUS.OK).execute();
				const report = await readReport(result.report_id);
				expect(contextIds(report)).toEqual([before.id, target.id, after.id]);
				for (const id of [before.id, after.id]) {
					const entry = report.messageContext!.find((candidate) => candidate.messageId.toString() === id)!;
					expect(entry.authorId?.toString()).toBe(goneAuthor.userId);
					expect(entry.authorUsername).toBe(DELETED_USER_USERNAME);
					expect(entry.authorDiscriminator).toBe(0);
					expect(entry.authorAvatarHash).toBeNull();
					expect(entry.content).toBe(id === before.id ? 'Said before' : 'Said after');
				}
			});
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
		test('a guild report past the reporter allowance is rate limited before it is a conflict', async () => {
			const {reporter, guild} = await setupMemberAndGuild();
			await reportGuild(harness, reporter.token, guild.id).expect(HTTP_STATUS.OK).execute();
			const targets = await Promise.all([0, 1, 2, 3].map(() => createTestAccount(harness)));
			for (const target of targets) {
				await reportUser(harness, reporter.token, target.userId).expect(HTTP_STATUS.OK).execute();
			}
			await reportGuild(harness, reporter.token, guild.id).expect(429, APIErrorCodes.RATE_LIMITED).execute();
			await getRateLimitService().resetLimit(`report:create:user:${reporter.userId}`);
			await reportGuild(harness, reporter.token, guild.id)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			expect(await countReports()).toBe(5);
		});
	});
	describe('Duplicate Reports', () => {
		test('should reject a second report of the same guild by the same reporter', async () => {
			const reporter = await createTestAccount(harness);
			const otherReporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Problematic Guild');
			const otherGuild = await createGuild(harness, owner.token, 'Other Guild');
			for (const target of [guild, otherGuild]) {
				const invite = await createChannelInvite(harness, owner.token, target.system_channel_id!);
				await acceptInvite(harness, reporter.token, invite.code);
				await acceptInvite(harness, otherReporter.token, invite.code);
			}
			const firstReport = await reportGuild(harness, reporter.token, guild.id).expect(HTTP_STATUS.OK).execute();
			await createBuilder(harness, reporter.token)
				.post('/reports/guild')
				.body({
					guild_id: guild.id,
					category: 'extremist_community',
				})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			const otherGuildReport = await reportGuild(harness, reporter.token, otherGuild.id)
				.expect(HTTP_STATUS.OK)
				.execute();
			const otherReporterReport = await reportGuild(harness, otherReporter.token, guild.id)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(new Set([firstReport.report_id, otherGuildReport.report_id, otherReporterReport.report_id]).size).toBe(3);
			expect(await countReports()).toBe(3);
		});
		test('parallel guild reports by one reporter file one report', async () => {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Problematic Guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, reporter.token, invite.code);
			const results = await Promise.all(
				[0, 1, 2].map(() => reportGuild(harness, reporter.token, guild.id).executeRaw()),
			);
			expect(results.map(({response}) => response.status).sort()).toEqual([200, 409, 409]);
			expect(await countReports()).toBe(1);
		});
		test('a duplicate guild report does not spend the reporter allowance', async () => {
			const reporter = await createTestAccount(harness);
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Problematic Guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await reportGuild(harness, reporter.token, guild.id, invite.code).expect(HTTP_STATUS.OK).execute();
			for (let attempt = 0; attempt < 5; attempt += 1) {
				await reportGuild(harness, reporter.token, guild.id, invite.code)
					.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
					.execute();
			}
			const targets = await Promise.all([0, 1, 2, 3, 4].map(() => createTestAccount(harness)));
			for (const target of targets.slice(0, 4)) {
				await reportUser(harness, reporter.token, target.userId).expect(HTTP_STATUS.OK).execute();
			}
			await reportUser(harness, reporter.token, targets[4].userId).expect(429, APIErrorCodes.RATE_LIMITED).execute();
			expect(await countReports()).toBe(5);
		});
		test('reports filed by an account store the reporter id without an email address', async () => {
			const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
			const reporter = members[0];
			const message = await sendChannelMessage(harness, owner.token, systemChannel.id, 'Reported');
			const results = [
				await reportMessage(harness, reporter.token, systemChannel.id, message.id).expect(HTTP_STATUS.OK).execute(),
				await reportUser(harness, reporter.token, owner.userId).expect(HTTP_STATUS.OK).execute(),
				await reportGuild(harness, reporter.token, guild.id).expect(HTTP_STATUS.OK).execute(),
			];
			for (const result of results) {
				const report = await readReport(result.report_id);
				expect(report.reporterId?.toString()).toBe(reporter.userId);
				expect(report.reporterEmail).toBeNull();
			}
		});
	});
	describe('Reservations', () => {
		test('a user report refused when the allowance is spent keeps no reservation', async () => {
			const reporter = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			const spend = refuseNextAllowanceSpend();
			await reportUser(harness, reporter.token, targetUser.userId).expect(429, APIErrorCodes.RATE_LIMITED).execute();
			expect(spend).toHaveBeenCalledTimes(1);
			spend.mockRestore();
			expect(await countReports()).toBe(0);
			await reportUser(harness, reporter.token, targetUser.userId).expect(HTTP_STATUS.OK).execute();
			expect(await countReports()).toBe(1);
		});
		test('a guild report refused when the allowance is spent keeps no reservation', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const reporter = members[0];
			const spend = refuseNextAllowanceSpend();
			await reportGuild(harness, reporter.token, guild.id).expect(429, APIErrorCodes.RATE_LIMITED).execute();
			expect(spend).toHaveBeenCalledTimes(1);
			spend.mockRestore();
			expect(await countReports()).toBe(0);
			await reportGuild(harness, reporter.token, guild.id).expect(HTTP_STATUS.OK).execute();
			await reportGuild(harness, reporter.token, guild.id)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			expect(await countReports()).toBe(1);
		});
		test('a message report refused when the allowance is spent keeps no reservation', async () => {
			const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
			const message = await sendChannelMessage(harness, members[0].token, systemChannel.id, 'Reported');
			const spend = refuseNextAllowanceSpend();
			await reportMessage(harness, owner.token, systemChannel.id, message.id)
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
			expect(spend).toHaveBeenCalledTimes(1);
			spend.mockRestore();
			expect(await countReports()).toBe(0);
			await reportMessage(harness, owner.token, systemChannel.id, message.id).expect(HTTP_STATUS.OK).execute();
			expect(await countReports()).toBe(1);
		});
	});
	describe('DSA Report Flow', () => {
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
