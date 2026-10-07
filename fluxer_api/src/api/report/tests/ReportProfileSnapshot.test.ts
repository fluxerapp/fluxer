// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createReportID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {loadFixture} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	createChannel,
	sendChannelMessage,
	setupTestGuildWithMembers,
	updateGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {GuildMemberRepository} from '@app/api/guild/repositories/GuildMemberRepository';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {grantPremium} from '@app/api/user/tests/UserTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {listReportProfileSnapshotAssets} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface ReportResponse {
	report_id: string;
}

interface ProfileResponse {
	id: string;
	username: string;
	discriminator: string;
	global_name: string | null;
	avatar: string | null;
	bio: string | null;
	pronouns: string | null;
}

const PREMIUM_TYPE_SUBSCRIPTION = 2;
const FIRST_IMAGE = getPngDataUrl();
const SECOND_IMAGE = `data:image/png;base64,${loadFixture('yeah.png').toString('base64')}`;

async function readReport(reportId: string) {
	const report = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
	if (!report) {
		throw new Error(`Report ${reportId} was not stored`);
	}
	return report;
}

async function readSnapshot(reportId: string) {
	const snapshot = (await readReport(reportId)).reportedProfileSnapshot;
	if (!snapshot) {
		throw new Error(`Report ${reportId} has no profile snapshot`);
	}
	return snapshot;
}

async function countReports(): Promise<number> {
	return (await new ReportRepository().listAllReportsPaginated(100)).length;
}

function updateProfile(harness: ApiTestHarness, token: string, body: Record<string, unknown>) {
	return createBuilder<ProfileResponse>(harness, token).patch('/users/@me').body(body).expect(HTTP_STATUS.OK).execute();
}

function updateMemberProfile(harness: ApiTestHarness, token: string, guildId: string, body: Record<string, unknown>) {
	return createBuilder<GuildMemberResponse>(harness, token)
		.patch(`/guilds/${guildId}/members/@me`)
		.body(body)
		.expect(HTTP_STATUS.OK)
		.execute();
}

function reportUser(harness: ApiTestHarness, token: string, userId: string, guildId?: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/user')
		.body({user_id: userId, category: 'inappropriate_profile', ...(guildId ? {guild_id: guildId} : {})});
}

function reportGuild(harness: ApiTestHarness, token: string, guildId: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/guild')
		.body({guild_id: guildId, category: 'harassment'});
}

async function issueTicket(harness: ApiTestHarness): Promise<string> {
	await clearTestEmails(harness);
	const email = createUniqueEmail('dsa-profile');
	await createBuilderWithoutAuth(harness)
		.post('/reports/dsa/email/send')
		.body({email})
		.expect(HTTP_STATUS.OK)
		.execute();
	const code = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
	const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
		.post('/reports/dsa/email/verify')
		.body({email, code})
		.expect(HTTP_STATUS.OK)
		.execute();
	return ticket;
}

function submitDsa(harness: ApiTestHarness, ticket: string, target: Record<string, unknown>) {
	return createBuilderWithoutAuth<ReportResponse>(harness)
		.post('/reports/dsa')
		.body({
			ticket,
			category: 'harassment',
			reporter_full_legal_name: 'Jane Doe',
			reporter_country_of_residence: 'DE',
			...target,
		});
}

async function setupProfile(
	harness: ApiTestHarness,
): Promise<{owner: TestAccount; target: TestAccount; guild: GuildResponse; profile: ProfileResponse}> {
	const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
	const target = members[0];
	await ensureSessionStarted(harness, target.token);
	await grantPremium(harness, target.userId, PREMIUM_TYPE_SUBSCRIPTION);
	const profile = await updateProfile(harness, target.token, {
		avatar: FIRST_IMAGE,
		bio: 'Original bio',
		pronouns: 'they/them',
		global_name: 'Original Name',
	});
	await updateMemberProfile(harness, target.token, guild.id, {
		nick: 'Original Nick',
		bio: 'Original guild bio',
		avatar: FIRST_IMAGE,
	});
	return {owner, target, guild, profile};
}

function reportsBucketHas(harness: ApiTestHarness, key: string | null | undefined): boolean {
	return key != null && harness.storageService.hasObject(Config.s3.buckets.reports, key);
}

describe('Report profile snapshots', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('a user report with guild context keeps the profile as it was when reported', async () => {
		const {owner, target, guild, profile} = await setupProfile(harness);
		const member = await new GuildMemberRepository().getMember(
			createGuildID(BigInt(guild.id)),
			createUserID(BigInt(target.userId)),
		);
		expect(profile.avatar).toBeTruthy();
		expect(member?.nickname).toBe('Original Nick');
		expect(member?.avatarHash).toBeTruthy();
		const result = await reportUser(harness, owner.token, target.userId, guild.id).expect(HTTP_STATUS.OK).execute();
		const expected = {
			captured_at: expect.any(String),
			user: {
				id: target.userId,
				username: profile.username,
				discriminator: Number(profile.discriminator),
				global_name: 'Original Name',
				bio: 'Original bio',
				pronouns: 'they/them',
				avatar: {
					hash: profile.avatar,
					key: `reports/${result.report_id}/profile/user_avatar/${profile.avatar}`,
				},
				banner: null,
			},
			member: {
				guild_id: guild.id,
				nick: 'Original Nick',
				bio: member!.bio,
				pronouns: member!.pronouns,
				joined_at: member!.joinedAt.toISOString(),
				avatar: {
					hash: member!.avatarHash,
					key: `reports/${result.report_id}/profile/member_avatar/${member!.avatarHash}`,
				},
				banner: null,
			},
			guild: null,
		};
		const stored = await readSnapshot(result.report_id);
		expect(stored).toEqual(expected);
		expect(Number.isNaN(Date.parse(stored.captured_at))).toBe(false);
		const userAvatarSource = `avatars/${target.userId}/${profile.avatar}`;
		const memberAvatarSource = `guilds/${guild.id}/users/${target.userId}/avatars/${member!.avatarHash}`;
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, userAvatarSource)).toBe(true);
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, memberAvatarSource)).toBe(true);
		expect(reportsBucketHas(harness, stored.user?.avatar?.key)).toBe(true);
		expect(reportsBucketHas(harness, stored.member?.avatar?.key)).toBe(true);
		const changed = await updateProfile(harness, target.token, {
			avatar: SECOND_IMAGE,
			bio: 'Changed bio',
			global_name: 'Changed Name',
		});
		expect(changed.avatar).not.toBe(profile.avatar);
		await updateMemberProfile(harness, target.token, guild.id, {nick: 'Changed Nick', avatar: SECOND_IMAGE});
		await harness.storageService.deleteObject(Config.s3.buckets.cdn, userAvatarSource);
		await harness.storageService.deleteObject(Config.s3.buckets.cdn, memberAvatarSource);
		const after = await readSnapshot(result.report_id);
		expect(after).toEqual(stored);
		expect(reportsBucketHas(harness, after.user?.avatar?.key)).toBe(true);
		expect(reportsBucketHas(harness, after.member?.avatar?.key)).toBe(true);
	});

	test('a user report without guild context keeps no member profile', async () => {
		const {target, profile} = await setupProfile(harness);
		const reporter = await createTestAccount(harness);
		const result = await reportUser(harness, reporter.token, target.userId).expect(HTTP_STATUS.OK).execute();
		const stored = await readSnapshot(result.report_id);
		expect(stored.user?.bio).toBe('Original bio');
		expect(stored.user?.avatar?.hash).toBe(profile.avatar);
		expect(stored.member).toBeNull();
		expect(stored.guild).toBeNull();
		expect(listReportProfileSnapshotAssets(stored)).toHaveLength(1);
	});

	test('a guild report keeps the guild name and icon as they were when reported', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const named = await updateGuild(harness, owner.token, guild.id, {name: 'Original Guild', icon: FIRST_IMAGE});
		expect(named.icon).toBeTruthy();
		const result = await reportGuild(harness, members[0].token, guild.id).expect(HTTP_STATUS.OK).execute();
		const stored = await readSnapshot(result.report_id);
		expect(stored).toEqual({
			captured_at: expect.any(String),
			user: null,
			member: null,
			guild: {
				id: guild.id,
				name: 'Original Guild',
				vanity_url_code: null,
				icon: {hash: named.icon, key: `reports/${result.report_id}/profile/guild_icon/${named.icon}`},
				banner: null,
				splash: null,
			},
		});
		expect(reportsBucketHas(harness, stored.guild?.icon?.key)).toBe(true);
		const renamed = await updateGuild(harness, owner.token, guild.id, {name: 'Renamed Guild', icon: SECOND_IMAGE});
		expect(renamed.icon).not.toBe(named.icon);
		await harness.storageService.deleteObject(Config.s3.buckets.cdn, `icons/${guild.id}/${named.icon}`);
		expect(await readSnapshot(result.report_id)).toEqual(stored);
		expect(reportsBucketHas(harness, stored.guild?.icon?.key)).toBe(true);
	});

	test('a message report keeps the profile of the author in that guild', async () => {
		const {owner, target, guild, profile} = await setupProfile(harness);
		const channel = await createChannel(harness, owner.token, guild.id, 'profile-evidence');
		const message = await sendChannelMessage(harness, target.token, channel.id, 'Reported message');
		const result = await createBuilder<ReportResponse>(harness, owner.token)
			.post('/reports/message')
			.body({channel_id: channel.id, message_id: message.id, category: 'harassment'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const stored = await readSnapshot(result.report_id);
		expect(stored.user).toMatchObject({id: target.userId, bio: 'Original bio', avatar: {hash: profile.avatar}});
		expect(stored.member).toMatchObject({guild_id: guild.id, nick: 'Original Nick'});
		expect(stored.guild).toBeNull();
		for (const asset of listReportProfileSnapshotAssets(stored)) {
			expect(asset.key?.startsWith(`reports/${result.report_id}/profile/`)).toBe(true);
			expect(reportsBucketHas(harness, asset.key)).toBe(true);
		}
		expect(listReportProfileSnapshotAssets(stored)).toHaveLength(2);
	});

	test('a closed account is kept by id with no profile text or images', async () => {
		const {owner, target, guild} = await setupProfile(harness);
		const user = await new UserRepository().findUnique(createUserID(BigInt(target.userId)));
		await createBuilder(harness, '')
			.patch(`/test/users/${target.userId}/flags`)
			.body({flags: (user!.flags | UserFlags.DELETED).toString()})
			.execute();
		const copiesBefore = harness.storageService.getCopiedObjects().length;
		const result = await reportUser(harness, owner.token, target.userId, guild.id).expect(HTTP_STATUS.OK).execute();
		expect(await readSnapshot(result.report_id)).toEqual({
			captured_at: expect.any(String),
			user: {
				id: target.userId,
				username: null,
				discriminator: null,
				global_name: null,
				bio: null,
				pronouns: null,
				avatar: null,
				banner: null,
			},
			member: null,
			guild: null,
		});
		expect(harness.storageService.getCopiedObjects()).toHaveLength(copiesBefore);
	});

	test('an image that cannot be copied is kept by hash and the report is filed', async () => {
		const {owner, target, guild, profile} = await setupProfile(harness);
		harness.storageService.configure({shouldFailCopy: true});
		const result = await reportUser(harness, owner.token, target.userId, guild.id).expect(HTTP_STATUS.OK).execute();
		harness.storageService.configure({shouldFailCopy: false});
		const stored = await readSnapshot(result.report_id);
		expect(stored.user?.avatar).toEqual({hash: profile.avatar, key: null});
		expect(stored.member?.avatar?.hash).toBeTruthy();
		expect(stored.member?.avatar?.key).toBeNull();
		expect(stored.user?.bio).toBe('Original bio');
		expect(stored.member?.nick).toBe('Original Nick');
		expect(
			harness.storageService.hasObject(
				Config.s3.buckets.reports,
				`reports/${result.report_id}/profile/user_avatar/${profile.avatar}`,
			),
		).toBe(false);
	});

	test('a refused report copies no images', async () => {
		const {owner, target, guild} = await setupProfile(harness);
		await reportUser(harness, owner.token, target.userId, guild.id).expect(HTTP_STATUS.OK).execute();
		await reportGuild(harness, target.token, guild.id).expect(HTTP_STATUS.OK).execute();
		const copiesBefore = harness.storageService.getCopiedObjects().length;
		await reportUser(harness, owner.token, target.userId, guild.id)
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
			.execute();
		await reportGuild(harness, target.token, guild.id).expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT).execute();
		const outsider = await createTestAccount(harness);
		await reportUser(harness, outsider.token, target.userId, guild.id)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
			.execute();
		expect(harness.storageService.getCopiedObjects()).toHaveLength(copiesBefore);
		expect(await countReports()).toBe(2);
	});

	test('a notice whose ticket is claimed elsewhere copies no images', async () => {
		const {target} = await setupProfile(harness);
		const ticket = await issueTicket(harness);
		const copiesBefore = harness.storageService.getCopiedObjects().length;
		const claim = vi.spyOn(ReportRepository.prototype, 'consumeDsaTicket').mockResolvedValueOnce(false);
		await submitDsa(harness, ticket, {report_type: 'user', user_id: target.userId})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_TICKET)
			.execute();
		expect(claim).toHaveBeenCalledTimes(1);
		claim.mockRestore();
		expect(harness.storageService.getCopiedObjects()).toHaveLength(copiesBefore);
		expect(await countReports()).toBe(0);
	});

	test('notices filed through the DSA form keep the same snapshots', async () => {
		const {owner, target, guild, profile} = await setupProfile(harness);
		const named = await updateGuild(harness, owner.token, guild.id, {name: 'Original Guild', icon: FIRST_IMAGE});
		const channel = await createChannel(harness, owner.token, guild.id, 'profile-evidence');
		const message = await sendChannelMessage(harness, target.token, channel.id, 'Reported message');
		const userNotice = await submitDsa(harness, await issueTicket(harness), {
			report_type: 'user',
			user_id: target.userId,
		})
			.expect(HTTP_STATUS.OK)
			.execute();
		const userSnapshot = await readSnapshot(userNotice.report_id);
		expect(userSnapshot.user).toMatchObject({
			id: target.userId,
			username: profile.username,
			bio: 'Original bio',
			avatar: {hash: profile.avatar, key: `reports/${userNotice.report_id}/profile/user_avatar/${profile.avatar}`},
		});
		expect(userSnapshot.member).toBeNull();
		expect(userSnapshot.guild).toBeNull();
		expect(reportsBucketHas(harness, userSnapshot.user?.avatar?.key)).toBe(true);
		const guildNotice = await submitDsa(harness, await issueTicket(harness), {
			report_type: 'guild',
			guild_id: guild.id,
		})
			.expect(HTTP_STATUS.OK)
			.execute();
		const guildSnapshot = await readSnapshot(guildNotice.report_id);
		expect(guildSnapshot.user).toBeNull();
		expect(guildSnapshot.guild).toMatchObject({
			id: guild.id,
			name: 'Original Guild',
			icon: {hash: named.icon, key: `reports/${guildNotice.report_id}/profile/guild_icon/${named.icon}`},
		});
		expect(reportsBucketHas(harness, guildSnapshot.guild?.icon?.key)).toBe(true);
		const messageNotice = await submitDsa(harness, await issueTicket(harness), {
			report_type: 'message',
			message_link: `https://web.fluxer.app/channels/${guild.id}/${channel.id}/${message.id}`,
		})
			.expect(HTTP_STATUS.OK)
			.execute();
		const messageSnapshot = await readSnapshot(messageNotice.report_id);
		expect(messageSnapshot.user).toMatchObject({id: target.userId, bio: 'Original bio'});
		expect(messageSnapshot.member).toMatchObject({guild_id: guild.id, nick: 'Original Nick'});
		expect(messageSnapshot.guild).toBeNull();
	});
});
