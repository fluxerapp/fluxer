// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {acceptInvite, createChannelInvite, createGuild, getChannel} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	ProfileFieldPrivacyFlags,
	type ProfilePrivacyLevel,
	ProfilePrivacyLevels,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import {getCurrentTimeZoneOffsetMinutes} from '@fluxer/date_utils/src/TimeZoneUtils';
import type {UserPrivateResponse, UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const TEST_TIMEZONE = 'America/New_York';
const TEST_TIMEZONE_OFFSET = getCurrentTimeZoneOffsetMinutes(TEST_TIMEZONE);

if (TEST_TIMEZONE_OFFSET == null) {
	throw new Error(`Expected ${TEST_TIMEZONE} to have a current UTC offset`);
}

async function updateProfileTimezone(
	harness: ApiTestHarness,
	token: string,
	data: {timezone?: string | null; timezone_privacy_flags?: number},
): Promise<UserPrivateResponse> {
	return createBuilder<UserPrivateResponse>(harness, token).patch('/users/@me').body(data).execute();
}

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function updateProfilePrivacy(
	harness: ApiTestHarness,
	token: string,
	profilePrivacy: ProfilePrivacyLevel,
): Promise<void> {
	await createBuilder(harness, token).patch('/users/@me/settings').body({profile_privacy: profilePrivacy}).execute();
}

async function getUserProfile(
	harness: ApiTestHarness,
	token: string,
	userId: string,
): Promise<UserProfileFullResponse> {
	return createBuilder<UserProfileFullResponse>(harness, token).get(`/users/${userId}/profile`).execute();
}

async function createSharedGuild(harness: ApiTestHarness, ownerToken: string, memberToken: string): Promise<void> {
	const guild = await createGuild(harness, ownerToken, 'Timezone Test Guild');
	const systemChannel = await getChannel(harness, ownerToken, guild.system_channel_id!);
	const invite = await createChannelInvite(harness, ownerToken, systemChannel.id);
	await acceptInvite(harness, memberToken, invite.code);
}

describe('User Profile Timezone Visibility', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('defaults timezone visibility to everyone when a timezone is set', async () => {
		const targetAccount = await createTestAccount(harness);
		const viewerAccount = await createTestAccount(harness);
		await setUserFlags(harness, targetAccount.userId, UserFlags.STAFF);
		const updated = await updateProfileTimezone(harness, targetAccount.token, {timezone: TEST_TIMEZONE});
		expect(updated.timezone).toBe(TEST_TIMEZONE);
		expect(updated.timezone_privacy_flags).toBe(ProfileFieldPrivacyFlags.EVERYONE);
		await createFriendship(harness, targetAccount, viewerAccount);
		const profile = await getUserProfile(harness, viewerAccount.token, targetAccount.userId);
		expect(profile.timezone_offset).toBe(TEST_TIMEZONE_OFFSET);
	});
	it('restores default timezone visibility when a timezone is set again without explicit flags', async () => {
		const targetAccount = await createTestAccount(harness);
		await setUserFlags(harness, targetAccount.userId, UserFlags.STAFF);
		await updateProfileTimezone(harness, targetAccount.token, {
			timezone: TEST_TIMEZONE,
			timezone_privacy_flags: 0,
		});
		await updateProfileTimezone(harness, targetAccount.token, {timezone: null});
		const updated = await updateProfileTimezone(harness, targetAccount.token, {timezone: TEST_TIMEZONE});
		expect(updated.timezone_privacy_flags).toBe(ProfileFieldPrivacyFlags.EVERYONE);
	});
	it('hides timezone from the public profile when privacy flags are unset', async () => {
		const targetAccount = await createTestAccount(harness);
		await setUserFlags(harness, targetAccount.userId, UserFlags.STAFF);
		await updateProfileTimezone(harness, targetAccount.token, {
			timezone: TEST_TIMEZONE,
			timezone_privacy_flags: 0,
		});
		const profile = await getUserProfile(harness, targetAccount.token, targetAccount.userId);
		expect(profile.timezone_offset).toBeNull();
	});
	it('shows timezone to friends only when the friends flag is set', async () => {
		const targetAccount = await createTestAccount(harness);
		const friendAccount = await createTestAccount(harness);
		const guildMemberAccount = await createTestAccount(harness);
		await setUserFlags(harness, targetAccount.userId, UserFlags.STAFF);
		await updateProfileTimezone(harness, targetAccount.token, {
			timezone: TEST_TIMEZONE,
			timezone_privacy_flags: ProfileFieldPrivacyFlags.FRIENDS,
		});
		await createFriendship(harness, targetAccount, friendAccount);
		await createSharedGuild(harness, targetAccount.token, guildMemberAccount.token);
		const friendProfile = await getUserProfile(harness, friendAccount.token, targetAccount.userId);
		const guildMemberProfile = await getUserProfile(harness, guildMemberAccount.token, targetAccount.userId);
		expect(friendProfile.timezone_offset).toBe(TEST_TIMEZONE_OFFSET);
		expect(guildMemberProfile.timezone_offset).toBeNull();
	});
	it('shows timezone to mutual community members only when the mutual communities flag is set', async () => {
		const targetAccount = await createTestAccount(harness);
		const friendAccount = await createTestAccount(harness);
		const guildMemberAccount = await createTestAccount(harness);
		await setUserFlags(harness, targetAccount.userId, UserFlags.STAFF);
		await updateProfileTimezone(harness, targetAccount.token, {
			timezone: TEST_TIMEZONE,
			timezone_privacy_flags: ProfileFieldPrivacyFlags.MUTUAL_GUILDS,
		});
		await createFriendship(harness, targetAccount, friendAccount);
		await createSharedGuild(harness, targetAccount.token, guildMemberAccount.token);
		const friendProfile = await getUserProfile(harness, friendAccount.token, targetAccount.userId);
		const guildMemberProfile = await getUserProfile(harness, guildMemberAccount.token, targetAccount.userId);
		expect(friendProfile.timezone_offset).toBeNull();
		expect(guildMemberProfile.timezone_offset).toBe(TEST_TIMEZONE_OFFSET);
	});
	it('hides timezone when full profile privacy restricts the viewer', async () => {
		const targetAccount = await createTestAccount(harness);
		const guildMemberAccount = await createTestAccount(harness);
		await setUserFlags(harness, targetAccount.userId, UserFlags.STAFF);
		await updateProfileTimezone(harness, targetAccount.token, {timezone: TEST_TIMEZONE});
		await updateProfilePrivacy(harness, targetAccount.token, ProfilePrivacyLevels.FRIENDS_ONLY);
		await createSharedGuild(harness, targetAccount.token, guildMemberAccount.token);
		const profile = await getUserProfile(harness, guildMemberAccount.token, targetAccount.userId);
		expect(profile.profile_limited).toBe(true);
		expect(profile.timezone_offset).toBeNull();
	});
	it('ignores profile timezone updates from non-staff users', async () => {
		const targetAccount = await createTestAccount(harness, {skipEmailVerification: true});
		const updated = await updateProfileTimezone(harness, targetAccount.token, {timezone: TEST_TIMEZONE});
		expect(updated).not.toHaveProperty('timezone');
		expect(updated).not.toHaveProperty('timezone_privacy_flags');
	});
	it('hides stored profile timezone after the user no longer has the staff flag', async () => {
		const targetAccount = await createTestAccount(harness);
		const viewerAccount = await createTestAccount(harness);
		await setUserFlags(harness, targetAccount.userId, UserFlags.STAFF);
		await updateProfileTimezone(harness, targetAccount.token, {timezone: TEST_TIMEZONE});
		await setUserFlags(harness, targetAccount.userId, 0n);
		const currentUser = await createBuilder<UserPrivateResponse>(harness, targetAccount.token)
			.get('/users/@me')
			.execute();
		expect(currentUser).not.toHaveProperty('timezone');
		expect(currentUser).not.toHaveProperty('timezone_privacy_flags');
		await createFriendship(harness, targetAccount, viewerAccount);
		const profile = await getUserProfile(harness, viewerAccount.token, targetAccount.userId);
		expect(profile.timezone_offset).toBeNull();
	});
});
